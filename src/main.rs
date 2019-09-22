use failure;
use nix::unistd::Pid;
use rand;
use serde;
use std::collections::{HashMap, HashSet};
use std::fs::{File, Permissions};
use std::io::Read;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::os::unix::process::CommandExt;
use std::process::{Child, Command};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use toml;

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
struct TomlCommand {
    description: String,
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
}

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
struct TomlService {
    description: String,
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
    #[serde(default=[])]
    wants: Vec<String>,
}

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
struct TomlWatch {
    description: String,
    exec: Vec<String>,
    #[serde(default=[])]
    file_types: Vec<String>,
}

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
struct TomlConfig {
    environment: HashMap<String, String>,
    commands: HashMap<String, TomlCommand>,
    services: HashMap<String, TomlService>,
    watches: HashMap<String, TomlWatch>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct UnitName(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct UnitDescription(String);

#[derive(Debug, Clone)]
struct CommandName(String);

#[derive(Debug, Clone)]
enum ArgOrLookup {
    Arg(String),
    Lookup(String),
}

type CommandArgs = Vec<ArgOrLookup>;

#[derive(Debug, Clone)]
struct Unit {
    name: UnitName,
    description: UnitDescription,
    exec_spec: ExecSpec,
    wants: Vec<UnitName>,
    typ: UnitType,
}

#[derive(Debug, Clone)]
enum UnitType {
    Service,
    Command,
    Watch,
}

#[derive(Debug, Clone)]
enum ExecSpec {
    Exec(CommandName, CommandArgs),
    ExecScript(String),
}

#[derive(Debug, Clone)]
struct Config {
    units: HashMap<UnitName, Unit>,
    environment: HashMap<String, String>,
}

#[derive(Debug, failure::Fail)]
enum LapsError {
    #[fail(display = "Watches, services, commands can only have one of `exec`, `exec-script`")]
    BadExec(UnitName),
    #[fail(display = "Exec stanza cannot be empty")]
    ExecCantBeEmpty,
    #[fail(display = "Duplicate names somewhere")]
    Duplicate,
    #[fail(display = "Service has unknown dependencies")]
    UnknownDeps(UnitName, UnitName),
    #[fail(display = "Unknown targets specified")]
    UnknownTargets(HashSet<UnitName>),
    #[fail(display = "Bad lookup {}", _0)]
    BadLookup(String),
}

fn main() -> Result<(), failure::Error> {
    let toml_config: TomlConfig = read_toml_config()?;
    let validated_config: Config = validate_config(toml_config)?;

    let available_units: HashSet<UnitName> = validated_config.units.keys().cloned().collect();
    let user_specified_units: HashSet<UnitName> = std::env::args()
        .skip(1)
        .map(|a| UnitName(a.trim().to_string()))
        .collect();

    if user_specified_units.len() == 0 {
        let help_text = get_help_text(validated_config);
        print!("{}", help_text);
        return Ok(());
    }

    let unknown_targets: HashSet<UnitName> = user_specified_units
        .difference(&available_units)
        .cloned()
        .collect();
    failure::ensure!(
        unknown_targets.len() == 0,
        LapsError::UnknownTargets(unknown_targets)
    );

    let exec_plan: Plan = get_exec_plan(&user_specified_units, &validated_config)?;

    let running = Arc::new(AtomicBool::new(true));
    let r = running.clone();
    ctrlc::set_handler(move || {
        r.store(false, Ordering::SeqCst);
    })?;

    for step in exec_plan {
        let mut children: Vec<(&UnitName, bool, Child)> = Vec::new();

        for unit in &step {
            children.push((
                &unit.name,
                true,
                match &unit.exec_spec {
                    ExecSpec::Exec(command, args) => {
                        run_exec(command, args, &validated_config.environment)?
                    }
                    ExecSpec::ExecScript(script_content) => {
                        run_exec_script(&unit.name, script_content, &validated_config.environment)?
                    }
                },
            ));
        }

        // Wait on children to terminate by themselves so we can continue in the plan.
        // This loop is also exited when the user sent a termination signal. In that
        // case, we detect the signal and clean up at the end of this step.
        let mut running_children = children.len();
        while running.load(Ordering::SeqCst) {
            if running_children == 0 {
                break;
            }
            println!("{}", running_children);

            for (_unit_name, child_running, child) in children.iter_mut() {
                if !*child_running {
                    continue;
                }

                if let Some(_exit_code) = child.try_wait()? {
                    // TODO: Report ungraceful exit in some way?

                    *child_running = false;
                    running_children -= 1;
                }
            }

            std::thread::sleep(std::time::Duration::from_millis(200));
        }

        // Termination. Kill all children and exit.
        if !running.load(Ordering::SeqCst) {
            for (_unit_name, child_running, child) in children.iter_mut() {
                if !*child_running {
                    continue;
                }

                match child.try_wait() {
                    Ok(Some(_exit_code)) => continue, // Child has been terminated after all.
                    Ok(None) => {
                        // Find the process group ID, kill it, wait for results.
                        let child_pid: Pid = Pid::from_raw(child.id() as i32);
                        let child_pgid = nix::unistd::getpgid(Some(child_pid)).unwrap();
                        nix::sys::signal::kill(child_pgid, nix::sys::signal::Signal::SIGTERM)
                            .unwrap();

                        nix::sys::wait::waitpid(child_pgid, None).unwrap();
                    }
                    Err(_) => {
                        // TODO: What should happen here?
                        continue;
                    }
                }
            }
        }
    }

    Ok(())
}

type Plan = Vec<Step>;
type Step = Vec<Unit>;

fn get_exec_plan(
    user_unit_names: &HashSet<UnitName>,
    config: &Config,
) -> Result<Plan, failure::Error> {
    let mut step = Vec::new();
    for unit_name in user_unit_names {
        step.push(config.units.get(unit_name).unwrap().clone());
    }
    Ok(vec![step])
}

fn get_help_text(config: Config) -> String {
    let mut help = "laps - Project automation\n\nCOMMANDS\n".to_string();

    let mut units: Vec<Unit> = config.units.iter().map(|(_k, v)| v.to_owned()).collect();
    units.sort_by(|a, b| a.name.cmp(&b.name));

    for unit in &units {
        let spaces = &" ".repeat(12 - &unit.name.0.len());
        help.push_str(&format!(
            "  {name}{spaces}{description}\n",
            name = &unit.name.0,
            spaces = spaces,
            description = unit.description.0.as_str()
        ))
    }

    help
}

fn run_exec_script(
    name: &UnitName,
    script_contents: &String,
    env: &HashMap<String, String>,
) -> Result<Child, failure::Error> {
    let nonce: u32 = rand::random();
    let file_name: std::path::PathBuf = format!("laps-{}-{:x}", name.0, nonce).into();
    let script_path: std::path::PathBuf = [std::env::temp_dir(), file_name].iter().collect();

    let perms = Permissions::from_mode(0o700);
    let mut file = File::create(&script_path)?;
    file.set_permissions(perms)?;
    file.write_all(script_contents.as_bytes())?;
    drop(file); // Avoid file-busy errors when executing/removing.

    let child = Command::new(&script_path).envs(env).spawn()?;

    Ok(child)
}

fn run_exec(
    command: &CommandName,
    args: &CommandArgs,
    env: &HashMap<String, String>,
) -> Result<Child, failure::Error> {
    let child = unsafe {
        Command::new(&command.0)
            .args(
                &args
                    .iter()
                    .map(|a| arg_to_string(a.to_owned(), &env))
                    .collect::<Result<Vec<String>, failure::Error>>()?,
            )
            .envs(env)
            // Ensure that any children we spawn also receive SIGTERM and SIGKILL when
            // this process receives them. That should alleviate all the problems that
            // foreman has with processes keeping running when foreman has terminated.
            //
            // The stdlib docs mention that the reason this funciton is unsafe is
            // because you should take care not to mess up file descriptors and
            // such.
            .pre_exec(|| {
                nix::unistd::setsid().map(|_| ()).map_err(|e| match e {
                    nix::Error::Sys(errno) => std::io::Error::from_raw_os_error(errno as i32),
                    _ => std::io::Error::new(std::io::ErrorKind::Other, e),
                })
            })
            .spawn()
    }?;

    Ok(child)
}

fn read_toml_config() -> Result<TomlConfig, failure::Error> {
    let mut file = std::fs::File::open("Laps.toml")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    let config: TomlConfig = toml::from_str(&content)?;
    Ok(config)
}

fn get_exec_spec(
    name: &UnitName,
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
) -> Result<ExecSpec, failure::Error> {
    failure::ensure!(
        // Check exec and exec_script are set mututually exclusively.
        exec.is_none() != exec_script.is_none(),
        LapsError::BadExec(name.to_owned())
    );

    if exec.is_some() {
        let exec: Vec<String> = exec.unwrap();
        failure::ensure!(exec.len() >= 1, LapsError::ExecCantBeEmpty);

        let command_name = CommandName(exec[0].to_owned());
        let command_args = exec[1..].to_vec(); // Potentially crashes.
        let command_args = command_args
            .iter()
            .map(|a| evaluate_arg(a.to_owned()))
            .collect();

        return Ok(ExecSpec::Exec(command_name, command_args));
    }
    if exec_script.is_some() {
        return Ok(ExecSpec::ExecScript(exec_script.unwrap()));
    }

    failure::bail!(LapsError::BadExec(name.to_owned()))
}

fn evaluate_arg(input: String) -> ArgOrLookup {
    if !input.starts_with("$") {
        return ArgOrLookup::Arg(input);
    };

    if input.starts_with("$$") {
        return ArgOrLookup::Arg(input[1..].to_string());
    };

    ArgOrLookup::Lookup(input[1..].to_string())
}

fn arg_to_string(
    arg_or_lookup: ArgOrLookup,
    exec_env: &HashMap<String, String>,
) -> Result<String, failure::Error> {
    match arg_or_lookup {
        ArgOrLookup::Arg(s) => Ok(s),
        ArgOrLookup::Lookup(s) => {
            let val = exec_env.get(&s).ok_or(LapsError::BadLookup(s.clone()))?;
            Ok(val.to_string())
        }
    }
}

fn validate_config(toml_config: TomlConfig) -> Result<Config, failure::Error> {
    let mut units: HashMap<UnitName, Unit> = HashMap::new();

    let mut exec_env = std::env::vars().collect::<HashMap<String, String>>();
    for (key, value) in &toml_config.environment {
        exec_env.insert(key.to_string(), value.to_string());
    }

    for (name, command) in toml_config.commands {
        let name: UnitName = UnitName(name);
        let exec_spec: ExecSpec = get_exec_spec(&name, command.exec, command.exec_script)?;
        let prev = units.insert(
            name.clone(),
            Unit {
                name: name,
                description: UnitDescription(command.description),
                exec_spec: exec_spec,
                wants: Vec::new(),
                typ: UnitType::Command,
            },
        );
        failure::ensure!(prev.is_none(), LapsError::Duplicate)
    }

    for (name, service) in toml_config.services {
        let name: UnitName = UnitName(name);
        let exec_spec: ExecSpec = get_exec_spec(&name, service.exec, service.exec_script)?;
        let prev = units.insert(
            name.clone(),
            Unit {
                name: name,
                description: UnitDescription(service.description),
                exec_spec: exec_spec,
                wants: service
                    .wants
                    .iter()
                    .map(|s| UnitName(s.to_owned()))
                    .collect(),
                typ: UnitType::Service,
            },
        );
        failure::ensure!(prev.is_none(), LapsError::Duplicate)
    }

    for (name, watch) in toml_config.watches {
        let name: UnitName = UnitName(name);
        // Ugly: a watch is a bash script calling `fd` and `entr`
        let exec_spec: ExecSpec = ExecSpec::ExecScript(format!(
            "#!/bin/bash
fd -t f {extension_str} | entr {exec}
",
            extension_str = watch
                .file_types
                .iter()
                .map(|e| format!("-e {}", e))
                .collect::<Vec<String>>()
                .join(" "),
            exec = watch.exec.join(" ")
        ));
        let prev = units.insert(
            name.clone(),
            Unit {
                name: name,
                description: UnitDescription(watch.description),
                exec_spec: exec_spec,
                wants: Vec::new(),
                typ: UnitType::Watch,
            },
        );
        failure::ensure!(prev.is_none(), LapsError::Duplicate)
    }

    let config = Config {
        environment: exec_env,
        units: units,
    };

    let config = ensure_deps_exist(config)?;

    Ok(config)
}

fn ensure_deps_exist(config: Config) -> Result<Config, failure::Error> {
    for (name, unit) in &config.units {
        for dep in &unit.wants {
            failure::ensure!(
                config.units.contains_key(&dep),
                LapsError::UnknownDeps(name.clone(), dep.clone())
            )
        }
    }

    Ok(config)
}
