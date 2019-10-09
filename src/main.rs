use failure;
use nix::unistd::Pid;
use rand;
use serde;
use std::collections::VecDeque;
use std::collections::{HashMap, HashSet};
use std::fs::{remove_dir_all, DirBuilder, File, Permissions};
use std::io::Read;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::os::unix::process::CommandExt;
use std::path::PathBuf;
use std::process::{Child, Command, ExitStatus};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use toml;

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
struct TomlCommand {
    description: String,
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
    #[serde(default)]
    wants_started: Vec<String>,
    #[serde(default)]
    wants_finished: Vec<String>,
}

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
struct TomlService {
    description: String,
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
    #[serde(default)]
    wants_started: Vec<String>,
    #[serde(default)]
    wants_finished: Vec<String>,
}

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
struct TomlWatch {
    description: String,
    exec: Vec<String>,
    #[serde(default)]
    file_types: Vec<String>,
    #[serde(default)]
    wants_started: Vec<String>,
    #[serde(default)]
    wants_finished: Vec<String>,
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
    wants_started: Vec<UnitName>,
    wants_finished: Vec<UnitName>,
    after_started: Vec<UnitName>,
    after_finished: Vec<UnitName>,
    typ: UnitType,
}

#[derive(Debug, Clone)]
enum UnitType {
    Service,
    Command,
    Watch,
}

// TODO: We cannot clone this, so we cannot put it in `Unit`.
#[derive(Debug)]
enum UnitStatus {
    Inactive,
    Running(Child, Pid),
    Finished(ExitStatus),
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

impl Config {
    fn get_unit_names(&self) -> HashSet<UnitName> {
        self.units.keys().cloned().collect()
    }
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
    dbg!(&validated_config);

    let available_units: HashSet<UnitName> = validated_config.units.keys().cloned().collect();
    let user_specified_units: HashSet<UnitName> = std::env::args()
        .skip(1)
        .map(|a| UnitName(a.trim().to_string()))
        .collect();

    if user_specified_units.is_empty() {
        let help_text = get_help_text(validated_config);
        print!("{}", help_text);
        return Ok(());
    }

    let unknown_targets: HashSet<UnitName> = user_specified_units
        .difference(&available_units)
        .cloned()
        .collect();
    failure::ensure!(
        unknown_targets.is_empty(),
        LapsError::UnknownTargets(unknown_targets)
    );

    let exec_plan: NewPlan = get_new_exec_plan(user_specified_units, &validated_config)?;

    let exec_plan = dbg!(exec_plan);

    let running = Arc::new(AtomicBool::new(true));
    let r = running.clone();
    ctrlc::set_handler(move || {
        r.store(false, Ordering::SeqCst);
    })?;

    let temp_dir_base: PathBuf = [
        std::env::temp_dir(),
        format!("laps-{:x}", rand::random::<u32>()).into(),
    ]
    .iter()
    .collect();

    DirBuilder::new().recursive(true).create(&temp_dir_base)?;
    std::fs::set_permissions(&temp_dir_base, Permissions::from_mode(0o700))?;

    let mut children: HashMap<UnitName, UnitStatus> = HashMap::new();
    for unit_name in exec_plan.units {
        children.insert(unit_name.clone(), UnitStatus::Inactive);
    }
    let num_children = children.len();

    // Wait on children to terminate by themselves so we can continue in the plan.
    // This loop is also exited when the user sent a termination signal. In that
    // case, we detect the signal and clean up at the end of this step.
    let mut units_finished: usize = 0;
    while units_finished < num_children && running.load(Ordering::SeqCst) {
        for (unit_name, unit_status) in children.iter_mut() {
            match unit_status {
                UnitStatus::Inactive => {
                    if exec_plan.roots.contains(&unit_name) {
                        let unit = validated_config.units.get(&unit_name).unwrap();
                        match &unit.exec_spec {
                            ExecSpec::Exec(command, args) => {
                                let child =
                                    run_exec(&command, &args, &validated_config.environment)?;
                                let child_pid: Pid = Pid::from_raw(child.id() as i32);
                                *unit_status = UnitStatus::Running(child, child_pid);
                            }
                            ExecSpec::ExecScript(script_content) => {
                                let child = run_exec_script(
                                    &unit.name,
                                    &script_content,
                                    &validated_config.environment,
                                    &temp_dir_base,
                                )?;
                                let child_pid: Pid = Pid::from_raw(child.id() as i32);
                                *unit_status = UnitStatus::Running(child, child_pid);
                            }
                        }
                    }
                }
                UnitStatus::Running(child, _pid) => {
                    if let Some(exit_code) = child.try_wait()? {
                        *unit_status = UnitStatus::Finished(exit_code);
                    }
                }
                UnitStatus::Finished(_exit_status) => {
                    units_finished += 1;
                }
            }
        }
        std::thread::sleep(std::time::Duration::from_millis(200));
    }

    // Termination. Kill all children and exit.
    if !running.load(Ordering::SeqCst) {
        for (_unit_name, status) in children.iter_mut() {
            match status {
                UnitStatus::Inactive => {}
                UnitStatus::Running(child, pid) => {
                    match child.try_wait() {
                        Ok(Some(_exit_code)) => continue, // Child has been terminated after all.
                        Ok(None) => {
                            // Find the process group ID, kill it, wait for results. This ensures
                            // that there are never any processes left running when we terminate
                            // laps.
                            let child_pgid = nix::unistd::getpgid(Some(*pid)).unwrap();
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
                UnitStatus::Finished(_exit_status) => {}
            }
        }
    }

    remove_dir_all(&temp_dir_base)?;

    Ok(())
}

#[derive(Debug)]
struct NewPlan {
    roots: Vec<UnitName>,
    units: HashSet<UnitName>,
}

fn get_new_exec_plan(
    user_unit_names: HashSet<UnitName>,
    config: &Config,
) -> Result<NewPlan, failure::Error> {
    let mut roots = Vec::new();
    let mut units = HashSet::new();

    // BFS over the config to find a whitelist of all units required for
    // what the user wanted, as well as a set of roots to start executing
    // from. If a unit has no dependencies and it's reachable through the
    // dependency graph of the units the user wants executed, it is a root.
    let mut queue: VecDeque<UnitName> = user_unit_names.into_iter().collect();
    while let Some(unit) = queue.pop_back() {
        units.insert(unit.clone());
        let mut deps = 0;
        for dep in &config.units.get(&unit).unwrap().wants_started {
            if !units.contains(&dep) {
                queue.push_front(dep.clone());
            }
            deps += 1;
        }
        for dep in &config.units.get(&unit).unwrap().wants_finished {
            if !units.contains(&dep) {
                queue.push_front(dep.clone());
            }
            deps += 1;
        }
        if deps == 0 {
            roots.push(unit);
        }
    }

    Ok(NewPlan { roots, units })
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
    script_contents: &str,
    env: &HashMap<String, String>,
    temp_dir_base: &PathBuf,
) -> Result<Child, failure::Error> {
    let script_path: PathBuf = temp_dir_base.join(name.0.to_string());

    let perms = Permissions::from_mode(0o700);
    let mut file = File::create(&script_path)?;
    file.set_permissions(perms)?;
    file.write_all(script_contents.as_bytes())?;
    drop(file); // Avoid file-busy errors when executing/removing.

    let child = Command::new(&script_path).envs(env).spawn()?;

    Ok(child)
}

#[allow(clippy::ptr_arg)] // Was too much effort to fix. Might revisit
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

    if let Some(exec) = exec {
        failure::ensure!(!exec.is_empty(), LapsError::ExecCantBeEmpty);

        let command_name = CommandName(exec[0].to_owned());
        let command_args = exec[1..].to_vec(); // Potentially crashes.
        let command_args = command_args
            .iter()
            .map(|a| evaluate_arg(a.to_owned()))
            .collect();

        return Ok(ExecSpec::Exec(command_name, command_args));
    }
    if let Some(exec_script) = exec_script {
        return Ok(ExecSpec::ExecScript(exec_script));
    }

    failure::bail!(LapsError::BadExec(name.to_owned()))
}

fn evaluate_arg(input: String) -> ArgOrLookup {
    if !input.starts_with('$') {
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
            let val = exec_env
                .get(&s)
                .ok_or_else(|| LapsError::BadLookup(s.clone()))?;
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
                name,
                description: UnitDescription(command.description),
                exec_spec,
                wants_started: command
                    .wants_started
                    .iter()
                    .map(|s| UnitName(s.to_owned()))
                    .collect(),
                wants_finished: command
                    .wants_finished
                    .iter()
                    .map(|s| UnitName(s.to_owned()))
                    .collect(),
                after_started: Vec::new(),
                after_finished: Vec::new(),
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
                name,
                description: UnitDescription(service.description),
                exec_spec,
                wants_started: service
                    .wants_started
                    .iter()
                    .map(|s| UnitName(s.to_owned()))
                    .collect(),
                wants_finished: service
                    .wants_finished
                    .iter()
                    .map(|s| UnitName(s.to_owned()))
                    .collect(),
                after_started: Vec::new(),
                after_finished: Vec::new(),
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
                name,
                description: UnitDescription(watch.description),
                exec_spec,
                wants_started: watch
                    .wants_started
                    .iter()
                    .map(|s| UnitName(s.to_owned()))
                    .collect(),
                wants_finished: watch
                    .wants_finished
                    .iter()
                    .map(|s| UnitName(s.to_owned()))
                    .collect(),
                after_started: Vec::new(),
                after_finished: Vec::new(),
                typ: UnitType::Watch,
            },
        );
        failure::ensure!(prev.is_none(), LapsError::Duplicate)
    }

    let config = Config {
        environment: exec_env,
        units,
    };

    let config = ensure_deps_exist(config)?;
    let config = add_reverse_deps(config)?;

    Ok(config)
}

fn ensure_deps_exist(config: Config) -> Result<Config, failure::Error> {
    for (name, unit) in &config.units {
        for dep in unit.wants_started.iter().chain(&unit.wants_finished) {
            failure::ensure!(
                config.units.contains_key(&dep),
                LapsError::UnknownDeps(name.clone(), dep.clone())
            )
        }
    }

    Ok(config)
}

fn add_reverse_deps(mut config: Config) -> Result<Config, failure::Error> {
    dbg!(&config);

    let mut after_started: HashMap<UnitName, Vec<UnitName>> = config
        .get_unit_names()
        .into_iter()
        .map(|u| (u, Vec::new()))
        .collect();
    let mut after_finished: HashMap<UnitName, Vec<UnitName>> = after_started.clone();

    for (name, unit) in &config.units {
        for dep in unit.wants_started.iter() {
            after_started.get_mut(dep).unwrap().push(name.clone());
        }
        for dep in unit.wants_finished.iter() {
            after_finished.get_mut(dep).unwrap().push(name.clone());
        }
    }

    dbg!(&after_finished);
    dbg!(&after_started);

    for (name, mut units) in after_started {
        config
            .units
            .get_mut(&name)
            .unwrap()
            .after_started
            .append(&mut units);
    }
    for (name, mut units) in after_finished {
        config
            .units
            .get_mut(&name)
            .unwrap()
            .after_finished
            .append(&mut units);
    }

    Ok(config)
}
