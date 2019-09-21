use failure;
use serde;
use std::collections::{HashMap, HashSet};
use std::fs::{File, Permissions};
use std::io::Read;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::process::Command;
use toml;

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]
struct TomlCommand {
    description: String,
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
}

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]
struct TomlService {
    description: String,
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
    #[serde(default=[])]
    wants: Vec<String>,
}

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]
struct TomlWatch {
    description: String,
    exec: Vec<String>,
    #[serde(default=[])]
    file_types: Vec<String>,
}

#[derive(Debug, serde::Deserialize, Clone)]
#[serde(rename_all = "kebab-case")]
struct TomlConfig {
    environment: HashMap<String, String>,
    commands: HashMap<String, TomlCommand>,
    services: HashMap<String, TomlService>,
    watches: HashMap<String, TomlWatch>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct UnitName(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct UnitDescription(String);

#[derive(Debug, Clone)]
struct CommandName(String);

#[derive(Debug, Clone)]
struct CommandArgs(Vec<String>);

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
    BadExec,
    #[fail(display = "Exec stanza cannot be empty")]
    ExecCantBeEmpty,
    #[fail(display = "Duplicate names somewhere")]
    Duplicate,
    #[fail(display = "Script failed")]
    UnitFailed,
    #[fail(display = "Service has unknown dependencies")]
    UnknownDeps(UnitName, UnitName),
    #[fail(display = "Unknown targets specified")]
    UnknownTargets(HashSet<UnitName>),
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

    for unit_name in user_specified_units {
        let unit = validated_config.units.get(&unit_name).unwrap();
        match &unit.exec_spec {
            ExecSpec::Exec(command, args) => {
                run_exec(command, args, &validated_config.environment)?
            }
            ExecSpec::ExecScript(script_content) => {
                run_exec_script(script_content, &validated_config.environment)?
            }
        }
    }

    Ok(())
}

fn get_help_text(config: Config) -> String {
    let mut help = "laps - Project automation\n\nCOMMANDS\n".to_string();

    for (name, unit) in &config.units {
        let spaces = &" ".repeat(12 - name.0.len());
        help.push_str(&format!(
            "  {name}{spaces}{description}\n",
            name = name.0,
            spaces = spaces,
            description = unit.description.0.as_str()
        ))
    }

    help
}

fn run_exec_script(
    script_contents: &String,
    env: &HashMap<String, String>,
) -> Result<(), failure::Error> {
    let file_path: std::path::PathBuf = [std::env::temp_dir(), "laps-script".into()]
        .iter()
        .collect();

    println!("Writing script contents to {:?}", file_path);
    let mut file = File::create(file_path.clone())?;
    file.write_all(script_contents.as_bytes())?;
    drop(file);

    println!("Setting script permissions");
    let perms = Permissions::from_mode(0o755);
    std::fs::set_permissions(file_path.clone(), perms)?;

    println!("Executing script");
    let mut child = Command::new(file_path).envs(env).spawn()?;
    let exitcode = child.wait()?;

    failure::ensure!(exitcode.success(), LapsError::UnitFailed);

    Ok(())
}

fn run_exec(
    command: &CommandName,
    args: &CommandArgs,
    env: &HashMap<String, String>,
) -> Result<(), failure::Error> {
    println!("Executing script");
    let mut child = Command::new(&command.0).args(&args.0).envs(env).spawn()?;
    let exitcode = child.wait()?;

    failure::ensure!(exitcode.success(), LapsError::UnitFailed);

    Ok(())
}

fn read_toml_config() -> Result<TomlConfig, failure::Error> {
    let mut file = std::fs::File::open("Laps.toml")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    let config: TomlConfig = toml::from_str(&content)?;
    Ok(config)
}

fn get_exec_spec(
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
) -> Result<ExecSpec, failure::Error> {
    failure::ensure!(
        // Check exec and exec_script are set mututually exclusively.
        exec.is_none() != exec_script.is_none(),
        LapsError::BadExec
    );

    if exec.is_some() {
        let exec: Vec<String> = exec.unwrap();
        failure::ensure!(exec.len() >= 1, LapsError::ExecCantBeEmpty);

        let command_name = CommandName(exec[0].to_owned());
        let command_args = CommandArgs(exec[1..].to_vec()); // Potentially crashes.

        return Ok(ExecSpec::Exec(command_name, command_args));
    }
    if exec_script.is_some() {
        return Ok(ExecSpec::ExecScript(exec_script.unwrap()));
    }

    failure::bail!(LapsError::BadExec)
}

fn validate_config(toml_config: TomlConfig) -> Result<Config, failure::Error> {
    let mut units: HashMap<UnitName, Unit> = HashMap::new();

    for (name, command) in toml_config.commands {
        let name: UnitName = UnitName(name);
        let exec_spec: ExecSpec = get_exec_spec(command.exec, command.exec_script)?;
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
        let exec_spec: ExecSpec = get_exec_spec(service.exec, service.exec_script)?;
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
        environment: toml_config.environment,
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
