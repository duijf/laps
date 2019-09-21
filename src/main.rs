use clap;
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
    #[fail(display = "Duplicate names in scripts and services")]
    Duplicates(HashSet<String>),
    #[fail(display = "Duplicate names somewhere")]
    Duplicate,
    #[fail(display = "Please give a subcommand.")]
    MissingSubcommand,
    #[fail(display = "Script failed")]
    UnitFailed,
    #[fail(display = "Service failed")]
    ServiceFailed,
    #[fail(display = "Service has unknown dependencies")]
    UnknownDeps(String, HashSet<String>),
}

fn main() -> Result<(), failure::Error> {
    let toml_config: TomlConfig = read_toml_config()?;
    let validated_config: Config = validate_config(toml_config)?;
    println!("{:?}", validated_config);

    // let sub_template = "{bin}\n\n  {about}\n\nUSAGE\n  {usage}";
    // let mut subcommand_help: String = "".to_string();

    // let mut subcommands: Vec<clap::App> = Vec::new();
    // for (name, script) in &config.scripts {
    //     let subcommand = clap::SubCommand::with_name(name)
    //         .template(sub_template)
    //         .about(script.description.as_str());
    //     subcommands.push(subcommand);

    //     subcommand_help.push_str("  ");
    //     subcommand_help.push_str(name);
    //     subcommand_help.push_str(&" ".repeat(12 - name.len()));
    //     subcommand_help.push_str(script.description.as_str());
    //     subcommand_help.push('\n');
    // }

    // let mut services_help: String = "".to_string();
    // for (name, service) in &config.services {
    //     let subcommand = clap::SubCommand::with_name(name)
    //         .template(sub_template)
    //         .about(service.description.as_str());
    //     subcommands.push(subcommand);

    //     services_help.push_str("  ");
    //     services_help.push_str(name);
    //     services_help.push_str(&" ".repeat(12 - name.len()));
    //     services_help.push_str(service.description.as_str());
    //     services_help.push('\n');
    // }

    // let help_text: String = format!(
    //     "laps - Project automation.

    // COMMANDS
    // {subcommand_help}
    // SERVICES
    // {services_help}",
    //     subcommand_help = subcommand_help,
    //     services_help = services_help,
    // );

    // let app: clap::App = clap::App::new("laps")
    //     .version(clap::crate_version!())
    //     .help(&*help_text)
    //     .setting(clap::AppSettings::DisableHelpSubcommand)
    //     .setting(clap::AppSettings::VersionlessSubcommands)
    //     .setting(clap::AppSettings::SubcommandRequiredElseHelp)
    //     .subcommands(subcommands);

    // let matches = app.get_matches();

    // let script_or_cmd_name = matches
    //     .subcommand_name()
    //     .ok_or(LapsError::MissingSubcommand)?;

    // let to_run: Vec<ScriptOrService> = find_to_run(script_or_cmd_name, &config);
    // run(to_run, config)?;

    Ok(())
}

// fn find_to_run(script_or_cmd_name: &str, config: &LapsConfig) -> Vec<ScriptOrService> {
//     let config: LapsConfig = (config).to_owned();

//     if config.scripts.contains_key(script_or_cmd_name) {
//         return vec![ScriptOrService::Script(
//             (config.scripts.get(script_or_cmd_name).unwrap()).to_owned(),
//         )];
//     }

//     let mut res = vec![];

//     // Right now, we know it was a service instead of a script
//     let service: TomlService = config.services.get(script_or_cmd_name).unwrap().to_owned();

//     let service_orig = service.clone();

//     for dep in service.exec_before {
//         res.push(ScriptOrService::Script(
//             config.scripts.get(&dep).unwrap().to_owned(),
//         ));
//     }
//     res.push(ScriptOrService::Service(service_orig));

//     res
// }

// fn run(to_run: Vec<ScriptOrService>, config: LapsConfig) -> Result<(), failure::Error> {
//     for thing in to_run {
//         match thing {
//             ScriptOrService::Script(script) => run_script(&script, &config.environment),
//             ScriptOrService::Service(service) => run_service(&service, &config.environment),
//         };
//     }
//     Ok(())
// }

// fn run_script(script: &TomlCommand, env: &HashMap<String, String>) -> Result<(), failure::Error> {
//     let file_path: std::path::PathBuf = [std::env::temp_dir(), "laps-script".into()]
//         .iter()
//         .collect();

//     println!("Writing script contents to {:?}", file_path);
//     let mut file = File::create(file_path.clone())?;
//     file.write_all(script.script.as_bytes())?;
//     drop(file);

//     println!("Setting script permissions");
//     let perms = Permissions::from_mode(0o755);
//     std::fs::set_permissions(file_path.clone(), perms)?;

//     println!("Executing script");
//     let mut child = Command::new(file_path).envs(env).spawn()?;
//     let exitcode = child.wait()?;

//     failure::ensure!(exitcode.success(), LapsError::ScriptFailed);

//     Ok(())
// }

// fn run_service(service: &TomlService, env: &HashMap<String, String>) -> Result<(), failure::Error> {
//     println!("Executing script");
//     let mut child = Command::new(service.command[0].clone())
//         .args(service.command[1..].iter())
//         .envs(env)
//         .spawn()?;
//     let exitcode = child.wait()?;

//     failure::ensure!(exitcode.success(), LapsError::ServiceFailed);

//     Ok(())
// }

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
            },
        );
        failure::ensure!(prev.is_none(), LapsError::Duplicate)
    }

    Ok(Config {
        environment: toml_config.environment,
        units: units,
    })
}

// fn check_duplicate_names(config: LapsConfig) -> Result<LapsConfig, failure::Error> {
//     let script_names: HashSet<String> = config.scripts.keys().cloned().collect();
//     let service_names: HashSet<String> = config.services.keys().cloned().collect();
//     let intersection: HashSet<String> =
//         script_names.intersection(&service_names).cloned().collect();

//     failure::ensure!(intersection.len() == 0, LapsError::Duplicates(intersection));
//     Ok(config)
// }

// fn ensure_deps_exist(config: LapsConfig) -> Result<LapsConfig, failure::Error> {
//     let script_names: HashSet<String> = config.scripts.keys().cloned().collect();

//     for (name, service) in &config.services {
//         let dependencies: HashSet<String> = service.exec_before.iter().cloned().collect();
//         let unknown_deps: HashSet<String> =
//             dependencies.difference(&script_names).cloned().collect();

//         failure::ensure!(
//             unknown_deps.len() == 0,
//             LapsError::UnknownDeps(name.to_string(), unknown_deps)
//         );
//     }

//     Ok(config)
// }
