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
    exec: Option<Vec<String>>,
    exec_script: Option<String>,
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

#[derive(Debug, failure::Fail)]
enum LapsError {
    #[fail(display = "Duplicate names in scripts and services")]
    Duplicates(HashSet<String>),
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
    let config = read_toml_config()?;
    println!("{:?}", config);

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

    // let config = check_duplicate_names(config)?;
    // let config = ensure_deps_exist(config)?;

    Ok(config)
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
