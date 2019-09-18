use clap;
use clap::{app_from_crate, crate_authors, crate_description, crate_name, crate_version};

use failure;
use serde;
use std::collections::{HashMap, HashSet};
use std::fs::{File, Permissions};
use std::io::Read;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::process::Command;
use toml;

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Script {
    help: Option<String>,
    script: String,
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Service {
    help: Option<String>,
    command: Vec<String>,
    exec_before: Option<Vec<String>>,
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct LapsConfig {
    scripts: HashMap<String, Script>,
    services: HashMap<String, Service>,
}

#[derive(Debug, failure::Fail)]
enum LapsError {
    #[fail(display = "Duplicate names in scripts and services")]
    Duplicates(HashSet<String>),
    #[fail(display = "Please give a subcommand.")]
    MissingSubcommand,
    #[fail(display = "Script failed")]
    ScriptFailed,
}

fn main() -> Result<(), failure::Error> {
    let config = read_config()?;
    println!("{:?}", config);

    let sub_template = r#"{bin}

{about}

USAGE:
    {usage}"#;

    let mut subcommands: Vec<clap::App> = Vec::new();
    for (name, script) in &config.scripts {
        let subcommand = clap::SubCommand::with_name(name).template(sub_template);
        let subcommand = match &script.help {
            Some(s) => subcommand.about(s.as_str()),
            None => subcommand,
        };
        subcommands.push(subcommand)
    }

    let template = r#"{bin} - {about}

PROJECT COMMANDS

{subcommands}"#;

    let app: clap::App = app_from_crate!()
        .template(template)
        .setting(clap::AppSettings::DisableHelpSubcommand)
        .setting(clap::AppSettings::VersionlessSubcommands)
        .setting(clap::AppSettings::SubcommandRequiredElseHelp)
        .subcommands(subcommands);

    let matches = app.get_matches();

    let script: &Script = match matches.subcommand_name() {
        Some(s) => config.scripts.get(s).unwrap(), // Safe; otherwise bug in clap.
        None => failure::bail!(LapsError::MissingSubcommand),
    };

    run_script(script)?;

    Ok(())
}

fn run_script(script: &Script) -> Result<(), failure::Error> {
    let file_path: std::path::PathBuf = [std::env::temp_dir(), "laps-script".into()]
        .iter()
        .collect();

    println!("Writing script contents to {:?}", file_path);
    let mut file = File::create(file_path.clone())?;
    file.write_all(script.script.as_bytes())?;
    drop(file);

    println!("Setting script permissions");
    let perms = Permissions::from_mode(0o755);
    std::fs::set_permissions(file_path.clone(), perms)?;

    println!("Executing script");
    let mut child = Command::new(file_path).spawn()?;
    let exitcode = child.wait()?;

    failure::ensure!(exitcode.success(), LapsError::ScriptFailed);

    Ok(())
}

fn read_config() -> Result<LapsConfig, failure::Error> {
    let mut file = std::fs::File::open("Laps.toml")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    let config: LapsConfig = toml::from_str(&content)?;
    let config = check_duplicate_names(config)?;

    Ok(config)
}

fn check_duplicate_names(config: LapsConfig) -> Result<LapsConfig, failure::Error> {
    let script_names: HashSet<String> = config.scripts.keys().cloned().collect();
    let service_names: HashSet<String> = config.services.keys().cloned().collect();
    let intersection: HashSet<String> =
        script_names.intersection(&service_names).cloned().collect();

    failure::ensure!(intersection.len() == 0, LapsError::Duplicates(intersection));
    Ok(config)
}
