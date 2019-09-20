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

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Script {
    help: String,
    script: String,
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Service {
    help: String,
    command: Vec<String>,
    #[serde(default=[])]
    exec_before: Vec<String>,
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct LapsConfig {
    env: HashMap<String, String>,
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
    #[fail(display = "Service has unknown dependencies")]
    UnknownDeps(String, HashSet<String>),
}

fn main() -> Result<(), failure::Error> {
    let config = read_config()?;
    println!("{:?}", config);

    let sub_template = "{bin}\n\n  {about}\n\nUSAGE\n  {usage}";
    let mut subcommand_help: String = "".to_string();

    let mut subcommands: Vec<clap::App> = Vec::new();
    for (name, script) in &config.scripts {
        let subcommand = clap::SubCommand::with_name(name)
            .template(sub_template)
            .about(script.help.as_str());
        subcommands.push(subcommand);

        subcommand_help.push_str("  ");
        subcommand_help.push_str(name);
        subcommand_help.push_str(&" ".repeat(12 - name.len()));
        subcommand_help.push_str(script.help.as_str());
        subcommand_help.push('\n');
    }

    let mut services_help: String = "".to_string();
    for (name, service) in &config.services {
        let subcommand = clap::SubCommand::with_name(name)
            .template(sub_template)
            .about(service.help.as_str());
        subcommands.push(subcommand);

        services_help.push_str("  ");
        services_help.push_str(name);
        services_help.push_str(&" ".repeat(12 - name.len()));
        services_help.push_str(service.help.as_str());
        services_help.push('\n');
    }

    let help_text: String = format!(
        "laps - Project automation.

COMMANDS
{subcommand_help}
SERVICES
{services_help}",
        subcommand_help = subcommand_help,
        services_help = services_help,
    );

    let app: clap::App = clap::App::new("laps")
        .version(clap::crate_version!())
        .help(&*help_text)
        .setting(clap::AppSettings::DisableHelpSubcommand)
        .setting(clap::AppSettings::VersionlessSubcommands)
        .setting(clap::AppSettings::SubcommandRequiredElseHelp)
        .subcommands(subcommands);

    let matches = app.get_matches();

    let script: &Script = match matches.subcommand_name() {
        Some(s) => config.scripts.get(s).unwrap(), // Safe; otherwise bug in clap.
        None => failure::bail!(LapsError::MissingSubcommand),
    };

    run_script(script, config.env)?;

    Ok(())
}

fn run_script(script: &Script, env: HashMap<String, String>) -> Result<(), failure::Error> {
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
    let mut child = Command::new(file_path).envs(env).spawn()?;
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
    let config = ensure_deps_exist(config)?;

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

fn ensure_deps_exist(config: LapsConfig) -> Result<LapsConfig, failure::Error> {
    let script_names: HashSet<String> = config.scripts.keys().cloned().collect();

    for (name, service) in &config.services {
        let dependencies: HashSet<String> = service.exec_before.iter().cloned().collect();
        let unknown_deps: HashSet<String> =
            dependencies.difference(&script_names).cloned().collect();

        failure::ensure!(
            unknown_deps.len() == 0,
            LapsError::UnknownDeps(name.to_string(), unknown_deps)
        );
    }

    Ok(config)
}
