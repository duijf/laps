use clap;
use failure;
use serde;
use std::collections::{HashMap, HashSet};
use std::io::Read;
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
}

fn main() -> Result<(), failure::Error> {
    let config = read_config()?;
    println!("{:?}", config);

    let mut subcommands: Vec<clap::App> = Vec::new();
    for (name, script) in &config.scripts {
        let subcommand = clap::SubCommand::with_name(name);
        let subcommand = match &script.help {
            Some(s) => subcommand.about(s.as_str()),
            None => subcommand,
        };
        subcommands.push(subcommand)
    }

    let app: clap::App = clap::App::new("laps - project automation.").subcommands(subcommands);

    let matches = app.get_matches();

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
