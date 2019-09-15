use clap;
use failure;
use serde;
use std::collections::HashMap;
use std::io::Read;
use toml;

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Script {
    script: String,
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Service {
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
    #[fail(display = "Unused placeholder")]
    Unused,
}

fn main() -> Result<(), failure::Error> {
    let config = read_config()?;
    let app: clap::App = clap::App::new("laps - project automation.");
    let matches = app.get_matches();

    Ok(())
}

fn read_config() -> Result<LapsConfig, failure::Error> {
    let mut file = std::fs::File::open("Laps.toml")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    let config: LapsConfig = toml::from_str(&content)?;
    Ok(config)
}
