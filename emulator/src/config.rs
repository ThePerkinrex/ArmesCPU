use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Config {
    /// Input file
    // #[clap(short, long)]
    pub input: PathBuf,
    #[clap(name = "disk", short, long, multiple_values = false)]
    pub hdd: Vec<PathBuf>,

    /// Verbosity (gives register report on each instruction)
    #[clap(short, long)]
    pub verbose: bool,
}
