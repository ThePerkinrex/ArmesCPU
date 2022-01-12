use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Config {
    /// Files to compile
    // #[clap(short, long)]
    pub input: Vec<PathBuf>,

    /// Output file
    #[clap(short, long, default_value = "a.out")]
    pub out: PathBuf,
}