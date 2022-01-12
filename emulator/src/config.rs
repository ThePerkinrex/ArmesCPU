use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
pub struct Config {
    /// Input file
    // #[clap(short, long)]
    pub input: PathBuf,
}
