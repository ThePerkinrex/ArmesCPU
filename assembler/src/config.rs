use std::path::PathBuf;

use clap::Parser;

/// An assembler for the `ArmesCPU` instruction set
///
// #[cfg_attr(
//     feature = "dwarf",
//     doc = "(DWARF information generation feature enabled)  "
// )]
#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct Config {
    // #[cfg(feature = "dwarf")]
    #[clap(short, long = "dwarf", required = false)]
    /// File to write debug information to
    pub dwarf_file: Option<PathBuf>,

    /// Files to compile
    // #[clap(short, long)]
    pub input: Vec<PathBuf>,

    /// Output file
    #[clap(short, long, default_value = "a.out")]
    pub out: PathBuf,
}
