#![cfg(feature = "cli")]

use assembler::{config::Config, parse};
use clap::StructOpt;
fn main() {
    let c = Config::parse();

    // #[cfg(not(feature = "dwarf"))]
    // parse(&c.input, c.out, None);

    // #[cfg(feature = "dwarf")]
    parse(&c.input, c.out, c.dwarf_file);
}
