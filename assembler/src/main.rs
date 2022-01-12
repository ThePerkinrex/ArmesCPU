use assembler::{config::Config, parse};
use clap::StructOpt;
fn main() {
    let c = Config::parse();

    parse(&c.input, c.out)
}
