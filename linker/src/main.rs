#![cfg(feature = "cli")]

use std::{fs::File, path::PathBuf};

use armes_elf::Elf;
use asm_ir::Program;
use clap::Parser;
use linker::{link_to_elf, link_to_program};

#[derive(Debug, Parser)]
struct Args {
    #[clap(min_values = 1, required = true)]
    files: Vec<PathBuf>,

    #[clap(
        short,
        default_value = "a.out",
        required_unless_present = "elf",
        conflicts_with = "elf"
    )]
    out: PathBuf,

    #[clap(short, required_unless_present = "out", conflicts_with = "out")]
    elf: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let mut elf_files = Vec::with_capacity(args.files.len());
    for f in args.files {
        let e = Elf::parse(&std::fs::read(&f).unwrap()).unwrap();
        elf_files.push((e, f.file_name().unwrap().to_string_lossy().to_string()))
    }
    if let Some(elf) = args.elf {
        let elf_file = link_to_elf(elf_files).unwrap();
        elf_file.write(&mut File::create(elf).unwrap()).unwrap();
    } else {
        let program = Program {
            segments: link_to_program(elf_files).unwrap(),
        };

        program.write(&mut File::create(args.out).unwrap()).unwrap();
    }
}
