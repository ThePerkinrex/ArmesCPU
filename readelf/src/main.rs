use std::{iter::repeat, path::PathBuf};

use armes_elf::{Elf, Pointee};
use clap::Parser;

#[derive(Parser)]
struct Args {
    /// ELF files to print out
    #[clap(min_values = 1, required = true)]
    files: Vec<PathBuf>,
    /// Segments to hexdump, by id. Ex: `-s 0 -s 1`
    #[clap(short, parse(try_from_str = parse_ids), multiple_values = false)]
    segments: Vec<(usize, usize)>,
}

fn parse_ids(s: &str) -> Result<(usize, usize), String> {
    if let Some((file, segment)) = s.split_once('/') {
        let file = match file.parse::<usize>() {
            Ok(f) => f,
            Err(e) => return Err(e.to_string()),
        };
        let segment = match segment.parse::<usize>() {
            Ok(s) => s,
            Err(e) => return Err(e.to_string()),
        };
        Ok((file, segment))
    } else {
        Err(format!("Missing '/' sigil in {:?}", s))
    }
}

fn hexdump(bytes: &[u8]) {
    let mut last_addr = 0;
    for (addr, data) in bytes.chunks(16).scan(0, |s, v| {
        let og_s = *s;
        *s += v.len();
        Some((og_s, v))
    }) {
        last_addr = addr + data.len();
        print!("{addr:0>8x} ");
        for (i, v) in data
            .iter()
            .map(Some)
            .chain(repeat(None))
            .take(16)
            .enumerate()
        {
            if i % 8 == 0 {
                print!(" ")
            }
            v.map_or_else(
                || print!("   "),
                |v| {
                    print!("{v:0>2x} ");
                },
            )
        }

        print!(" |");
        for v in data {
            let c = *v as char;
            if c.is_ascii_graphic() {
                print!("{}", c)
            } else {
                print!(".")
            }
        }
        println!("|")
    }
    println!("{last_addr:0>8x}")
}

fn main() {
    let args = Args::parse();

    for (i, file) in args.files.into_iter().enumerate() {
        println!(">>>>> {} ", file.display());
        if file.exists() {
            let elf = Elf::parse(&std::fs::read(file).unwrap()).unwrap();
            println!("Symbols:");
            for (symbol, p) in elf.symbols() {
                println!(
                    " > {symbol} {}",
                    match p {
                        Pointee::Address(addr) => format!("-> #{:X}", addr),
                        Pointee::Symbol(s) => format!("-> {}", s),
                        Pointee::None => String::new(),
                    }
                )
            }
            println!("Relocations:");
            for (sect, addr, symbol) in elf.relocations() {
                println!(" > {symbol} @ {sect}/{addr:X}",)
            }
            println!("Segments:");
            for (j, (start_addr, p)) in elf.data().iter().enumerate() {
                println!(" > {j}: {} bytes @ #{start_addr:X}", p.len());
                if args.segments.contains(&(i, j)) {
                    hexdump(p)
                }
            }
        } else {
            println!("File doesn't exist")
        }
        println!();
    }
}
