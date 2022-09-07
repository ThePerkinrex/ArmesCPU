use std::{fs::File, io::Write, process::exit};

use clap::Parser;

#[derive(Debug, Parser, Clone)]
struct Args {
    /// Available modifiers G, M, K (1024 * prev modifier)
    #[clap(parse(try_from_str = parse))]
    size: u64,
    /// HDD name
    name: String,
}

fn parse(s: &str) -> Result<u64, String> {
    if s.is_empty() {
        Err("argument is empty".into())
    } else {
        match s.chars().last().unwrap().to_ascii_lowercase() {
            x if x.is_ascii_digit() => Ok(s.parse::<u64>().map_err(|x| x.to_string())?),
            'g' => parse(&format!("{}m", &s[..s.len() - 1])).map(|x| x * 1024),
            'm' => parse(&format!("{}k", &s[..s.len() - 1])).map(|x| x * 1024),
            'k' => parse(&s[..s.len() - 1]).map(|x| x * 1024),
            x => Err(format!("{x:?} is not a valid size modifier")),
        }
    }
}

fn main() {
    let args = Args::parse();
    println!("HDD with name {} and size {} bytes", args.name, args.size);
    let mut f = match File::create(args.name) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Error creating HDD file:");
            eprintln!("\t{e}");
            exit(1);
        }
    };

    match f.write_all(&args.size.to_le_bytes()) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("Error writing HDD file:");
            eprintln!("\t{e}");
            exit(2);
        }
    };
}
