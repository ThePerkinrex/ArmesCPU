use std::process::exit;

// use asm_ir::Program;
use assembler::{
    cache::{FnCache, OnlyOne},
    parse_file, ParseErr,
};

// const OG: &str = "LD I, V0, #0x10 ; V0 = V1\nCALL #0x1000 ; AAAA\nRET";
const OTHER: &str = "
LD I, #0xF002
LD V0, $72 ; H
LD [I], V0
LD V0, $105 ; i
LD [I], V0
LD V0, $10 ; newline
LD [I], V0
JP #0x0004
";

fn main() {
    // for l in OG.lines() {
    //     println!("{}", l);
    // }
    let mut cache = FnCache::new(|_: &OnlyOne| Result::<_, ()>::Ok(OTHER.to_string()));
    let r = match parse_file(&OnlyOne, &mut cache) {
        Ok(v) => v,
        Err(ParseErr::Reports(r)) => {
            for r in r {
                r.print(&mut cache).unwrap()
            }
            exit(1)
        }
        Err(ParseErr::FileError(_)) => unreachable!(),
    };
    println!("Res {:?}", r);
    // let mut p = Program::default();
    // p.segments.push((0, r));
    // let mut f = File::create("hi.o").unwrap();
    // p.write(&mut f).unwrap();
    // f.flush().unwrap();

    // let s = Span::new(og);

    // let (_, r) = match lines(s) {
    //     Ok(x) => x,
    //     Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
    //         println!("{}", e);
    //         for e in error_as_reports(nom::Err::Error(e), ()) {
    //             e.print(Source::from(og)).unwrap();
    //         }
    //         exit(1)
    //     }
    //     Err(e) => {
    //         println!("{}", e);
    //         exit(1)
    //     }
    // };
    // for e in r {
    //     let off = e.location_offset();
    //     let frag = *e.fragment();
    //     let (i, args) = e.extra;
    //     println!("{} {{{:?}}}", i, args);

    //     Report::build(ReportKind::Error, (), 34)
    //         .with_message("Incompatible types")
    //         .with_label(
    //             Label::new(off..(off + frag.len()))
    //                 .with_message("This is of type Nat")
    //                 .with_color(Color::Yellow),
    //         )
    //         // .with_label(Label::new(42..45).with_message("This is of type Str"))
    //         .finish()
    //         .print(Source::from(og))
    //         .unwrap();
    //     // break;
    // }
}
