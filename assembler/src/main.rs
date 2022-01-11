use std::process::exit;

use ariadne::{Color, Label, Report, ReportKind, Source};
use assembler::{
    error_as_reports,
    parser::{lines, Span},
};

fn main() {
    let og = r#"
	LD V0, $0x10 ; V0 = V1
	CALL [0x1000] ; AAAA
NOTANOPCODE
	RET
	"#;
    let s = Span::new(og);

    let (_, r) = match lines(s) {
        Ok(x) => x,
        Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
            println!("{}", e);
            for e in error_as_reports(nom::Err::Error(e), ()) {
                e.print(Source::from(og)).unwrap();
            }
            exit(1)
        }
        Err(e) => {
            println!("{}", e);
            exit(1)
        }
    };
    for e in r {
        let off = e.location_offset();
        let frag = *e.fragment();
        let (i, args) = e.extra;
        println!("{} {{{:?}}}", i, args);

        Report::build(ReportKind::Error, (), 34)
            .with_message("Incompatible types")
            .with_label(
                Label::new(off..(off + frag.len()))
                    .with_message("This is of type Nat")
                    .with_color(Color::Yellow),
            )
            // .with_label(Label::new(42..45).with_message("This is of type Str"))
            .finish()
            .print(Source::from(og))
            .unwrap();
        // break;
    }
}
