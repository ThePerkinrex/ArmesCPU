use ariadne::{Color, Label, Report, ReportKind, Source};
use assembler::parser::{lines, Span};

fn main() {
    let og = r#"
	LD V0, V1 ; V0 = V1
	CALL [0x1000]


	RET
	"#;
    let s = Span::new(og);

    let (_, r) = lines(s).unwrap();
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
