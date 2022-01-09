use ariadne::{Color, Label, Report, ReportKind};
use nom::IResult;
use nom_supreme::error::{ErrorTree, StackContext};
use parser::Span;

use crate::parser::OPCODES;

pub mod parser;
// pub mod error;

pub fn error_as_report(r: nom::Err<ErrorTree<Span>>) -> Report {
    let r = match r {
        nom::Err::Incomplete(e) => panic!("ICE: Input incomplete: {:?}", e),
        nom::Err::Error(e) => e,
        nom::Err::Failure(e) => e,
    };
    error_tree_as_report(r)
        .into_iter()
        .fold(
            Report::build(ReportKind::Error, (), 0).with_message("Syntax error"),
            |r, l| r.with_label(l.with_color(Color::Red)),
        )
        .finish()

    // println!("{}", r);
}

fn error_tree_as_report(r: ErrorTree<Span>) -> Vec<Label> {
    match r {
        ErrorTree::Base { location, kind } => {
            let off = location.location_offset();
            let frag = location.fragment();
            vec![Label::new(off..(off + frag.len())).with_message(format!("{}", kind))]
        }
        ErrorTree::Stack { base, contexts } => {
            // println!("ctx: {:#?}", contexts);
            // println!("base: {:?}", base);
            // contexts.into_iter().map(|(s, ctx)| match_context(s, ctx)).find(|x| x.is_some()).flatten().unwrap_or_else(|| error_tree_as_report(*base))
            error_tree_as_report(*base)
        }
        ErrorTree::Alt(a) => a.into_iter().flat_map(error_tree_as_report).collect(),
    }
}

fn match_context(s: Span, ctx: StackContext) -> Option<Report> {
    println!("{:?}", ctx);
    let off = s.location_offset();
    // let frag = s.fragment();
    let label = match ctx {
        StackContext::Context("eol") => {
            Label::new(off..(off + 1)).with_message("Expected EOL or EOF")
        }
        StackContext::Context("comment") => Label::new(off..(off + 1)).with_message("Expected ;"),
        StackContext::Context("opcode") => Label::new(off..(off + s.fragment().len()))
            .with_message(format!(
                "Expected opcode (one of {})",
                OPCODES
                    .iter()
                    .fold(None, |x, a| match x {
                        None => Some(format!("{:?}", a)),
                        Some(x) => Some(format!("{}, {:?}", x, a)),
                    })
                    .unwrap()
            )),
        _ => return None,
    }
    .with_color(Color::Red);
    Some(
        Report::build(ReportKind::Error, (), 34)
            .with_message("Syntax error")
            .with_label(label)
            // .with_label(Label::new(42..45).with_message("This is of type Str"))
            .finish(),
    )
}
