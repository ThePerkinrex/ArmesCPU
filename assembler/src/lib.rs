use std::{hash::Hash, ops::Range};

use ariadne::{Color, Label, Report};
use error::{BaseErrorKind, Error};
use parser::Span;

pub mod error;
mod from_str_radix;
pub mod parser;

pub fn error_as_reports<Id: std::fmt::Debug + Hash + Eq + Clone>(
    r: nom::Err<Error<Span>>,
    i: Id,
) -> Vec<Report<(Id, Range<usize>)>> {
    let r = match r {
        nom::Err::Incomplete(e) => panic!("ICE: Input incomplete: {:?}", e),
        nom::Err::Error(e) => e,
        nom::Err::Failure(e) => e,
    };
    error_tree_as_reports(r, i)

    // println!("{}", r);
}

fn error_tree_as_reports<Id: std::fmt::Debug + Hash + Eq + Clone>(
    e: Error<Span>,
    src: Id,
) -> Vec<Report<(Id, Range<usize>)>> {
    match e {
        // todo write a better impl
        Error::Base { kind, input } => vec![Report::build(
            ariadne::ReportKind::Error,
            src.clone(),
            input.location_offset(),
        )
        .with_message("Syntax error")
        .with_label(
            Label::new((
                src,
                input.location_offset()..(input.location_offset() + input.fragment().len()),
            ))
            .with_message(error_kind_as_message(kind))
            .with_color(Color::Red),
        )
        .finish()],
        Error::Stack { base, stack: _ } => error_tree_as_reports(*base, src),
        Error::Alt(c) => c
            .into_iter()
            .flat_map(|x| error_tree_as_reports(x, src.clone()))
            .collect(),
    }
}

fn error_kind_as_message(k: BaseErrorKind) -> String {
    match k {
        BaseErrorKind::NomError(e) => unreachable!("Unexpected nom error: {:?}", e),
        BaseErrorKind::Expected(e) => e.to_string(),
    }
}
