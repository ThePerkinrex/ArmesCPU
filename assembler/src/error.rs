use std::{fmt::Display, hash::Hash, ops::Range};

use nom::{
    error::{ErrorKind, FromExternalError, ParseError},
    InputLength,
};

use ariadne::{Color, Label, Report};

use crate::{from_str_radix::FromStrRadixError, parser::Span};

#[derive(Debug)]
pub enum Expectation {
    /// Expected tag
    Tag(&'static str),
    /// Expected character
    Char(char),
    /// Expected end of file
    Eof,
    /// Unexpected end of file
    NotEof,
    /// An end of line (`\r\n` or `\n`) was expected
    Eol,
    /// An ASCII letter (`[a-zA-Z]`) was expected.
    Alpha,

    /// A decimal digit (`[0-9]`) was expected.
    Digit,

    /// A hexadecimal digit (`[0-9a-fA-F]`) was expected.
    HexDigit,

    /// A binary digit (`[0-1]`) was expected.
    BinDigit,

    /// An octal digit (`[0-7]`) was expected.
    OctDigit,

    /// An alphanumeric character (`[0-9a-zA-Z]`) was expected.
    AlphaNumeric,

    /// A space or tab was expected.
    Space,

    /// A space, tab, newline, or carriage return was expected.
    Multispace,

    /// A hexadecimal number in that range was expected
    HexNumber(&'static str),

    /// A binary number in that range was expected
    BinNumber(&'static str),

    /// A decimal number in that range was expected
    DecNumber(&'static str),
}

impl Display for Expectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expectation::Tag(t) => write!(f, "expected {:?}", t),
            Expectation::Char(c) => write!(f, "expected {:?}", c),
            Expectation::Eof => write!(f, "expected EOF"),
            Expectation::NotEof => write!(f, "unexpected EOF"),
            Expectation::Eol => write!(f, "expected EOL"),
            Expectation::Alpha => write!(f, "expected alphabetic ([a-zA-z]) character"),
            Expectation::Digit => write!(f, "expected digit [0-9] character"),
            Expectation::HexDigit => write!(f, "expected hex digit [0-9a-fA-F] character"),
            Expectation::BinDigit => write!(f, "expected binary digit [0-1] character"),
            Expectation::OctDigit => write!(f, "expected octal digit [0-7] character"),
            Expectation::AlphaNumeric => write!(f, "expected alphanumeric [0-9a-zA-Z] character"),
            Expectation::Space => write!(f, "expected space or tab"),
            Expectation::Multispace => {
                write!(f, "expected space, tab, newline, or carriage return")
            }
            Expectation::HexNumber(n) => write!(f, "expected hex number in range for {}", n),
            Expectation::BinNumber(n) => write!(f, "expected binary number in range for {}", n),
            Expectation::DecNumber(n) => write!(f, "expected decimal number in range for {}", n),
        }
    }
}

#[derive(Debug)]
pub enum BaseErrorKind {
    NomError(ErrorKind),
    Expected(Expectation),
}

impl BaseErrorKind {
    fn from<I: InputLength>(input: &I, kind: ErrorKind) -> Self {
        match kind {
            ErrorKind::Alpha => BaseErrorKind::Expected(Expectation::Alpha),
            ErrorKind::Digit => BaseErrorKind::Expected(Expectation::Digit),
            ErrorKind::HexDigit => BaseErrorKind::Expected(Expectation::HexDigit),
            ErrorKind::OctDigit => BaseErrorKind::Expected(Expectation::OctDigit),
            ErrorKind::AlphaNumeric => BaseErrorKind::Expected(Expectation::AlphaNumeric),
            ErrorKind::Space => BaseErrorKind::Expected(Expectation::Space),
            ErrorKind::MultiSpace => BaseErrorKind::Expected(Expectation::Multispace),
            ErrorKind::CrLf => BaseErrorKind::Expected(Expectation::Eol),

            // Problem: ErrorKind::Eof is used interchangeably by various nom
            // parsers to mean either "expected Eof" or "expected NOT eof". See
            // https://github.com/Geal/nom/issues/1259. For now, we examine the
            // input string to guess what the likely intention is.
            ErrorKind::Eof => match input.input_len() {
                // The input is at Eof, which means that this refers to an
                // *unexpected* eof.
                0 => BaseErrorKind::Expected(Expectation::NotEof),

                // The input is *not* at eof, which means that this refers to
                // an *expected* eof.
                _ => BaseErrorKind::Expected(Expectation::Eof),
            },
            kind => BaseErrorKind::NomError(kind),
        }
    }
}

impl Display for BaseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseErrorKind::NomError(e) => write!(f, "{:?}", e),
            BaseErrorKind::Expected(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug)]
pub enum StackContext {
    Kind(BaseErrorKind),
    Ctx(Context),
}

impl Display for StackContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackContext::Kind(k) => write!(f, "{}", k),
            StackContext::Ctx(c) => write!(f, "{:?}", c),
        }
    }
}

#[derive(Debug)]
pub enum Error<I> {
    Base {
        kind: BaseErrorKind,
        input: I,
    },
    Stack {
        base: Box<Self>,
        stack: Vec<(I, StackContext)>,
    },
    Alt(Vec<Self>),
}

impl<I: InputLength> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        let kind = BaseErrorKind::from(&input, kind);

        Self::Base { input, kind }
    }

    fn append(input: I, kind: ErrorKind, other: Self) -> Self {
        let k = StackContext::Kind(BaseErrorKind::from(&input, kind));
        let context = (input, k);

        match other {
            // Don't create a stack of [ErrorKind::Alt, Self::Alt(..)]
            alt @ Self::Alt(..) if kind == ErrorKind::Alt => alt,

            // This is already a stack, so push on to it
            Self::Stack { mut stack, base } => Self::Stack {
                base,
                stack: {
                    stack.push(context);
                    stack
                },
            },

            // This isn't a stack; create a new stack
            base => Self::Stack {
                base: Box::new(base),
                stack: vec![context],
            },
        }
    }

    fn from_char(input: I, c: char) -> Self {
        if c == '\n' {
            return Self::Base {
                input,
                kind: BaseErrorKind::Expected(Expectation::Eol),
            };
        }
        let kind = BaseErrorKind::Expected(Expectation::Char(c));

        Self::Base { input, kind }
    }

    fn or(self, other: Self) -> Self {
        let siblings = match (self, other) {
            (Self::Alt(mut siblings1), Self::Alt(mut siblings2)) => {
                match siblings1.capacity() >= siblings2.capacity() {
                    true => {
                        siblings1.extend(siblings2);
                        siblings1
                    }
                    false => {
                        siblings2.extend(siblings1);
                        siblings2
                    }
                }
            }
            (Self::Alt(mut siblings), err) | (err, Self::Alt(mut siblings)) => {
                siblings.push(err);
                siblings
            }
            (err1, err2) => vec![err1, err2],
        };

        Self::Alt(siblings)
    }
}

impl<I> FromExternalError<I, FromStrRadixError> for Error<I> {
    fn from_external_error(input: I, kind: ErrorKind, e: FromStrRadixError) -> Self {
        eprintln!("FromStrRadixError: {} {}", e.radix(), e.name());
        let e = match e.radix() {
            2 => BaseErrorKind::Expected(Expectation::BinNumber(e.name())),
            16 => BaseErrorKind::Expected(Expectation::HexNumber(e.name())),
            10 => BaseErrorKind::Expected(Expectation::DecNumber(e.name())),
            _ => BaseErrorKind::NomError(kind),
        };
        Self::Base { input, kind: e }
    }
}

impl<I: std::fmt::Debug> Error<I> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, tabs: usize) -> std::fmt::Result {
        match self {
            Error::Base { kind, input } => {
                write!(f, "{0:\t>1$}{2} from {3:?}", "", &tabs, kind, input)
            }
            Error::Stack { base, stack } => {
                for (i, (input, ctx)) in stack.iter().rev().enumerate() {
                    writeln!(f, "{0:\t>1$}{2} from {3:?}", "", &(tabs + i), ctx, input)?;
                }
                base.print(f, tabs + stack.len())
            }
            Error::Alt(v) => {
                for e in v {
                    e.print(f, tabs)?;
                    writeln!(f)?;
                }
                Ok(())
            }
        }
    }
}

impl<I: std::fmt::Debug> Display for Error<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f, 0)
    }
}

pub trait ErrorContext<I> {
    fn add_context(input: I, ctx: Context, other: Self) -> Self;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Context {
    Instr,
    Comment,
    Eol,
    Line,
    Opcode,
    Arg,
    Register,
    Addr,
    ConstantAddr,
    Byte,
    Hex,
    Bin,
    Dec,
}

impl<I> ErrorContext<I> for Error<I> {
    fn add_context(input: I, ctx: Context, other: Self) -> Self {
        match ctx {
            Context::Eol => Self::Base {
                kind: BaseErrorKind::Expected(Expectation::Eol),
                input,
            },
            ctx => {
                let context = (input, StackContext::Ctx(ctx));
                match other {
                    // This is already a stack, so push on to it
                    Self::Stack { mut stack, base } => Self::Stack {
                        base,
                        stack: {
                            stack.push(context);
                            stack
                        },
                    },

                    // This isn't a stack, create a new stack
                    base => Self::Stack {
                        base: Box::new(base),
                        stack: vec![context],
                    },
                }
            }
        }
    }
}

pub fn context<I: Clone, E: ErrorContext<I>, F, O>(
    ctx: Context,
    mut f: F,
) -> impl FnMut(I) -> nom::IResult<I, O, E>
where
    F: nom::Parser<I, O, E>,
{
    move |i: I| match f.parse(i.clone()) {
        Ok(o) => Ok(o),
        Err(nom::Err::Incomplete(i)) => Err(nom::Err::Incomplete(i)),
        Err(nom::Err::Error(e)) => Err(nom::Err::Error(E::add_context(i, ctx, e))),
        Err(nom::Err::Failure(e)) => Err(nom::Err::Failure(E::add_context(i, ctx, e))),
    }
}

pub fn error_as_reports<Id: std::fmt::Debug + Hash + Eq + Clone>(
    r: nom::Err<Error<Span>>,
    i: &Id,
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
    src: &Id,
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
                src.clone(),
                input.location_offset()..(input.location_offset() + input.fragment().len()),
            ))
            .with_message(error_kind_as_message(kind))
            .with_color(Color::Red),
        )
        .finish()],
        Error::Stack { base, stack: _ } => error_tree_as_reports(*base, src),
        Error::Alt(c) => c
            .into_iter()
            .flat_map(|x| error_tree_as_reports(x, src))
            .collect(),
    }
}

fn error_kind_as_message(k: BaseErrorKind) -> String {
    match k {
        BaseErrorKind::NomError(e) => unreachable!("Unexpected nom error: {:?}", e),
        BaseErrorKind::Expected(e) => e.to_string(),
    }
}
