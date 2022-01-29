use nom::{
    branch::alt,
    bytes::complete::take,
    character::complete::{alpha1, alphanumeric1, char, space0, space1},
    combinator::{consumed, cut, eof, map, map_parser, not, opt, peek, recognize, success, value},
    error::ParseError,
    multi::{many0, separated_list0},
    sequence::{pair, preceded, terminated, tuple},
    Compare, CompareResult, IResult, InputLength, InputTake, InputTakeAtPosition, Parser,
};
use nom_locate::LocatedSpan;
// use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};

use crate::error::{context, BaseErrorKind, Context, Error, Expectation};

use self::arg::arg;

mod arg;

pub use arg::{Addr, Arg, ConstantAddr};

pub fn take_all<Input, Error: ParseError<Input>>(i: Input) -> IResult<Input, Input, Error>
where
    Input: InputTakeAtPosition,
{
    i.split_at_position_complete(|_| false)
}

pub type Span<'a, T = ()> = LocatedSpan<&'a str, T>;
pub type PResult<'a, O = ()> = IResult<Span<'a>, Span<'a, O>, Error<Span<'a>>>;
pub type Instr<'a> = (Span<'a>, Vec<Span<'a, Arg<'a>>>);

#[derive(Debug, Clone)]
pub enum Line<'a> {
    Instr(Instr<'a>),
    Label(Span<'a>),
}

struct Choice<I: Iterator>(I);
impl<I: Clone, O, E: ParseError<I>, T: Parser<I, O, E>, Iter: Iterator<Item = T>>
    nom::branch::Alt<I, O, E> for Choice<Iter>
{
    fn choice(&mut self, input: I) -> IResult<I, O, E> {
        let mut err = None;
        for mut p in &mut self.0 {
            match p.parse(input.clone()) {
                Err(nom::Err::Error(e)) => {
                    if err.is_none() {
                        err = Some(e)
                    } else {
                        err = err.map(|err| err.or(e))
                    }
                }
                r => return r,
            }
        }
        assert!(err.is_some(), "Empty iterator cannot opt for a choice");
        Err(nom::Err::Error(err.unwrap()))
    }
}

pub const OPCODES: &[&str] = &[
    "NOP", "LD", "RET", "CALL", "JP", "ADD", "SUB", "SUBN", "AND", "OR", "XOR", "SE", "SNE", "SHR",
    "SHL",
];

pub fn tag<Input>(tag: &'static str) -> impl Fn(Input) -> IResult<Input, Input, Error<Input>>
where
    Input: InputTake + Compare<&'static str>,
{
    move |i: Input| {
        let tag_len = tag.input_len();
        let res: IResult<_, _, Error<Input>> = match i.compare(tag) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            _ => Err(nom::Err::Error(Error::Base {
                input: i,
                kind: BaseErrorKind::Expected(Expectation::Tag(tag)),
            })),
        };
        res
    }
}

fn identifier(input: Span) -> PResult {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

pub fn instr(i: Span) -> PResult<Instr> {
    context(
        Context::Instr,
        map(
            consumed(tuple((
                map_parser(
                    identifier,
                    cut(context(
                        Context::Opcode,
                        alt(Choice(OPCODES.iter().map(|x: &&'static str| tag(*x)))),
                    )),
                ),
                alt((space1, success(Span::new("")))),
                separated_list0(|i2| tuple((char(','), space0))(i2), arg),
            ))),
            |(c, (s, _, v))| c.map_extra(|_| (s, v.clone())),
        ),
    )(i)
}

fn eol(i: Span) -> PResult {
    map(
        consumed(alt((
            value((), context(Context::Eol, pair(opt(char('\r')), char('\n')))),
            value((), eof),
        ))),
        |(x, _): (Span, _)| x,
    )(i)
}

pub fn comment<'a>(i: Span<'a>) -> PResult {
    context(
        Context::Comment,
        map(
            consumed(tuple((char(';'), |mut x: Span<'a>| {
                while peek(not(eol))(x).is_ok() {
                    let (rest, _) = take(1usize)(x)?;
                    // println!("REST {:?}", rest.fragment());
                    x = rest;
                }
                Ok((x, ()))
            }))),
            |(x, _): (Span, _)| x,
        ),
    )(i)
}

fn label(i: Span) -> PResult {
    terminated(identifier, char(':'))(i)
}

pub fn line(i: Span) -> IResult<Span, Option<Span<Line>>, Error<Span>> {
    context(
        Context::Line,
        alt((
            map(tuple((space0, comment)), |_| None),
            map(preceded(space0, label), |l| {
                Some(l.map_extra(|_| Line::Label(l)))
            }),
            map(
                tuple((space0, opt(instr), opt(pair(space0, comment)))),
                |(_, x, _)| x.map(|x| x.map_extra(Line::Instr)),
            ),
        )),
    )(i)
}

pub fn lines(i: Span) -> IResult<Span, Vec<Span<Line>>, Error<Span>> {
    let mut v = Vec::new();
    let (mut rest, (_, mut l)) = consumed(terminated(line, eol))(i)?;
    loop {
        // println!("Consumed: {:?}; Line: {:?}", con, l);
        if let Some(x) = &l {
            v.push(x.clone());
        }
        match consumed(terminated(line, eol))(rest) {
            Ok((nrest, (ncon, nl))) => {
                rest = nrest;
                // con = ncon;
                l = nl;
                if ncon.is_empty() {
                    break;
                }
            }
            e => {
                // println!("Error: {:?}", e);
                e?;
            }
        };
    }
    Ok((rest, v))
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use nom::IResult;

    use crate::error::Error;

    use super::{arg::Arg, comment, instr, line, ConstantAddr, Line, PResult, Span};

    pub fn assert_nom_err<'a, O: PartialEq + Debug, F: Fn(Span<'a>) -> PResult<'a, O>>(
        f: F,
        i: Span<'a>,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(e)) => println!("Fail {}", e),
            Err(nom::Err::Error(e)) => println!("Error {}", e),
            _ => (),
        };
        assert!(matches!(r, Err(nom::Err::Error(_))));
    }

    pub fn assert_nom_failure<'a, O: PartialEq + Debug, F: Fn(Span<'a>) -> PResult<'a, O>>(
        f: F,
        i: Span<'a>,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(e)) => println!("Fail {}", e),
            Err(nom::Err::Error(e)) => println!("Error {}", e),
            _ => (),
        };
        assert!(matches!(r, Err(nom::Err::Failure(_))));
    }

    pub fn assert_nom_ok<'a, O: PartialEq + Debug, F: Fn(Span<'a>) -> PResult<'a, O>>(
        f: F,
        i: Span<'a>,
        res: &str,
        frag: Option<&str>,
        v: Option<O>,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(e)) => println!("Fail {}", e),
            Err(nom::Err::Error(e)) => println!("Error {}", e),
            _ => (),
        };
        assert!(r.is_ok());
        let (rest, c) = r.unwrap();
        assert_eq!(rest.fragment(), &res);
        if let Some(v) = v {
            assert_eq!(c.extra, v);
        }
        if let Some(frag) = frag {
            assert_eq!(c.fragment(), &frag);
        }
    }

    pub fn assert_nom_ok_extra<'a, O: PartialEq + Debug, F: Fn(Span<'a>) -> PResult<'a, O>>(
        f: F,
        i: Span<'a>,
        res: &str,
        v: O,
    ) {
        assert_nom_ok(f, i, res, None, Some(v))
    }

    pub fn assert_nom_ok_fragment<'a, O: PartialEq + Debug, F: Fn(Span<'a>) -> PResult<'a, O>>(
        f: F,
        i: Span<'a>,
        res: &str,
        v: &str,
    ) {
        assert_nom_ok(f, i, res, Some(v), None)
    }

    pub fn assert_nom_ok_generic<
        'a,
        O: Debug,
        F: Fn(Span<'a>) -> IResult<Span<'a>, O, Error<Span<'a>>>,
        C: Fn(O) -> bool,
    >(
        f: F,
        i: Span<'a>,
        res: &str,
        check: C,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(e)) => println!("Fail {}", e),
            Err(nom::Err::Error(e)) => println!("Error {}", e),
            _ => (),
        };
        println!("Should be ok: {:?}", r);
        assert!(r.is_ok());
        let (rest, c) = r.unwrap();
        assert_eq!(rest.fragment(), &res);
        assert!(check(c));
    }

    #[test]
    fn parse_instr() {
        assert_nom_ok_generic(instr, Span::new("LD V0, V1"), "", |x| {
            let (code, args) = x.extra;
            code.fragment() == &"LD"
                && args
                    .iter()
                    .zip([Arg::Register(0), Arg::Register(1)].iter())
                    .all(|(a, b)| &a.extra == b)
        });

        assert_nom_ok_generic(
            instr,
            Span::new("LD V0, V1 AAAAAAAAAAAAA"),
            " AAAAAAAAAAAAA",
            |x| {
                let (code, args) = x.extra;
                code.fragment() == &"LD"
                    && args
                        .iter()
                        .zip([Arg::Register(0), Arg::Register(1)].iter())
                        .all(|(a, b)| &a.extra == b)
            },
        );

        assert_nom_ok_generic(instr, Span::new("RET A"), "", |x| {
            let (code, args) = x.extra;
            code.fragment() == &"RET"
                && args
                    .iter()
                    .zip([Arg::ConstantAddr(ConstantAddr::Symbol("A"))].iter())
                    .all(|(a, b)| &a.extra == b)
        });

        assert_nom_ok_generic(instr, Span::new("RET\n\rA"), "\n\rA", |x| {
            let (code, args) = x.extra;
            code.fragment() == &"RET" && args.is_empty()
        });
    }

    #[test]
    fn parse_comment() {
        assert_nom_ok_fragment(comment, Span::new("; jkjjojojojo"), "", "; jkjjojojojo");
        assert_nom_ok_fragment(
            comment,
            Span::new("; jkjjojojojo\n\rHey"),
            "\n\rHey",
            "; jkjjojojojo",
        );
        assert_nom_ok_fragment(
            comment,
            Span::new("; jkjjojojojo\nHey"),
            "\nHey",
            "; jkjjojojojo",
        );
        assert_nom_err(comment, Span::new("RET ; Return"));
    }

    #[test]
    fn parse_line() {
        assert_nom_ok_generic(line, Span::new("; jkjjojojojo"), "", |x| x.is_none());
        assert_nom_ok_generic(line, Span::new("; jkjjojojojo\n\rHey"), "\n\rHey", |x| {
            x.is_none()
        });
        assert_nom_ok_generic(line, Span::new("; jkjjojojojo\nHey"), "\nHey", |x| {
            x.is_none()
        });
        assert_nom_ok_generic(line, Span::new("RET ; Return"), "", |x| {
            x.is_some() && {
                let x = x.unwrap();
                if let Line::Instr((a, b)) = x.extra {
                    a.fragment() == &"RET" && b.is_empty()
                } else {
                    false
                }
            }
        });
        assert_nom_ok_generic(line, Span::new("RET ; Return\nRET"), "\nRET", |x| {
            x.is_some() && {
                let x = x.unwrap();
                if let Line::Instr((a, b)) = x.extra {
                    a.fragment() == &"RET" && b.is_empty()
                } else {
                    false
                }
            }
        });
    }
}
