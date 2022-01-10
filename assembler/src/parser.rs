use std::{borrow::Borrow, fmt::Debug};

use nom::{
    branch::alt,
    character::complete::{alpha1, alphanumeric1, char, not_line_ending, space0, space1},
    combinator::{consumed, cut, eof, map, map_parser, opt, recognize, success, value},
    error::{ErrorKind, ParseError},
    multi::{many0, separated_list0},
    sequence::{pair, terminated, tuple},
    Compare, CompareResult, IResult, InputLength, InputTake, Parser,
};
use nom_locate::LocatedSpan;
// use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};

use crate::error::{context, BaseErrorKind, Error, Expectation};

use self::arg::{arg, Arg};

mod arg;

pub fn map_res_fail<I: Clone, O1, O2, E: nom::error::FromExternalError<I, E2>, E2, F, G>(
    mut parser: F,
    mut f: G,
) -> impl FnMut(I) -> IResult<I, O2, E>
where
    F: nom::Parser<I, O1, E>,
    G: FnMut(O1) -> Result<O2, E2>,
{
    move |input: I| {
        let i = input.clone();
        let (input, o1) = parser.parse(input)?;
        match f(o1) {
            Ok(o2) => Ok((input, o2)),
            Err(e) => Err(nom::Err::Failure(E::from_external_error(
                i,
                nom::error::ErrorKind::MapRes,
                e,
            ))),
        }
    }
}

pub fn verify_fail<I: Debug + Clone, O1, O2, E: ParseError<I>, F, G>(
    mut first: F,
    second: G,
) -> impl FnMut(I) -> IResult<I, O1, E>
where
    F: Parser<I, O1, E>,
    G: Fn(&O2) -> bool,
    O1: Borrow<O2> + Into<I> + Debug,
    O2: ?Sized,
{
    move |input: I| {
        //    let i = input.clone();
        let (input, o) = first.parse(input)?;
        //   println!("i: {:?} o: {:?}", i, o);

        if second(o.borrow()) {
            Ok((input, o))
        } else {
            Err(nom::Err::Failure(E::from_error_kind(
                o.into(),
                ErrorKind::Verify,
            )))
        }
    }
}

pub type Span<'a, T = ()> = LocatedSpan<&'a str, T>;
pub type PResult<'a, O = ()> = IResult<Span<'a>, Span<'a, O>, Error<Span<'a>>>;
pub type Instr<'a> = (Span<'a>, Vec<Span<'a, Arg>>);

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
    "NOP", "LD", "RET", "CALL", "JP", "ADD", "SUB", "SUBN", "OR", "XOR", "SE", "SNE", "SHR", "SHL",
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
        "instr",
        map(
            consumed(tuple((
                map_parser(
                    identifier,
                    cut(alt(Choice(OPCODES.iter().map(|x: &&'static str| tag(*x))))),
                ),
                alt((space1, success(Span::new("")))),
                separated_list0(
                    |i2| dbg!(tuple((char(','), space0))(dbg!(i2))),
                    |i| dbg!(arg(dbg!(i))),
                ),
            ))),
            |(c, (s, _, v))| c.map(|_| (s, v.clone())),
        ),
    )(i)
}

fn eol(i: Span) -> PResult {
    context(
        "eol",
        map(
            consumed(alt((
                value((), context("newline", pair(opt(char('\r')), char('\n')))),
                value((), eof),
            ))),
            |(x, _): (Span, _)| x,
        ),
    )(i)
}

pub fn comment(i: Span) -> PResult {
    context(
        "comment",
        map(
            consumed(tuple((char(';'), not_line_ending))),
            |(x, _): (Span, _)| x,
        ),
    )(i)
}

pub fn line(i: Span) -> IResult<Span, Option<Span<Instr>>, Error<Span>> {
    context(
        "line",
        alt((
            map(tuple((space0, comment)), |_| None),
            map(
                tuple((space0, opt(instr), opt(pair(space0, comment)))),
                |(_, x, _)| x,
            ),
        )),
    )(i)
}

pub fn lines(i: Span) -> IResult<Span, Vec<Span<Instr>>, Error<Span>> {
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

    use super::{arg::Arg, comment, instr, line, PResult, Span};

    pub fn assert_nom_err<O: PartialEq + Debug, F: Fn(Span) -> PResult<O>>(f: F, i: Span) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(e)) => println!("Fail {}", e),
            Err(nom::Err::Error(e)) => println!("Error {}", e),
            _ => (),
        };
        assert!(matches!(r, Err(nom::Err::Error(_))));
    }

    pub fn assert_nom_failure<O: PartialEq + Debug, F: Fn(Span) -> PResult<O>>(f: F, i: Span) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(e)) => println!("Fail {}", e),
            Err(nom::Err::Error(e)) => println!("Error {}", e),
            _ => (),
        };
        assert!(matches!(r, Err(nom::Err::Failure(_))));
    }

    pub fn assert_nom_ok<O: PartialEq + Debug, F: Fn(Span) -> PResult<O>>(
        f: F,
        i: Span,
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

    pub fn assert_nom_ok_extra<O: PartialEq + Debug, F: Fn(Span) -> PResult<O>>(
        f: F,
        i: Span,
        res: &str,
        v: O,
    ) {
        assert_nom_ok(f, i, res, None, Some(v))
    }

    pub fn assert_nom_ok_fragment<O: PartialEq + Debug, F: Fn(Span) -> PResult<O>>(
        f: F,
        i: Span,
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

        assert_nom_ok_generic(instr, Span::new("RET A"), "A", |x| {
            let (code, args) = x.extra;
            code.fragment() == &"RET" && args.is_empty()
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
                let (a, b) = x.extra;
                a.fragment() == &"RET" && b.is_empty()
            }
        });
        assert_nom_ok_generic(line, Span::new("RET ; Return\nRET"), "\nRET", |x| {
            x.is_some() && {
                let x = x.unwrap();
                let (a, b) = x.extra;
                a.fragment() == &"RET" && b.is_empty()
            }
        });
    }
}
