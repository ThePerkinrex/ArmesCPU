use std::{num::ParseIntError, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::take_till1,
    character::{
        complete::{char, not_line_ending, space0, space1},
        is_newline, is_space,
    },
    combinator::{consumed, eof, map, opt, success, value},
    error::context,
    multi::separated_list0,
    sequence::{pair, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
use nom_supreme::error::ErrorTree;

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

trait FromStrRadix: Sized + FromStr + Copy {
    fn from_str_radix(x: &str, radix: u32) -> Result<Self, ParseIntError>;
}

impl FromStrRadix for u8 {
    fn from_str_radix(x: &str, radix: u32) -> Result<Self, ParseIntError> {
        Self::from_str_radix(x, radix)
    }
}

impl FromStrRadix for u16 {
    fn from_str_radix(x: &str, radix: u32) -> Result<Self, ParseIntError> {
        Self::from_str_radix(x, radix)
    }
}

pub type Span<'a, T = ()> = LocatedSpan<&'a str, T>;
type PResult<'a, O = ()> = IResult<Span<'a>, Span<'a, O>, ErrorTree<Span<'a>>>;
pub type Instr<'a> = (Span<'a>, Vec<Span<'a, Arg>>);

pub fn instr(i: Span) -> PResult<Instr> {
    context(
        "instr",
        map(
            consumed(tuple((
                context(
                    "opcode",
                    take_till1(|x| is_space(x as u8) || is_newline(x as u8)),
                ),
                alt((space1, success(Span::new("")))),
                separated_list0(tuple((char(','), opt(space0))), arg),
            ))),
            |(c, (s, _, v))| c.map(|_| (s, v.clone())),
        ),
    )(i)
}

fn eol(i: Span) -> PResult {
    context(
        "EOL",
        map(
            consumed(alt((
                value((), pair(opt(char('\r')), char('\n'))),
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

pub fn line(i: Span) -> IResult<Span, Option<Span<Instr>>, ErrorTree<Span>> {
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

pub fn lines(i: Span) -> IResult<Span, Vec<Span<Instr>>, ErrorTree<Span>> {
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
    use nom_supreme::error::ErrorTree;

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
        F: Fn(Span<'a>) -> IResult<Span<'a>, O, ErrorTree<Span<'a>>>,
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
