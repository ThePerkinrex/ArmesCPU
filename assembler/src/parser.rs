use std::{num::ParseIntError, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till1, take_while},
    character::{
        complete::{char, newline, not_line_ending, one_of, space0, space1},
        is_newline, is_space,
    },
    combinator::{cond, consumed, map, opt, success, value},
    error::ParseError,
    multi::{many0, separated_list0},
    sequence::{pair, tuple},
    IResult,
};
use nom_locate::LocatedSpan;

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
    const NAME: &'static str;
}

impl FromStrRadix for u8 {
    fn from_str_radix(x: &str, radix: u32) -> Result<Self, ParseIntError> {
        Self::from_str_radix(x, radix)
    }

    const NAME: &'static str = "byte";
}

impl FromStrRadix for u16 {
    fn from_str_radix(x: &str, radix: u32) -> Result<Self, ParseIntError> {
        Self::from_str_radix(x, radix)
    }

    const NAME: &'static str = "word";
}

pub type Span<T> = LocatedSpan<T>;
type PResult<T, O> = IResult<Span<T>, Span<O>>;
pub type Instr<'a> = (Span<&'a str>, Vec<Span<Arg>>);

pub fn instr(i: Span<&str>) -> PResult<&str, Instr> {
    map(
        consumed(tuple((
            take_till1(|x| is_space(x as u8) || is_newline(x as u8)),
            alt((space1, success(Span::new("")))),
            separated_list0(tuple((char(','), opt(space0))), arg),
        ))),
        |(c, (s, _, v))| c.map(|_| (s, v.clone())),
    )(i)
}

fn eol(i: Span<&str>) -> PResult<&str, &str> {
    map(consumed(many0(one_of("\n\r"))), |(x, _): (Span<&str>, _)| x)(i)
}

pub fn comment(i: Span<&str>) -> PResult<&str, &str> {
    map(
        consumed(tuple((char(';'), not_line_ending, eol))),
        |(x, _): (Span<&str>, _)| x,
    )(i)
}

pub fn line(i: Span<&str>) -> IResult<Span<&str>, Option<Span<Instr>>> {
    alt((
        map(tuple((space0, comment, eol)), |_| None),
        map(pair(opt(instr), alt((comment, eol))), |(x, _)| x),
    ))(i)
}

#[cfg(test)]
mod tests {
    use std::fmt::{Debug, Display};

    use nom::{error::Error, AsBytes, IResult};

    use super::{arg::Arg, comment, instr, line, PResult, Span};

    pub fn assert_nom_err<
        T: Display + AsBytes,
        O: PartialEq + Debug,
        F: Fn(Span<T>) -> PResult<T, O>,
    >(
        f: F,
        i: Span<T>,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(Error { input, code })) => println!(
                "Fail at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.get_utf8_column(),
                input.fragment()
            ),
            Err(nom::Err::Error(Error { input, code })) => println!(
                "Error at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.get_utf8_column(),
                input.fragment()
            ),
            _ => (),
        };
        assert!(matches!(r, Err(nom::Err::Error(_))));
    }

    pub fn assert_nom_failure<
        T: Display + AsBytes,
        O: PartialEq + Debug,
        F: Fn(Span<T>) -> PResult<T, O>,
    >(
        f: F,
        i: Span<T>,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(Error { input, code })) => println!(
                "Fail at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.get_utf8_column(),
                input.fragment()
            ),
            Err(nom::Err::Error(Error { input, code })) => println!(
                "Error at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.get_utf8_column(),
                input.fragment()
            ),
            _ => (),
        };
        assert!(matches!(r, Err(nom::Err::Failure(_))));
    }

    pub fn assert_nom_ok<
        T: Display + Debug + PartialEq + AsBytes,
        O: PartialEq + Debug,
        F: Fn(Span<T>) -> PResult<T, O>,
    >(
        f: F,
        i: Span<T>,
        res: T,
        v: O,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(Error { input, code })) => println!(
                "Fail at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.get_utf8_column(),
                input.fragment()
            ),
            Err(nom::Err::Error(Error { input, code })) => println!(
                "Error at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.get_utf8_column(),
                input.fragment()
            ),
            _ => (),
        };
        assert!(r.is_ok());
        let (rest, c) = r.unwrap();
        assert_eq!(rest.fragment(), &res);
        assert_eq!(c.fragment(), &v);
    }

    pub fn assert_nom_ok_generic<
        T: Display + Debug + PartialEq + AsBytes,
        O: Debug,
        F: Fn(Span<T>) -> IResult<Span<T>, O>,
        C: Fn(O) -> bool,
    >(
        f: F,
        i: Span<T>,
        res: T,
        check: C,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(Error { input, code })) => println!(
                "Fail at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.get_utf8_column(),
                input.fragment()
            ),
            Err(nom::Err::Error(Error { input, code })) => println!(
                "Error at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.get_utf8_column(),
                input.fragment()
            ),
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
            let (code, args) = x.fragment();
            code.fragment() == &"LD"
                && args
                    .iter()
                    .zip([Arg::Register(0), Arg::Register(1)].iter())
                    .all(|(a, b)| a.fragment() == b)
        });

        assert_nom_ok_generic(
            instr,
            Span::new("LD V0, V1 AAAAAAAAAAAAA"),
            " AAAAAAAAAAAAA",
            |x| {
                let (code, args) = x.fragment();
                code.fragment() == &"LD"
                    && args
                        .iter()
                        .zip([Arg::Register(0), Arg::Register(1)].iter())
                        .all(|(a, b)| a.fragment() == b)
            },
        );

        assert_nom_ok_generic(instr, Span::new("RET A"), "A", |x| {
            let (code, args) = x.fragment();
            code.fragment() == &"RET" && args.is_empty()
        });

        assert_nom_ok_generic(instr, Span::new("RET\n\rA"), "\n\rA", |x| {
            let (code, args) = x.fragment();
            code.fragment() == &"RET" && args.is_empty()
        });
    }

    #[test]
    fn parse_comment() {
        assert_nom_ok(comment, Span::new("; jkjjojojojo"), "", "; jkjjojojojo");
        assert_nom_ok(
            comment,
            Span::new("; jkjjojojojo\n\rHey"),
            "Hey",
            "; jkjjojojojo\n\r",
        );
        assert_nom_ok(
            comment,
            Span::new("; jkjjojojojo\nHey"),
            "Hey",
            "; jkjjojojojo\n",
        );
        assert_nom_err(comment, Span::new("RET ; Return"));
    }

    #[test]
    fn parse_line() {
        assert_nom_ok_generic(line, Span::new("; jkjjojojojo"), "", |x| x.is_none());
        assert_nom_ok_generic(line, Span::new("; jkjjojojojo\n\rHey"), "Hey", |x| {
            x.is_none()
        });
        assert_nom_ok_generic(line, Span::new("; jkjjojojojo\nHey"), "Hey", |x| {
            x.is_none()
        });
        assert_nom_ok_generic(line, Span::new("RET ; Return"), "", |x| {
            x.is_some() && {
                let x = x.unwrap();
                let (a, b) = x.fragment();
                a.fragment() == &"RET" && b.is_empty()
            }
        });
        assert_nom_ok_generic(line, Span::new("RET ; Return\nRET"), "RET", |x| {
            x.is_some() && {
                let x = x.unwrap();
                let (a, b) = x.fragment();
                a.fragment() == &"RET" && b.is_empty()
            }
        });
    }
}
