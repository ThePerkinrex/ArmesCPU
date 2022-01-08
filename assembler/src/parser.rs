use std::{num::ParseIntError, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till},
    character::complete::char,
    character::complete::{digit1, hex_digit0, hex_digit1, one_of},
    combinator::{all_consuming, cut, map, map_parser, map_res, recognize, value},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;

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

type Span<T> = LocatedSpan<T>;
type PResult<T, O> = IResult<Span<T>, Span<O>>;

fn binary<N: FromStrRadix>(input: Span<&str>) -> PResult<&str, N> {
    let (r, (_, n)) = tuple((
        alt((tag("0b"), tag("0B"))),
        cut(context(
            N::NAME,
            map_res(
                recognize(many1(terminated(one_of("01"), many0(char('_'))))),
                |x: Span<&str>| N::from_str_radix(x.fragment(), 2).map(|n| x.map(|_| n)),
            ),
        )),
    ))(input)?;
    Ok((r, n))
}

fn hex<N: FromStrRadix>(input: Span<&str>) -> PResult<&str, N> {
    let (r, (_, n)) = tuple((
        alt((tag("0x"), tag("0X"))),
        cut(context(
            N::NAME,
            map_res(
                recognize(many1(terminated(hex_digit1, many0(char('_'))))),
                |x: Span<&str>| N::from_str_radix(x.fragment(), 16).map(|n| x.map(|_| n)),
            ),
        )),
    ))(input)?;
    Ok((r, n))
}

fn decimal<N: FromStrRadix>(input: Span<&str>) -> PResult<&str, N> {
    context(
        N::NAME,
        map_res_fail(
            recognize(many1(terminated(digit1, many0(char('_'))))),
            |x: Span<&str>| N::from_str(x.fragment()).map(|n| x.map(|_| n)),
        ),
    )(input)
}

fn number<N: FromStrRadix>(n: Span<&str>) -> PResult<&str, N> {
    alt((binary, hex, decimal))(n)
}

fn constant_byte(i: Span<&str>) -> PResult<&str, u8> {
    let (r, (_, n)) = tuple((tag("$"), number))(i)?;
    Ok((r, n))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Addr {
    Pointer,
    Addr(u16),
}

fn addr(i: Span<&str>) -> PResult<&str, Addr> {
    delimited(
        char('['),
        cut(context(
            "address (2 bytes)",
            alt((
                map(number, |s: Span<u16>| s.map(Addr::Addr)),
                map(tag("I"), |x: Span<&str>| x.map(|_| Addr::Pointer)),
            )),
        )),
        char(']'),
    )(i)
}

fn register(i: Span<&str>) -> PResult<&str, u8> {
    preceded(
        tag("V"),
        map(
            map_parser(take(1usize), all_consuming(hex_digit1)),
            |x: Span<&str>| x.map(|x| u8::from_str_radix(x, 16).unwrap()),
        ),
    )(i)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arg {
    Addr(Addr),
    Byte(u8),
    Register(u8),
}

fn arg(i: Span<&str>) -> PResult<&str, Arg> {
    alt((
        map(constant_byte, |x| x.map(Arg::Byte)),
        map(addr, |x| x.map(Arg::Addr)),
        map(register, |x| x.map(Arg::Register)),
    ))(i)
}

#[cfg(test)]
mod tests {
    use std::fmt::{Debug, Display};

    use nom::error::Error;

    use crate::parser::{addr, constant_byte, PResult, Span};

    use super::{number, register, Addr, Arg, arg};

    fn assert_nom_err<T: Display, O: PartialEq + Debug, F: Fn(Span<T>) -> PResult<T, O>>(
        f: F,
        i: Span<T>,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(Error { input, code })) => println!(
                "Fail at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.location_offset(),
                input.fragment()
            ),
            Err(nom::Err::Error(Error { input, code })) => println!(
                "Error at {:?} {}:{}:\n\t{}",
                code,
                input.location_line(),
                input.location_offset(),
                input.fragment()
            ),
            _ => (),
        };
        assert!(matches!(r, Err(nom::Err::Error(_))));
    }

    fn assert_nom_failure<T: Display, O: PartialEq + Debug, F: Fn(Span<T>) -> PResult<T, O>>(
        f: F,
        i: Span<T>,
    ) {
        let r = f(i);
        match &r {
            Err(nom::Err::Failure(e)) => println!("Fail:\n{}", e),
            Err(nom::Err::Error(e)) => println!("Error:\n{}", e),
            _ => (),
        };
        assert!(matches!(r, Err(nom::Err::Failure(_))));
    }

    fn assert_nom_ok<
        T: Display + Debug + PartialEq,
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
            Err(nom::Err::Failure(e)) => println!("Fail:\n{}", e),
            Err(nom::Err::Error(e)) => println!("Error:\n{}", e),
            _ => (),
        };
        assert!(r.is_ok());
        let (rest, c) = r.unwrap();
        assert_eq!(rest.fragment(), &res);
        assert_eq!(c.fragment(), &v);
    }

    #[test]
    fn parse_num() {
        assert_nom_ok(number::<u8>, Span::new("10"), "", 10);
        assert_nom_failure(number::<u8>, Span::new("256"));
        assert_nom_ok(number::<u8>, Span::new("0x10"), "", 0x10);
        assert_nom_ok(number::<u8>, Span::new("0xFF"), "", 0xFF);
        assert_nom_failure(number::<u8>, Span::new("0x100"));
        assert_nom_ok(number::<u16>, Span::new("0x100"), "", 0x100);
        assert_nom_ok(number::<u8>, Span::new("0X10"), "", 0x10);
        assert_nom_ok(number::<u8>, Span::new("0b10"), "", 0b10);
        assert_nom_failure(number::<u8>, Span::new("0b100000000"));
        assert_nom_ok(number::<u8>, Span::new("0B10"), "", 0b10);
        assert_nom_err(number::<u8>, Span::new("AAAAAA"));
        // panic!()
    }

    #[test]
    fn parse_constant_byte() {
        assert_nom_ok(constant_byte, Span::new("$10"), "", 10);
        assert_nom_ok(constant_byte, Span::new("$0x10 hi"), " hi", 0x10);
        assert_nom_err(constant_byte, Span::new("0b10"));
        assert_nom_err(constant_byte, Span::new("[0b10]"));
    }

    #[test]
    fn parse_addr() {
        assert_nom_err(addr, Span::new("$10"));
        assert_nom_ok(addr, Span::new("[10]"), "", Addr::Addr(10));
        assert_nom_ok(addr, Span::new("[I]"), "", Addr::Pointer);
        assert_nom_failure(addr, Span::new("[hi]"));
    }

    #[test]
    fn parse_register() {
        assert_nom_err(register, Span::new("$10"));
        assert_nom_err(register, Span::new("[10]"));
        assert_nom_ok(register, Span::new("V0"), "", 0);
        assert_nom_ok(register, Span::new("VF"), "", 0xF);
        assert_nom_ok(register, Span::new("VFF"), "F", 0xF);
        assert_nom_err(register, Span::new("VG"));
        assert_nom_err(register, Span::new("V"));
        // panic!()
    }

    #[test]
    fn parse_arg() {
        assert_nom_ok(arg, Span::new("$10"), "", Arg::Byte(10));
        assert_nom_ok(arg, Span::new("VA"), "", Arg::Register(10));
        assert_nom_ok(arg, Span::new("[10]"), "", Arg::Addr(Addr::Addr(10)));
        assert_nom_ok(arg, Span::new("[I]AA"), "AA", Arg::Addr(Addr::Pointer));
        assert_nom_err(arg, Span::new("AAAAAAAAAAAAAAAA"));
    }
}
