use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::char,
    character::complete::{digit1, hex_digit1, one_of},
    combinator::{all_consuming, cut, map, map_parser, map_res, recognize},
    error::context,
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated, tuple},
};

use super::{map_res_fail, FromStrRadix, PResult, Span};

fn binary<N: FromStrRadix>(input: Span) -> PResult<N> {
    let (r, (_, n)) = tuple((
        context("binary_prefix", alt((tag("0b"), tag("0B")))),
        cut(
            context("binary", map_res(
                recognize(many1(terminated(one_of("01"), many0(char('_'))))),
                |x: Span| N::from_str_radix(x.fragment(), 2).map(|n| x.map(|_| n)),
            ),
        )),
    ))(input)?;
    Ok((r, n))
}

fn hex<N: FromStrRadix>(input: Span) -> PResult<N> {
    let (r, (_, n)) = tuple((
        context("hex_prefix", alt((tag("0x"), tag("0X")))),
        cut(context("hex", map_res(
            recognize(many1(terminated(hex_digit1, many0(char('_'))))),
            |x: Span| N::from_str_radix(x.fragment(), 16).map(|n| x.map(|_| n)),
        ))),
    ))(input)?;
    Ok((r, n))
}

fn decimal<N: FromStrRadix>(input: Span) -> PResult<N>
where
    <N as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    context("decimal", map_res_fail(
        recognize(many1(terminated(digit1, many0(char('_'))))),
        |x: Span| N::from_str(x.fragment()).map(|n| x.map(|_| n)),
    ))(input)
}

fn number<N: FromStrRadix>(n: Span) -> PResult<N>
where
    <N as FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    alt((binary, hex, decimal))(n)
}

fn constant_byte(i: Span) -> PResult<u8> {
    let (r, (_, n)) = context("constant", tuple((char('$'), number)))(i)?;
    Ok((r, n))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Addr {
    Pointer,
    Addr(u16),
}

fn addr(i: Span) -> PResult<Addr> {
    context("address", delimited(
        char('['),
        cut(context(
            "address number",
            alt((
                map(number, |s: Span<u16>| s.map(Addr::Addr)),
                map(context("pointer", tag("I")), |x: Span| x.map(|_| Addr::Pointer)),
            )),
        )),
        char(']'),
    ))(i)
}

fn register(i: Span) -> PResult<u8> {
    context("register", preceded(
        char('V'),
        map(
            map_parser(take(1usize), all_consuming(hex_digit1)),
            |x: Span| x.map(|_| u8::from_str_radix(x.fragment(), 16).unwrap()),
        ),
    ))(i)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arg {
    Addr(Addr),
    Byte(u8),
    Register(u8),
}

pub fn arg(i: Span) -> PResult<Arg> {
    context("arg", alt((
        map(constant_byte, |x| x.map(Arg::Byte)),
        map(addr, |x| x.map(Arg::Addr)),
        map(register, |x| x.map(Arg::Register)),
    )))(i)
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::{assert_nom_err, assert_nom_failure, assert_nom_ok_extra};

    use super::{addr, arg, constant_byte, number, register, Addr, Arg, Span};

    #[test]
    fn parse_num() {
        assert_nom_ok_extra(number::<u8>, Span::new("10"), "", 10);
        assert_nom_failure(number::<u8>, Span::new("256"));
        assert_nom_ok_extra(number::<u8>, Span::new("0x10"), "", 0x10);
        assert_nom_ok_extra(number::<u8>, Span::new("0xFF"), "", 0xFF);
        assert_nom_failure(number::<u8>, Span::new("0x100"));
        assert_nom_ok_extra(number::<u16>, Span::new("0x100"), "", 0x100);
        assert_nom_ok_extra(number::<u8>, Span::new("0X10"), "", 0x10);
        assert_nom_ok_extra(number::<u8>, Span::new("0b10"), "", 0b10);
        assert_nom_failure(number::<u8>, Span::new("0b100000000"));
        assert_nom_ok_extra(number::<u8>, Span::new("0B10"), "", 0b10);
        assert_nom_err(number::<u8>, Span::new("AAAAAA"));
        // panic!()
    }

    #[test]
    fn parse_constant_byte() {
        assert_nom_ok_extra(constant_byte, Span::new("$10"), "", 10);
        assert_nom_ok_extra(constant_byte, Span::new("$0x10 hi"), " hi", 0x10);
        assert_nom_err(constant_byte, Span::new("0b10"));
        assert_nom_err(constant_byte, Span::new("[0b10]"));
        // panic!()
    }

    #[test]
    fn parse_addr() {
        assert_nom_err(addr, Span::new("$10"));
        assert_nom_ok_extra(addr, Span::new("[10]"), "", Addr::Addr(10));
        assert_nom_ok_extra(addr, Span::new("[I]"), "", Addr::Pointer);
        assert_nom_failure(addr, Span::new("[hi]"));
    }

    #[test]
    fn parse_register() {
        assert_nom_err(register, Span::new("$10"));
        assert_nom_err(register, Span::new("[10]"));
        assert_nom_ok_extra(register, Span::new("V0"), "", 0);
        assert_nom_ok_extra(register, Span::new("VF"), "", 0xF);
        assert_nom_ok_extra(register, Span::new("VFF"), "F", 0xF);
        assert_nom_err(register, Span::new("VG"));
        assert_nom_err(register, Span::new("V"));
        // panic!()
    }

    #[test]
    fn parse_arg() {
        assert_nom_ok_extra(arg, Span::new("$10"), "", Arg::Byte(10));
        assert_nom_ok_extra(arg, Span::new("VA"), "", Arg::Register(10));
        assert_nom_ok_extra(arg, Span::new("[10]"), "", Arg::Addr(Addr::Addr(10)));
        assert_nom_ok_extra(arg, Span::new("[I]AA"), "AA", Arg::Addr(Addr::Pointer));
        assert_nom_err(arg, Span::new("AAAAAAAAAAAAAAAA"));
        // panic!()
    }
}
