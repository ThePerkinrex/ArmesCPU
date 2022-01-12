// use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::take,
    character::complete::char,
    character::complete::{digit1, hex_digit1, one_of},
    combinator::{all_consuming, cut, map, map_parser, map_res, recognize},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated},
};

use crate::error::{context, Context};

use super::{super::from_str_radix::FromStrRadix, tag, take_all, PResult, Span};

fn binary<N: FromStrRadix>(input: Span) -> PResult<N> {
    context(
        Context::Bin,
        preceded(
            alt((tag("0b"), tag("0B"))),
            cut(map_parser(
                recognize(many1(terminated(one_of("01"), many0(char('_'))))),
                map_res(take_all, |x: Span| {
                    N::from_str_radix(x.fragment(), 2).map(|n| x.map(|_| n))
                }),
            )),
        ),
    )(input)
}

fn hex<N: FromStrRadix>(input: Span) -> PResult<N> {
    context(
        Context::Hex,
        preceded(
            alt((tag("0x"), tag("0X"))),
            cut(map_parser(
                recognize(many1(terminated(hex_digit1, many0(char('_'))))),
                map_res(take_all, |x: Span| {
                    N::from_str_radix(x.fragment(), 16).map(|n| x.map(|_| n))
                }),
            )),
        ),
    )(input)
}

fn decimal<N: FromStrRadix>(input: Span) -> PResult<N> {
    context(
        Context::Dec,
        map_parser(
            recognize(many1(terminated(digit1, many0(char('_'))))),
            cut(map_res(take_all, |x: Span| {
                N::from_str_radix(x.fragment(), 10).map(|n| x.map(|_| n))
            })),
        ),
    )(input)
}

fn number<N: FromStrRadix>(n: Span) -> PResult<N> {
    alt((binary, hex, decimal))(n)
}

fn constant_byte(i: Span) -> PResult<u8> {
    context(Context::Byte, preceded(char('$'), number))(i)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Addr {
    Pointer,
    Addr(u16),
}

fn addr(i: Span) -> PResult<Addr> {
    context(
        Context::Addr,
        delimited(
            char('['),
            cut(alt((
                map(number, |s: Span<u16>| s.map(Addr::Addr)),
                map(tag("I"), |x: Span| x.map(|_| Addr::Pointer)),
            ))),
            char(']'),
        ),
    )(i)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstantAddr {
    Pointer,
    Addr(u16),
}

fn constant_addr(i: Span) -> PResult<ConstantAddr> {
    context(
        Context::ConstantAddr,
        alt((
            preceded(
                char('#'),
                cut(alt(
                    (map(number, |s: Span<u16>| s.map(ConstantAddr::Addr)),),
                )),
            ),
            map(tag("I"), |x: Span| x.map(|_| ConstantAddr::Pointer)),
        )),
    )(i)
}

fn register(i: Span) -> PResult<u8> {
    context(
        Context::Register,
        preceded(
            char('V'),
            cut(map(
                map_parser(take(1usize), all_consuming(hex_digit1)),
                |x: Span| x.map(|_| u8::from_str_radix(x.fragment(), 16).unwrap()),
            )),
        ),
    )(i)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arg {
    Addr(Addr),
    Byte(u8),
    Register(u8),
    ConstantAddr(ConstantAddr),
}

pub fn arg(i: Span) -> PResult<Arg> {
    context(
        Context::Arg,
        alt((
            map(constant_byte, |x| x.map(Arg::Byte)),
            map(addr, |x| x.map(Arg::Addr)),
            map(register, |x| x.map(Arg::Register)),
            map(register, |x| x.map(Arg::Register)),
            map(constant_addr, |x| x.map(Arg::ConstantAddr)),
        )),
    )(i)
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::{assert_nom_err, assert_nom_failure, assert_nom_ok_extra};

    use super::{
        addr, arg, constant_addr, constant_byte, number, register, Addr, Arg, ConstantAddr, Span,
    };

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
    fn parse_constant_addr() {
        assert_nom_err(constant_addr, Span::new("$10"));
        assert_nom_ok_extra(constant_addr, Span::new("#10"), "", ConstantAddr::Addr(10));
        assert_nom_ok_extra(constant_addr, Span::new("I"), "", ConstantAddr::Pointer);
        assert_nom_failure(constant_addr, Span::new("#hi"));
    }

    #[test]
    fn parse_register() {
        assert_nom_err(register, Span::new("$10"));
        assert_nom_err(register, Span::new("[10]"));
        assert_nom_ok_extra(register, Span::new("V0"), "", 0);
        assert_nom_ok_extra(register, Span::new("VF"), "", 0xF);
        assert_nom_ok_extra(register, Span::new("VFF"), "F", 0xF);
        assert_nom_failure(register, Span::new("VG"));
        assert_nom_failure(register, Span::new("V"));
        // panic!()
    }

    #[test]
    fn parse_arg() {
        assert_nom_ok_extra(arg, Span::new("$10"), "", Arg::Byte(10));
        assert_nom_ok_extra(arg, Span::new("VA"), "", Arg::Register(10));
        assert_nom_ok_extra(arg, Span::new("[10]"), "", Arg::Addr(Addr::Addr(10)));
        assert_nom_ok_extra(arg, Span::new("[I]AA"), "AA", Arg::Addr(Addr::Pointer));
        assert_nom_ok_extra(
            arg,
            Span::new("#10"),
            "",
            Arg::ConstantAddr(ConstantAddr::Addr(10)),
        );
        assert_nom_ok_extra(
            arg,
            Span::new("IAA"),
            "AA",
            Arg::ConstantAddr(ConstantAddr::Pointer),
        );
        assert_nom_err(arg, Span::new("AAAAAAAAAAAAAAAA"));
        // panic!()
    }
}
