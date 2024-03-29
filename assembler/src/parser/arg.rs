// use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::take,
    character::complete::char,
    character::complete::{digit1, hex_digit1, one_of},
    combinator::{all_consuming, cut, map, map_parser, map_res, recognize},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated},
    Parser,
};

use crate::error::{context, Context};

use super::{
    super::from_str_radix::FromStrRadix, identifier, tag, take_all, Choice, PResult, Span,
    ESCAPED_CHARS,
};

fn binary<N: FromStrRadix>(input: Span) -> PResult<N> {
    context(
        Context::Bin,
        preceded(
            alt((tag("0b"), tag("0B"))),
            cut(map_parser(
                recognize(many1(terminated(one_of("01"), many0(char('_'))))),
                map_res(take_all, |x: Span| {
                    N::from_str_radix(x.fragment(), 2).map(|n| x.map_extra(|_| n))
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
                    N::from_str_radix(x.fragment(), 16).map(|n| x.map_extra(|_| n))
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
                N::from_str_radix(x.fragment(), 10).map(|n| x.map_extra(|_| n))
            })),
        ),
    )(input)
}

pub(super) fn char_literal(input: Span) -> PResult<char> {
    let escaped_chars = Choice::new_with_iter(
        ESCAPED_CHARS
            .iter()
            .map::<Box<dyn Parser<_, _, _>>, _>(|(a, b)| {
                Box::new(map(tag(a), |s: Span| s.map_extra(|_| *b)))
            })
            .chain(std::iter::once::<Box<dyn Parser<_, _, _>>>(Box::new(map(
                recognize(take(1usize)),
                |c: Span| c.map_extra(|_| c.fragment().chars().next().unwrap()),
            )))),
    );
    context(
        Context::Char,
        delimited(char('\''), alt(escaped_chars), char('\'')),
    )(input)
}

pub(super) fn number<N: FromStrRadix>(n: Span) -> PResult<N> {
    alt((binary, hex, decimal))(n)
}

fn constant_byte(i: Span) -> PResult<u8> {
    context(
        Context::Byte,
        alt((
            preceded(char('$'), number),
            map(char_literal, |c: Span<char>| c.map_extra(|c| c as u8)),
        )),
    )(i)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Addr<'a> {
    Pointer,
    Addr(u16),
    Symbol(&'a str),
}

fn addr(i: Span) -> PResult<Addr> {
    context(
        Context::Addr,
        delimited(
            char('['),
            cut(alt((
                map(number, |s: Span<u16>| s.map_extra(Addr::Addr)),
                map(tag("I"), |x: Span| x.map_extra(|_| Addr::Pointer)),
                map(identifier, |x: Span| {
                    x.map_extra(|_| Addr::Symbol(x.fragment()))
                }),
            ))),
            char(']'),
        ),
    )(i)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstantAddr<'a> {
    Pointer,
    Addr(u16),
    Symbol(&'a str),
}

fn constant_addr(i: Span) -> PResult<ConstantAddr> {
    context(
        Context::ConstantAddr,
        alt((
            preceded(
                char('#'),
                cut(alt((map(number, |s: Span<u16>| {
                    s.map_extra(ConstantAddr::Addr)
                }),))),
            ),
            map(tag("I"), |x: Span| x.map_extra(|_| ConstantAddr::Pointer)),
            map(identifier, |x: Span| {
                x.map_extra(|_| ConstantAddr::Symbol(x.fragment()))
            }),
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
                |x: Span| x.map_extra(|_| u8::from_str_radix(x.fragment(), 16).unwrap()),
            )),
        ),
    )(i)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arg<'a> {
    Addr(Addr<'a>),
    Byte(u8),
    Register(u8),
    ConstantAddr(ConstantAddr<'a>),
}

pub fn arg(i: Span) -> PResult<Arg> {
    context(
        Context::Arg,
        alt((
            map(register, |x| x.map_extra(Arg::Register)),
            map(constant_byte, |x| x.map_extra(Arg::Byte)),
            map(addr, |x| x.map_extra(Arg::Addr)),
            map(constant_addr, |x| x.map_extra(Arg::ConstantAddr)),
        )),
    )(i)
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::{assert_nom_err, assert_nom_failure, assert_nom_ok_extra};

    use super::{
        addr, arg, char_literal, constant_addr, constant_byte, number, register, Addr, Arg,
        ConstantAddr, Span,
    };

    #[test]
    fn parse_char() {
        assert_nom_ok_extra(char_literal, Span::new("'c'"), "", 'c');
        assert_nom_ok_extra(char_literal, Span::new("'\\n'"), "", '\n');
        assert_nom_ok_extra(char_literal, Span::new("'\\\\'"), "", '\\');
        assert_nom_err(char_literal, Span::new("'hadjs'"));
    }

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

        assert_nom_ok_extra(constant_byte, Span::new("'A' hi"), " hi", b'A');
        assert_nom_err(constant_byte, Span::new("0b10"));
        assert_nom_err(constant_byte, Span::new("[0b10]"));
        // panic!()
    }

    #[test]
    fn parse_addr() {
        assert_nom_err(addr, Span::new("$10"));
        assert_nom_ok_extra(addr, Span::new("[10]"), "", Addr::Addr(10));
        assert_nom_ok_extra(addr, Span::new("[I]"), "", Addr::Pointer);
        assert_nom_ok_extra(addr, Span::new("[hi]"), "", Addr::Symbol("hi"));
    }

    #[test]
    fn parse_constant_addr() {
        assert_nom_err(constant_addr, Span::new("$10"));
        assert_nom_ok_extra(constant_addr, Span::new("#10"), "", ConstantAddr::Addr(10));
        assert_nom_ok_extra(constant_addr, Span::new("I"), "", ConstantAddr::Pointer);
        assert_nom_ok_extra(
            constant_addr,
            Span::new("hi"),
            "",
            ConstantAddr::Symbol("hi"),
        );
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
        assert_nom_ok_extra(
            arg,
            Span::new("AAAAAAAAAAAAAAAA"),
            "",
            Arg::ConstantAddr(ConstantAddr::Symbol("AAAAAAAAAAAAAAAA")),
        );
        // panic!()
    }
}
