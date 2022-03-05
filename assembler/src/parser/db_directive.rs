use asm_ir::Len;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take},
    character::complete::{char, multispace0, multispace1},
    combinator::{consumed, cut, map, verify},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    IResult, Parser,
};

use crate::error;

use super::{
    arg::{char_literal, number},
    Choice, PResult, Span, ESCAPED_CHARS,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Db<'a> {
    Str(String),
    U8(u8),
    // U16(Span<'a, u16>),
    // U32(Span<'a, u32>),
    // U64(Span<'a, u64>),
    // U128(Span<'a, Box<u128>>), // So that, the sife of the enum is the size of the 64bit system pointer size, not double that
    // I8(Span<'a, i8>),
    // I16(Span<'a, i16>),
    // I32(Span<'a, i32>),
    // I64(Span<'a, i64>),
    // I128(Span<'a, Box<i128>>), // So that, the sife of the enum is the size of the 64bit system pointer size, not double that
    // F32(Span<'a, f32>),
    // F64(Span<'a, f64>),
    Combination(Vec<Span<'a, Db<'a>>>),
}

impl<'a> IntoIterator for Db<'a> {
    type Item = u8;

    type IntoIter = Box<dyn Iterator<Item = u8> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Db::Str(s) => Box::new(s.bytes().collect::<Vec<_>>().into_iter()),
            Db::U8(x) => Box::new(x.to_le_bytes().into_iter()),
            // Db::U16(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::U32(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::U64(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::U128(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::I8(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::I16(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::I32(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::I64(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::I128(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::F32(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            // Db::F64(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::Combination(v) => Box::new(v.into_iter().flat_map(|x| x.extra.into_iter())),
        }
    }
}

impl<'a> Len for Db<'a> {
    fn len(&self) -> usize {
        match self {
            Db::Str(s) => s.as_bytes().len(),
            Db::U8(_) => 1,
            // Db::U16(_) => 2,
            // Db::U32(_) => 4,
            // Db::U64(_) => 8,
            // Db::U128(_) => 16,
            // Db::I8(_) => 1,
            // Db::I16(_) => 2,
            // Db::I32(_) => 4,
            // Db::I64(_) => 8,
            // Db::I128(_) => 16,
            // Db::F32(_) => 4,
            // Db::F64(_) => 8,
            Db::Combination(x) => x.iter().map(|x| x.len()).sum(),
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Db::Str(s) => s.as_bytes().is_empty(),
            Db::U8(_) => false,
            // Db::U16(_) => false,
            // Db::U32(_) => false,
            // Db::U64(_) => false,
            // Db::U128(_) => false,
            // Db::I8(_) => false,
            // Db::I16(_) => false,
            // Db::I32(_) => false,
            // Db::I64(_) => false,
            // Db::I128(_) => false,
            // Db::F32(_) => false,
            // Db::F64(_) => false,
            Db::Combination(x) => x.is_empty() || x.iter().all(|x| x.is_empty()),
            #[allow(unreachable_patterns)]
            x => x.len() == 0,
        }
    }
}

fn str_literal(s: Span) -> PResult<String> {
    let escaped_chars = Choice::new_with_iter(
        ESCAPED_CHARS
            .iter()
            .map::<Box<dyn Parser<_, _, _>>, _>(|(a, b)| {
                Box::new(map(tag(*a), |s: Span| s.map_extra(|_| *b)))
            }),
    );
    delimited(
        char('"'),
        map(
            consumed(many0(alt((
                alt(escaped_chars),
                verify(
                    map(take(1usize), |s: Span| {
                        dbg!(s).map_extra(|_| s.fragment().chars().next().unwrap())
                    }),
                    |c: &Span<char>| c.extra != '"' && c.extra != '\\',
                ),
            )))),
            |(parsed, res): (Span, Vec<Span<char>>)| {
                parsed.map_extra(|()| res.into_iter().map(|s| s.extra).collect())
            },
        ),
        char('"'),
    )(s)
}

fn byte(s: Span) -> PResult<u8> {
    alt((number, map(char_literal, |s| s.map_extra(|x| x as u8))))(s)
}

fn combination(s: Span) -> IResult<Span, Vec<Span<Db>>, error::Error<Span>> {
    separated_list1(tuple((multispace0, char(','), multispace0)), arg_single)(s)
}

fn arg_single(s: Span) -> PResult<Db> {
    alt((
        map(str_literal, |x| x.map_extra(Db::Str)),
        map(byte, |x| x.map_extra(Db::U8)),
    ))(s)
}

fn arg(s: Span) -> PResult<Db> {
    alt((
        map(
            consumed(verify(combination, |x: &Vec<_>| x.len() > 1)),
            |(cons, x)| cons.map_extra(|()| Db::Combination(x)),
        ),
        arg_single,
    ))(s)
}

pub fn db_directive(s: Span) -> PResult<Db> {
    preceded(pair(tag_no_case("db"), multispace1), cut(arg))(s)
}

#[cfg(test)]
mod test {
    use crate::parser::{
        tests::{assert_nom_failure, assert_nom_ok_extra, assert_nom_ok_generic},
        Span,
    };

    use super::{arg, byte, db_directive, str_literal, Db};

    #[test]
    fn str_literal_test() {
        assert_nom_ok_extra(
            str_literal,
            Span::new("\"aaaa\"aaaa"),
            "aaaa",
            "aaaa".into(),
        );

        assert_nom_ok_extra(
            str_literal,
            Span::new("\"12\"34\"567"),
            "34\"567",
            "12".into(),
        );

        assert_nom_ok_extra(
            str_literal,
            Span::new("\"12\\\"34\"567"),
            "567",
            "12\"34".into(),
        );
    }

    #[test]
    fn byte_test() {
        assert_nom_ok_extra(byte, Span::new("191 aaaa"), " aaaa", 191);

        assert_nom_ok_extra(byte, Span::new("'a'jdiwjo"), "jdiwjo", b'a');

        assert_nom_failure(byte, Span::new("300"));
    }

    #[test]
    fn arg_test() {
        assert_nom_ok_extra(arg, Span::new("191 aaaa"), " aaaa", Db::U8(191));

        assert_nom_ok_extra(arg, Span::new("'a'jdiwjo"), "jdiwjo", Db::U8(b'a'));

        assert_nom_failure(arg, Span::new("300"));

        assert_nom_ok_extra(
            arg,
            Span::new("\"aaaa\"aaaa"),
            "aaaa",
            Db::Str("aaaa".into()),
        );

        assert_nom_ok_extra(
            arg,
            Span::new("\"12\"34\"567"),
            "34\"567",
            Db::Str("12".into()),
        );

        assert_nom_ok_extra(
            arg,
            Span::new("\"12\\\"34\"567"),
            "567",
            Db::Str("12\"34".into()),
        );

        assert_nom_ok_generic(arg, Span::new("\"12\\\"34\",22,'a'"), "", |o| {
            match o.extra {
                Db::Combination(v) => v
                    .into_iter()
                    .zip([Db::Str("12\"34".into()), Db::U8(22), Db::U8(b'a')].into_iter())
                    .all(|(a, b)| a.extra == b),
                _ => false,
            }
        });
    }

    #[test]
    fn db_directive_test() {
        assert_nom_ok_extra(db_directive, Span::new("db 191 aaaa"), " aaaa", Db::U8(191));

        assert_nom_ok_extra(
            db_directive,
            Span::new("db 'a'jdiwjo"),
            "jdiwjo",
            Db::U8(b'a'),
        );

        assert_nom_failure(db_directive, Span::new("db 300"));

        assert_nom_ok_extra(
            db_directive,
            Span::new("db \"aaaa\"aaaa"),
            "aaaa",
            Db::Str("aaaa".into()),
        );

        assert_nom_ok_extra(
            db_directive,
            Span::new("db \"12\"34\"567"),
            "34\"567",
            Db::Str("12".into()),
        );

        assert_nom_ok_extra(
            db_directive,
            Span::new("db \"12\\\"34\"567"),
            "567",
            Db::Str("12\"34".into()),
        );

        assert_nom_ok_generic(
            db_directive,
            Span::new("db \"12\\\"34\",22,'a'"),
            "",
            |o| match o.extra {
                Db::Combination(v) => v
                    .into_iter()
                    .zip([Db::Str("12\"34".into()), Db::U8(22), Db::U8(b'a')].into_iter())
                    .all(|(a, b)| a.extra == b),
                _ => false,
            },
        );
    }
}
