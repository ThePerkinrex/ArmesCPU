use asm_ir::Len;
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::char,
    combinator::{consumed, map, map_parser, not, recognize},
    multi::many0,
    sequence::delimited,
    Parser,
};

use super::{Choice, ChoiceIter, PResult, Span, ESCAPED_CHARS};

#[derive(Debug, Clone)]
pub enum Db<'a> {
    Str(Span<'a, String>),
    U8(Span<'a, u8>),
    U16(Span<'a, u16>),
    U32(Span<'a, u32>),
    U64(Span<'a, u64>),
    U128(Span<'a, Box<u128>>), // So that, the sife of the enum is the size of the 64bit system pointer size, not double that
    I8(Span<'a, i8>),
    I16(Span<'a, i16>),
    I32(Span<'a, i32>),
    I64(Span<'a, i64>),
    I128(Span<'a, Box<i128>>), // So that, the sife of the enum is the size of the 64bit system pointer size, not double that
    F32(Span<'a, f32>),
    F64(Span<'a, f64>),
}

impl<'a> IntoIterator for Db<'a> {
    type Item = u8;

    type IntoIter = Box<dyn Iterator<Item = u8> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Db::Str(s) => Box::new(s.fragment().bytes()),
            Db::U8(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::U16(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::U32(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::U64(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::U128(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::I8(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::I16(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::I32(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::I64(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::I128(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::F32(x) => Box::new(x.extra.to_le_bytes().into_iter()),
            Db::F64(x) => Box::new(x.extra.to_le_bytes().into_iter()),
        }
    }
}

impl<'a> Len for Db<'a> {
    fn len(&self) -> usize {
        match self {
            Db::Str(s) => s.fragment().as_bytes().len(),
            Db::U8(_) => 1,
            Db::U16(_) => 2,
            Db::U32(_) => 4,
            Db::U64(_) => 8,
            Db::U128(_) => 16,
            Db::I8(_) => 1,
            Db::I16(_) => 2,
            Db::I32(_) => 4,
            Db::I64(_) => 8,
            Db::I128(_) => 16,
            Db::F32(_) => 4,
            Db::F64(_) => 8,
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            Db::Str(s) => s.fragment().as_bytes().is_empty(),
            Db::U8(_) => false,
            Db::U16(_) => false,
            Db::U32(_) => false,
            Db::U64(_) => false,
            Db::U128(_) => false,
            Db::I8(_) => false,
            Db::I16(_) => false,
            Db::I32(_) => false,
            Db::I64(_) => false,
            Db::I128(_) => false,
            Db::F32(_) => false,
            Db::F64(_) => false,
            #[allow(unreachable_patterns)]
            x => x.len() == 0,
        }
    }
}

fn str_literal<'a>(s: Span<'a>) -> PResult<String> {
    let escaped_chars = ChoiceIter(ESCAPED_CHARS.iter().map::<Box<dyn Parser<_, _, _>>, _>(
        |(a, b)| Box::new(map(tag(*a), |s: Span| s.map_extra(|_| *b))),
    ));
    // TODO get chars inside str correctly
    delimited(
        char('"'),
        map(
            consumed(many0(alt((
                alt(escaped_chars),
                map(
                    map_parser(take(1usize), |s: Span<'a>| {
                        dbg!(recognize(|s: Span<'a>| dbg!(not(alt((
                            char('\\'),
                            char('"')
                        )))(s)))(dbg!(s)))
                    }),
                    |s: Span| s.map_extra(|_| s.fragment().chars().next().unwrap()),
                ),
            )))),
            |(parsed, res): (Span, Vec<Span<char>>)| {
                parsed.map_extra(|()| res.into_iter().map(|s| s.extra).collect())
            },
        ),
        char('"'),
    )(s)
}

#[cfg(test)]
mod test {
    use crate::parser::{tests::assert_nom_ok_extra, Span};

    use super::str_literal;

    #[test]
    fn str_literal_test() {
        // FIXME this tests function
        // assert_nom_ok_extra(
        //     str_literal,
        //     Span::new("\"aaaa\"aaaa"),
        //     "aaaa",
        //     "aaaa".into(),
        // );
    }
}
