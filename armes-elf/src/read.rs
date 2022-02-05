use std::collections::HashMap;

use nom::{
    bytes::complete::{take, take_till},
    combinator::map,
    number::complete::{le_u16, le_u32},
    sequence::{preceded, tuple},
    IResult, Parser,
};

use crate::{Elf, Pointee};

#[derive(Debug)]
struct Header {
    symn: u16,
    reln: u16,
    datan: u16,
    stroff: u32,
}

type PResult<'a, O> = IResult<&'a [u8], O>;

fn parse_header(i: &[u8]) -> PResult<Header> {
    map(
        tuple((le_u16, le_u16, le_u16, le_u32)),
        |(symn, reln, datan, stroff)| Header {
            symn,
            reln,
            datan,
            stroff,
        },
    )(i)
}

fn parse_list<I, O, E, P: Parser<I, O, E>>(
    entries: u16,
    mut parser: P,
) -> impl FnMut(I) -> IResult<I, Vec<O>, E> {
    move |i| {
        let parser = &mut parser;
        (0..entries).fold(
            Ok((i, Vec::with_capacity(entries as usize))),
            move |r, _| match r {
                Ok((i, mut v)) => {
                    let (i, r) = parser.parse(i)?;
                    v.push(r);
                    Ok((i, v))
                }
                e => e,
            },
        )
    }
}

#[derive(Debug)]
struct SymTabEntry {
    name: u32,
    points: u8,
    pointee: u16,
}

fn parse_symtab_entry(i: &[u8]) -> PResult<SymTabEntry> {
    map(
        tuple((le_u32, map(take(1usize), |x: &[u8]| x[0]), le_u16)),
        |(name, points, pointee)| SymTabEntry {
            name,
            points,
            pointee,
        },
    )(i)
}

fn parse_symtab<'a>(entries: u16) -> impl FnMut(&'a [u8]) -> PResult<Vec<SymTabEntry>> {
    parse_list(entries, parse_symtab_entry)
}

#[derive(Debug)]
struct RelTabEntry {
    sect: u16,
    from: u16,
    sym: u16,
}

fn parse_reltab_entry(i: &[u8]) -> PResult<RelTabEntry> {
    map(tuple((le_u16, le_u16, le_u16)), |(sect, from, sym)| {
        RelTabEntry { sect, from, sym }
    })(i)
}

fn parse_reltab<'a>(entries: u16) -> impl FnMut(&'a [u8]) -> PResult<Vec<RelTabEntry>> {
    parse_list(entries, parse_reltab_entry)
}

#[derive(Debug)]
struct DataTabEntry {
    addr: u16,
    off: u32,
    size: u16,
}

fn parse_datatab_entry(i: &[u8]) -> PResult<DataTabEntry> {
    map(tuple((le_u16, le_u32, le_u16)), |(addr, off, size)| {
        DataTabEntry { addr, off, size }
    })(i)
}

fn parse_datatab<'a>(entries: u16) -> impl FnMut(&'a [u8]) -> PResult<Vec<DataTabEntry>> {
    parse_list(entries, parse_datatab_entry)
}

#[allow(clippy::type_complexity)]
fn parse_tables(
    i: &[u8],
) -> PResult<(
    Header,
    Vec<SymTabEntry>,
    Vec<RelTabEntry>,
    Vec<DataTabEntry>,
)> {
    let (i, header) = parse_header(i)?;
    let (i, symtab) = parse_symtab(header.symn)(i)?;
    let (i, reltab) = parse_reltab(header.reln)(i)?;
    let (i, datatab) = parse_datatab(header.datan)(i)?;
    Ok((i, (header, symtab, reltab, datatab)))
}

fn parse_str<'a>(at: u32) -> impl FnMut(&'a [u8]) -> PResult<String> {
    map(preceded(take(at as usize), take_till(|c| c == 0)), |x| {
        String::from_utf8_lossy(x).to_string()
    })
}

#[derive(Debug)]
pub enum ParseError<'a> {
    NomErr(nom::Err<nom::error::Error<&'a [u8]>>),
    SymbolNotFound,
}

impl<'a> From<nom::Err<nom::error::Error<&'a [u8]>>> for ParseError<'a> {
    fn from(e: nom::Err<nom::error::Error<&'a [u8]>>) -> Self {
        Self::NomErr(e)
    }
}

impl Elf {
    pub fn parse(data: &[u8]) -> Result<Self, ParseError> {
        let (_, (Header { stroff, .. }, symtab, reltab, datatab)) = parse_tables(data)?;
        let mut string_cache: HashMap<u32, String> = HashMap::new();
        let mut symbols_indices: Vec<String> = Vec::with_capacity(symtab.len());
        let symbols = symtab
            .iter()
            .map(
                |SymTabEntry {
                     name,
                     points,
                     pointee,
                 }| {
                    let name = if let Some(s) = string_cache.get(name) {
                        s.clone()
                    } else {
                        let s = parse_str(stroff + name)(data)?.1;
                        string_cache.insert(*name, s.clone());
                        s
                    };
                    symbols_indices.push(name.clone());
                    let p = match points {
                        1 => Pointee::Address(*pointee),
                        2 => {
                            Pointee::Symbol(if let Some(s) = string_cache.get(&(*pointee as u32)) {
                                s.clone()
                            } else {
                                let s = parse_str(stroff + *pointee as u32)(data)?.1;
                                string_cache.insert(*pointee as u32, s.clone());
                                s
                            })
                        }
                        _ => Pointee::None,
                    };
                    Ok((name, p))
                },
            )
            .fold::<Result<_, ParseError>, _>(
                Ok(HashMap::with_capacity(symtab.len())),
                |h, v: Result<_, ParseError>| {
                    h.and_then(|mut h| {
                        let (name, pointee) = v?;
                        h.insert(name, pointee);
                        Ok(h)
                    })
                },
            )?;
        let relocations = reltab
            .iter()
            .map::<Result<(_, _, String), ParseError>, _>(|RelTabEntry { sect, from, sym }| {
                Ok((
                    *sect,
                    *from,
                    symbols_indices
                        .get(*sym as usize)
                        .ok_or(ParseError::SymbolNotFound)?
                        .clone(),
                ))
            })
            .fold::<Result<_, ParseError>, _>(
                Ok(Vec::with_capacity(reltab.len())),
                |h, v: Result<_, _>| {
                    h.and_then(|mut h| {
                        let (sect, from, sym) = v?;
                        h.push((sect, from, sym));
                        Ok(h)
                    })
                },
            )?;
        let data = datatab
            .into_iter()
            .map(|DataTabEntry { addr, off, size }| {
                (
                    addr,
                    data[(off as usize)..(off as usize + size as usize)].to_vec(),
                )
            })
            .collect();
        Ok(Self {
            symbols,
            relocations,
            data,
        })
    }
}
