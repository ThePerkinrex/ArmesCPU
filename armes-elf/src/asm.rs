use std::collections::HashMap;

use asm_ir::{AstExtended, Len};

use crate::{Elf, Pointee};

impl<
        I: IntoIterator<Item = u8>,
        J: IntoIterator<Item = AstExtended<I>>,
        K: IntoIterator<Item = (u16, J)>,
    > From<K> for Elf
{
    fn from(v: K) -> Self {
        Self::new(
            v.into_iter()
                .map(|(off, v)| {
                    (
                        off,
                        v.into_iter().flat_map(AstExtended::into_iter).collect(),
                    )
                })
                .collect(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmElf<I> {
    symbols: HashMap<String, AsmPointee>,
    relocations: Vec<(u16, u16, String)>,
    data: Vec<(u16, Vec<AstExtended<I>>)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsmPointee {
    None,
    Symbol(String),
    /// Section idx & instr number
    Idx(usize, usize),
    Address(u16),
}

impl Default for AsmPointee {
    fn default() -> Self {
        Self::None
    }
}

impl<I> AsmElf<I> {
    pub fn new(data: Vec<(u16, Vec<AstExtended<I>>)>) -> Self {
        Self {
            data,
            symbols: Default::default(),
            relocations: Default::default(),
        }
    }

    /// Defines symbol not pointed to anything
    pub fn declare(&mut self, symbol: String) {
        self.symbols.insert(symbol, AsmPointee::None);
    }

    /// Declares if not present, points symbol to sect and index of instruction or data
    pub fn define(&mut self, symbol: String, addr: AsmPointee) {
        *self.symbols.entry(symbol).or_default() = addr;
    }

    /// Addr is instruction idx, not byte addr
    pub fn relocate(&mut self, sect: u16, addr: u16, sym: String) {
        self.relocations.push((sect, addr, sym))
    }

    pub fn clear_rel(&mut self) {
        self.relocations.clear();
    }
}

impl<I: IntoIterator<Item = u8> + Len> From<AsmElf<I>> for Elf {
    fn from(a: AsmElf<I>) -> Self {
        let mut offsets = Vec::with_capacity(a.data.len());
        let data = a
            .data
            .into_iter()
            .map(|(off, v)| {
                (off, {
                    let mut offsets_inner = Vec::with_capacity(v.len());
                    let r = v
                        .into_iter()
                        .scan(0, |off, item| {
                            offsets_inner.push(*off);
                            *off += item.len();
                            Some(item)
                        })
                        .flat_map(AstExtended::into_iter)
                        .collect::<Vec<_>>();
                    offsets.push((off, offsets_inner));
                    r
                })
            })
            .collect();
        let mut elf = Self::new(data);

        for (name, s) in a.symbols.into_iter() {
            elf.define(
                name,
                match s {
                    AsmPointee::None => Pointee::None,
                    AsmPointee::Symbol(s) => Pointee::Symbol(s),
                    AsmPointee::Idx(i, j) => Pointee::Address({
                        let (off, offsets) = &offsets[i];
                        *off + offsets[j] as u16
                    }),
                    AsmPointee::Address(a) => Pointee::Address(a),
                },
            )
        }

        for (sect, addr, sym) in a.relocations {
            elf.relocate(sect, addr, sym)
        }

        elf
    }
}
