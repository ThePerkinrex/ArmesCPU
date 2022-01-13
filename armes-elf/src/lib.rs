use std::collections::HashMap;

#[cfg(feature = "read")]
pub mod read;

#[cfg(feature = "write")]
pub mod write;
#[derive(Debug, Clone)]
pub enum Pointee {
    Address(u16),
    Symbol(String),
    None,
}

impl Default for Pointee {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Clone)]
pub struct Relocatable {
    symbols: HashMap<String, Pointee>,
    relocations: Vec<(u16, u16, String)>,
    data: Vec<(u16, Vec<u8>)>,
}

impl Relocatable {
    pub fn new(data: Vec<(u16, Vec<u8>)>) -> Self {
        Self {
            data,
            symbols: Default::default(),
            relocations: Default::default(),
        }
    }

    /// Defines symbol not pointed to anything
    pub fn declare(&mut self, symbol: String) {
        self.symbols.insert(symbol, Pointee::None);
    }

    /// Declares if not present, points sym[bol to vaddr
    pub fn define(&mut self, symbol: String, addr: Pointee) {
        *self.symbols.entry(symbol).or_default() = addr;
    }

    pub fn relocate(&mut self, sect: u16, addr: u16, sym: String) {
        self.relocations.push((sect, addr, sym))
    }

    pub fn clear_rel(&mut self) {
        self.relocations.clear();
    }

    pub fn data(&self) -> &[(u16, Vec<u8>)] {
        &self.data
    }
}
