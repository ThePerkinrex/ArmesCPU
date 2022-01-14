use std::collections::HashMap;

#[cfg(feature = "read")]
pub mod read;

#[cfg(feature = "write")]
pub mod write;
#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Elf {
    symbols: HashMap<String, Pointee>,
    relocations: Vec<(u16, u16, String)>,
    data: Vec<(u16, Vec<u8>)>,
}

impl Elf {
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

#[cfg(test)]
mod tests {
    #[cfg(all(feature = "write", feature = "read"))]
    #[test]
    fn test_serialize() {
        use crate::{Elf, Pointee};
        let data = vec![(0, vec![0; 16]), (0xff, vec![0, 1, 2, 3, 4])];
        let mut r = Elf::new(data);
        r.define("_main".to_string(), Pointee::Address(0));
        r.declare("memcpy".to_string());
        r.relocate(0, 2, "memcpy".to_string());
        let mut buf = Vec::new();
        r.write(&mut buf).unwrap();

        let parsed = Elf::parse(&buf).unwrap();
        assert_eq!(r, parsed);
    }
}
