use std::collections::HashMap;

// pub mod literal; // Literal (as you read) elf types

// pub struct ElfHeader {}
#[derive(Debug, Clone)]
pub struct Relocatable {
    symbols: HashMap<String, Option<u16>>,
    relocations: Vec<(u16, String)>,
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
        self.symbols.insert(symbol, None);
    }

    /// Declares if not present, points symbol to vaddr
    pub fn define(&mut self, symbol: String, addr: u16) {
        *self.symbols.entry(symbol).or_default() = Some(addr);
    }

    pub fn relocate(&mut self, addr: u16, sym: String) {
        self.relocations.push((addr, sym))
    }

    pub fn clear_rel(&mut self) {
        self.relocations.clear();
    }

    pub fn data(&self) -> &[(u16, Vec<u8>)] {
        &self.data
    }
}
