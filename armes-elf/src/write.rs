use std::{collections::HashMap, io::Write};

use crate::Pointee;

impl super::Elf {
    pub fn write<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        // Write the header
        let mut strings =
            Vec::with_capacity(self.symbols.keys().map(|x| x.as_bytes().len() + 1).sum());
        let mut symbol_positions = HashMap::with_capacity(self.symbols.len());
        let symbols = self
            .symbols
            .iter()
            .enumerate()
            .map(|(idx, (name, s))| {
                let i = strings.len();
                // println!("Adding {:?} to i: {}, len: {} {:?}", name, i, name.as_bytes().len(), name.as_bytes());
                strings.extend(name.as_bytes());
                strings.push(0);
                symbol_positions.insert(name, idx);
                (i, s)
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|(i, p)| {
                (
                    i as u32,
                    match p {
                        Pointee::None => (0, 0),
                        Pointee::Address(a) => (1u8, *a),
                        Pointee::Symbol(a) => (2, symbol_positions[a] as u16),
                    },
                )
            })
            .map(|(i, (a, b))| (i, a, b));
        // let mut p = 0;
        let symn = self.symbols.len() as u16;
        writer.write_all(&symn.to_le_bytes())?; // symn
                                                // p += 2;
        let symoff = 10u32;
        // writer.write_all(&symoff.to_le_bytes())?; // symoff
        let reln = self.relocations.len() as u16;
        writer.write_all(&reln.to_le_bytes())?; // reln
                                                // p += 2;
        let reloff = symoff + symn as u32 * 7;
        // writer.write_all(&reloff.to_le_bytes())?; // reloff
        let datan = self.data.len() as u16;
        writer.write_all(&datan.to_le_bytes())?; // datan
                                                 // p += 2;
        let dataoff = reloff + reln as u32 * 6;
        // writer.write_all(&dataoff.to_le_bytes())?; // dataoff
        let stroff = dataoff + datan as u32 * 8;
        writer.write_all(&stroff.to_le_bytes())?; // stroff
                                                  // p += 4;
                                                  // println!("Header: {}", p);
        for (name, points, pointee) in symbols {
            writer.write_all(&name.to_le_bytes())?;
            // p += 4;
            writer.write_all(&[points])?;
            // p += 1;
            writer.write_all(&pointee.to_le_bytes())?;
            // p += 2;
        }
        // println!("Symtab: {}", p);
        for (sect, from, sym) in &self.relocations {
            let sym = symbol_positions[sym] as u16;
            writer.write_all(&sect.to_le_bytes())?;
            // p += 2;
            writer.write_all(&from.to_le_bytes())?;
            // p += 2;
            writer.write_all(&sym.to_le_bytes())?;
            // p += 2;
        }
        // println!("reltab: {}", p);
        let mut off = stroff + strings.len() as u32;
        for (addr, data) in &self.data {
            writer.write_all(&addr.to_le_bytes())?;
            // p += 2;
            writer.write_all(&off.to_le_bytes())?;
            // p += 4;
            writer.write_all(&(data.len() as u16).to_le_bytes())?;
            // p += 2;
            off += data.len() as u32;
        }
        // println!("datatab: {}", p);
        writer.write_all(&strings)?;
        // p += strings.len();
        // println!("strtab: {}", p);
        for (_, data) in &self.data {
            writer.write_all(data)?;
        }
        Ok(())
    }
}
