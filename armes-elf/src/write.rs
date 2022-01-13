use std::{io::Write, collections::HashMap};

use crate::Pointee;

impl super::Relocatable {
	pub fn write<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
		// Write the header
		let mut strings = Vec::with_capacity(self.symbols.keys().map(|x| x.as_bytes().len()+1).sum());
		let mut symbol_positions = HashMap::with_capacity(self.symbols.len());
		let symbols = self.symbols.iter().enumerate().map(|(idx, (name, s))| {
			let i = strings.len();
			strings.extend(name.as_bytes());
			strings.push(0);
			symbol_positions.insert(name, idx);
			(i, s)
		}).collect::<Vec<_>>().into_iter().map(|(i, p)| {
			(i as u32,match p {
				Pointee::None => (0,0),
				Pointee::Address(a) => (1u8, *a),
				Pointee::Symbol(a) => (2, symbol_positions[a] as u16),
			})
		}).map(|(i,(a,b))| (i,a,b));

		let symn = self.symbols.len() as u16;
		writer.write_all(&symn.to_le_bytes())?; // symn
		let symoff = 22u32;
		writer.write_all(&symoff.to_le_bytes())?; // symoff
		let reln = self.relocations.len() as u16;
		writer.write_all(&reln.to_le_bytes())?; // reln
		let reloff = symoff + symn as u32 * 7;
		writer.write_all(&reloff.to_le_bytes())?; // reloff
		let datan = self.data.len() as u16;
		writer.write_all(&datan.to_le_bytes())?; // datan
		let dataoff = reloff + reln as u32 * 6;
		writer.write_all(&dataoff.to_le_bytes())?; // dataoff
		let stroff = dataoff + datan as u32 * 6;
		writer.write_all(&stroff.to_le_bytes())?; // stroff
		for (name, points, pointee) in symbols {
			writer.write_all(&name.to_le_bytes())?;
			writer.write_all(&[points])?;
			writer.write_all(&pointee.to_le_bytes())?;
		}
		for (sect, from, sym) in &self.relocations {
			let sym = symbol_positions[sym];
			writer.write_all(&sect.to_le_bytes())?;
			writer.write_all(&from.to_le_bytes())?;
			writer.write_all(&sym.to_le_bytes())?;
		}
		let mut off = stroff + strings.len() as u32;
		for (addr, data) in &self.data {
			writer.write_all(&addr.to_le_bytes())?;
			writer.write_all(&off.to_le_bytes())?;
			writer.write_all(&(data.len() as u16).to_le_bytes())?;
			off += data.len() as u32;
		}
		writer.write_all(&strings)?;
		for (_, data) in &self.data {
			writer.write_all(data)?;
		}
		Ok(())
	}
}