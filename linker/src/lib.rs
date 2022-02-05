use std::collections::HashMap;

use armes_elf::{Elf, Pointee};
// use asm_ir::Program;
pub type Program = Vec<(u16, Vec<u8>)>;

mod error;

use error::{ElfError, ProgramLinkError};

fn link_to_elf_with_info(
    files: Vec<(Elf, String)>,
) -> Result<(Elf, HashMap<String, String>), Vec<ElfError>> {
    let mut info: HashMap<String, String> = HashMap::new();
    let mut errors = Vec::new();
    // Generate symbol information
    for (file, name) in &files {
        for (s, p) in file.symbols() {
            if p != &Pointee::None {
                if let Some(og) = info.get(s) {
                    errors.push(ElfError::DuplicateSymbol(
                        s.clone(),
                        og.clone(),
                        name.clone(),
                    ))
                } else {
                    info.insert(s.clone(), name.clone());
                }
            }
        }
    }
    // Fill nones
    for (file, name) in &files {
        for (s, p) in file.symbols() {
            if !info.contains_key(s) && p == &Pointee::None {
                info.insert(s.clone(), name.clone());
            }
        }
    }
    // Actually link elf files
    if errors.is_empty() {
        let mut fat_elf = Elf::new(vec![]);
        let mut data = Vec::new();
        let mut start_addr = 0;
        let mut i = 0;
        for (elf, _) in files {
            let (segments, relocations, symbols) = elf.into_inner();
            for (symbol, pointee) in symbols {
                fat_elf.define(
                    symbol,
                    match pointee {
                        Pointee::Address(vaddr) => Pointee::Address(start_addr as u16 + vaddr),
                        p => p,
                    },
                )
            }
            for (segment, addr, symbol) in relocations {
                fat_elf.relocate(segment + i as u16, addr, symbol);
            }
            let old_addr = start_addr;
            if let Some((addr, bytes)) = segments.last() {
                start_addr += *addr as usize + bytes.len();
            }
            i += segments.len();
            data.extend(
                segments
                    .into_iter()
                    .map(|(addr, s)| (old_addr as u16 + addr, s)),
            );
        }
        fat_elf.data = data;
        Ok((fat_elf, info))
    } else {
        Err(errors)
    }
}

pub fn link_to_elf(files: Vec<(Elf, String)>) -> Result<Elf, Vec<ElfError>> {
    link_to_elf_with_info(files).map(|(x, _)| x)
}

/// Expects all symbols to be defined
fn elf_to_program(
    elf: Elf,
    origin: HashMap<String, String>,
) -> Result<Program, Vec<ProgramLinkError>> {
    let mut errors = Vec::new();
    let (mut data, relocations, symbols) = elf.into_inner();
    // Resolve symbols
    errors.extend(
        symbols
            .iter()
            .filter(|(_, i)| **i == Pointee::None)
            .map(|(symbol, _)| {
                ProgramLinkError::SymbolNotDefined(symbol.clone(), origin.get(symbol).cloned())
            }),
    );
    let mut resolved = HashMap::new();
    resolved.extend(
        symbols
            .iter()
            .filter(|(_, i)| matches!(i, Pointee::Address(_)))
            .map(|(symbol, p)| match p {
                Pointee::Address(addr) => (symbol.clone(), *addr),
                _ => unreachable!(),
            }),
    );
    // add Pointee::Symbol
    if errors.is_empty() {
        loop {
            let mut resolved_sth = false;
            resolved.extend(
                symbols
                    .iter()
                    .filter(|(_, i)| matches!(i, Pointee::Symbol(_)))
                    .map(|(symbol, p)| {
                        (
                            symbol,
                            match p {
                                Pointee::Symbol(s) => s,
                                _ => unreachable!(),
                            },
                        )
                    })
                    .filter(|(s, _)| !resolved.contains_key(*s))
                    .filter(|(_, s)| resolved.contains_key(*s))
                    .map(|(symbol, points)| {
                        resolved_sth = true;
                        (symbol.clone(), resolved[points])
                    })
                    .collect::<Vec<_>>()
                    .into_iter(),
            );

            if !resolved_sth {
                break;
            }
        }
        errors.extend(
            symbols
                .into_iter()
                .filter(|(s, _)| !resolved.contains_key(s))
                .map(|(s, _)| {
                    let x = origin.get(&s).cloned();
                    ProgramLinkError::SymbolNotDeclared(s, x)
                }),
        );
    }
    if errors.is_empty() {
        // Replace relocations
        for (sect, fileaddr, sym, memaddr) in relocations
            .iter()
            .map(|(a, b, c)| (a, b, c, resolved.get(c)))
        {
            if let Some(memaddr) = memaddr {
                if let Some((_, segment)) = data.get_mut(*sect as usize) {
                    if let Some(x) = segment.get_mut(*fileaddr as usize..*fileaddr as usize + 2) {
                        x.copy_from_slice(&memaddr.to_le_bytes())
                    } else {
                        errors.push(ProgramLinkError::SegmentAddrNotFound(
                            sym.clone(),
                            *sect,
                            *fileaddr,
                        ))
                    }
                } else {
                    errors.push(ProgramLinkError::SegmentNotFound(sym.clone(), *sect))
                }
            } else {
                errors.push(ProgramLinkError::SymbolNotDefined(
                    sym.clone(),
                    origin.get(sym).cloned(),
                ));
            }
        }
    }
    if errors.is_empty() {
        Ok(data)
    } else {
        Err(errors)
    }
}

pub fn link_to_program(files: Vec<(Elf, String)>) -> Result<Program, Vec<ElfError>> {
    let (elf, symbols) = link_to_elf_with_info(files)?;
    elf_to_program(elf, symbols).map_err(|s| s.into_iter().map(Into::into).collect())
}

#[cfg(test)]
mod tests {
    // TODO write tests

    use std::collections::HashMap;

    use armes_elf::{Elf, Pointee};

    use crate::{elf_to_program, error::ProgramLinkError};

    #[test]
    fn link_program_ok_test() {
        let mut elf = Elf::new(vec![(0, vec![0, 0, 0, 0])]);
        elf.define("memcpy".to_string(), Pointee::Address(0xffff));
        elf.relocate(0, 2, "memcpy".to_string());
        let mut origin: HashMap<String, String> = HashMap::new();
        origin.insert("memcpy".to_string(), "src".to_string());
        let p = elf_to_program(elf, origin);
        assert_eq!(p, Ok(vec![(0, vec![0, 0, 0xff, 0xff])]))
    }

    #[test]
    fn link_program_symbol_not_defined_test() {
        let mut elf = Elf::new(vec![(0, vec![0, 0, 0, 0])]);
        // elf.define("memcpy".to_string(), Pointee::Address(0xffff));
        elf.relocate(0, 2, "memcpy".to_string());
        let mut origin: HashMap<String, String> = HashMap::new();
        origin.insert("memcpy".to_string(), "src".to_string());
        let p = elf_to_program(elf, origin);
        assert_eq!(
            p,
            Err(vec![ProgramLinkError::SymbolNotDefined(
                "memcpy".to_string(),
                Some("src".to_string())
            )])
        )
    }
}
