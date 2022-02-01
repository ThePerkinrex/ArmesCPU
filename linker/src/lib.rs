use std::collections::HashMap;

use armes_elf::{Elf, Pointee};
// use asm_ir::Program;
pub type Program = Vec<(u16, Vec<u8>)>;

mod error;

use error::ProgramLinkError;

fn link_to_elf_with_info(files: &[(Elf, String)]) -> (Elf, HashMap<String, String>) {
    todo!()
}

pub fn link_to_elf(files: &[(Elf, String)]) -> Elf {
    link_to_elf_with_info(files).0
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
                ProgramLinkError::SymbolNotDefined(
                    symbol.clone(),
                    origin
                        .get(symbol)
                        .expect("Symbol defined in a file")
                        .clone(),
                )
            }),
    );
    let mut resolved = HashMap::new();
    resolved.extend(symbols
        .iter()
        .filter(|(_, i)| matches!(i, Pointee::Address(_)))
        .map(|(symbol, p)| match p {
            Pointee::Address(addr) => (symbol.clone(), *addr),
            _ => unreachable!(),
        }));
    // TODO add Pointee:Symbol
    // symbols.iter()
    // .filter(|(_, i)| matches!(i, Pointee::Symbol(_)))
    // .map(|(symbol, p)| (symbol, match p {
    //     Pointee::Symbol(s) => s,
    //     _ => unreachable!(),
    // })).flat_map(|(origin, pointed)| if resolved.contains_key(pointed) {});
    // Replace relocations
    if errors.is_empty() {
        Ok(data)
    } else {
        Err(errors)
    }
}

pub fn link_to_program(files: &[(Elf, String)]) -> Result<Program, Vec<ProgramLinkError>> {
    let (elf, symbols) = link_to_elf_with_info(files);
    elf_to_program(elf, symbols)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
