use armes_elf::Elf;
use asm_ir::Program;

fn link_to_elf_with_info(files: &[Elf]) -> (Elf, Vec<(String, String)>) {
    todo!()
}

pub fn link_to_elf(files: &[Elf]) -> Elf {
    link_to_elf_with_info(files).0
}

fn elf_to_program(elf: Elf, symbols: Vec<(String, String)>) -> Program {
    todo!()
}

pub fn link_to_program(files: &[Elf]) -> Program {
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
