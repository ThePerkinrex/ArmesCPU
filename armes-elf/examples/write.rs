use std::fs::File;

use armes_elf::{Elf, Pointee};

fn main() {
    let mut r = Elf::new(vec![(0, vec![0, 0, 0, 0])]);
    r.define("_main".to_string(), Pointee::Address(0));
    r.declare("memcpy".to_string());
    r.relocate(0, 2, "memcpy".to_string());

    let mut f = File::create("test.o").unwrap();
    r.write(&mut f).unwrap();
}
