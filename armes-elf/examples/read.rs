use armes_elf::Elf;

fn main() {
    let r = Elf::parse(&std::fs::read("test.o").unwrap()).unwrap();
    println!("{:#?}", r);
}
