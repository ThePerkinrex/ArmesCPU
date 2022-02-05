use asm_ir::Program;
use clap::StructOpt;
use config::Config;
use emulator::Cpu;
mod config;
fn main() {
    let c = Config::parse();
    let mut cpu = Cpu::default();
    let p = Program::parse(&std::fs::read(c.input).unwrap());
    for (off, p) in p.segments {
        cpu.load_bytes(p.into_iter(), off);
    }
    // let p = AstProgram::parse(&std::fs::read(c.input).unwrap());
    // for (off, p) in p.segments {
    //     cpu.load_ast_program(&p, off);
    // }
    // cpu.load_program(&[Ast::JumpOffset(0, 0x2000), Ast::Nop], 0);
    // cpu.load_program(
    //     &[
    //         Ast::LoadPointer(0xF002),
    //         Ast::LoadByte(0, b'H'),
    //         Ast::LoadFromRegs(0),
    //         Ast::LoadByte(0, b'i'),
    //         Ast::LoadFromRegs(0),
    //         Ast::LoadByte(0, b'\n'),
    //         Ast::LoadFromRegs(0),
    //         Ast::Jump(0x1004),
    //     ],
    //     0x1000,
    // ); // prints hi

    // cpu.load_program(
    //     &[
    //         // setup
    //         Ast::LoadByte(0, 0),      // x
    //         Ast::LoadByte(1, 1),      // y
    //         Ast::LoadByte(2, 0),      // a
    //         Ast::LoadPointer(0xF005), // Set the serial number out
    //         // swap part 1
    //         Ast::LoadReg(2, 1),
    //         // addition
    //         Ast::AddReg(1, 0),
    //         // swap part 2,
    //         Ast::LoadReg(0, 2),
    //         // print
    //         Ast::LoadFromRegs(0),
    //         // Reset if overflow
    //         Ast::SkipNotEqByte(0xF, 0),
    //         Ast::Jump(0x200A),
    //         //Ast::LoadFromRegs(0),
    //         Ast::Jump(0x1000),
    //     ],
    //     0x2000,
    // ); // prints fib
    loop {
        // TODO add halt io port
        // let t = std::time::Instant::now();
        cpu.cycle();
        // println!("> {:?}", t.elapsed());
        // println!("{:?}", cpu);
    }
}
