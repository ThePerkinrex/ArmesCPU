use std::fmt::Debug;

use asm_ir::{Ast, BytecodeInstr};
use memio::{MemoryIO, MemorySection};

pub mod memio;

pub struct Cpu {
    memory: MemorySection,
    program_counter: u16,
    pointer: u16,
    registers: [u8; 0x10],
    stack: Vec<u16>,
    skip: bool,
}
impl Default for Cpu {
    fn default() -> Self {
        let mut mem = MemorySection::new();
        mem.add([0; 0xF000]);
        mem.add(memio::serial::SerialTty);
        mem.add(memio::serial::SerialNumberInterface);
        Self {
            memory: mem,
            program_counter: Default::default(),
            pointer: Default::default(),
            registers: Default::default(),
            stack: Vec::default(),
            skip: false,
        }
    }
}

impl Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cpu")
            .field("program_counter", &self.program_counter)
            .field("pointer", &self.pointer)
            .field("registers", &self.registers)
            .field("stack", &self.stack)
            .field("skip", &self.skip)
            .finish()
    }
}

impl Cpu {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn load_bytes<I: Iterator<Item = u8>>(&mut self, bytes: I, offset: u16) {
        for (i, b) in bytes.enumerate() {
            self.memory.set(offset + i as u16, b);
        }
    }

    pub fn load_program(&mut self, program: &[Ast], offset: u16) {
        self.load_bytes(
            program
                .iter()
                .flat_map(|x| BytecodeInstr::from(*x).into_iter()),
            offset,
        )
    }

    pub fn cycle(&mut self) {
        let mut next = Some(
            self.memory.get(self.program_counter + 2).unwrap() as u16
                | ((self.memory.get(self.program_counter + 3).unwrap() as u16) << 8),
        );
        let instr = match Ast::parse(
            self.memory.get(self.program_counter).unwrap() as u16
                | ((self.memory.get(self.program_counter + 1).unwrap() as u16) << 8),
            &mut next,
        ) {
            Ok(a) => a,
            Err(asm_ir::ParseError::UnknownInstruction(opcode)) => {
                panic!("Unknown instruction: {:04X}", opcode)
            }
            Err(asm_ir::ParseError::MoreDataNecessary(_)) => unreachable!(),
        };
        self.program_counter += if next.is_some() { 2 } else { 4 };
        if self.skip {
            self.skip = false;
        } else {
            match instr {
                Ast::Nop => (),
                Ast::Return => self.program_counter = self.stack.pop().unwrap(),
                Ast::Jump(addr) => self.program_counter = addr,
                Ast::JumpOffset(x, addr) => {
                    self.program_counter = addr + self.registers[x as usize] as u16
                }
                Ast::JumpPointer => self.program_counter = self.pointer,
                Ast::Call(addr) => {
                    self.stack.push(self.program_counter);
                    self.program_counter = addr
                }
                Ast::CallOffset(x, addr) => {
                    self.stack.push(self.program_counter);
                    self.program_counter = addr + self.registers[x as usize] as u16
                }
                Ast::CallPointer => {
                    self.stack.push(self.program_counter);
                    self.program_counter = self.pointer
                }
                Ast::SkipEqByte(x, kk) => self.skip = self.registers[x as usize] == kk,
                Ast::SkipNotEqByte(x, kk) => self.skip = self.registers[x as usize] != kk,
                Ast::SkipEqReg(x, y) => {
                    self.skip = self.registers[x as usize] != self.registers[y as usize]
                }
                Ast::SkipNotEqReg(x, y) => {
                    self.skip = self.registers[x as usize] != self.registers[y as usize]
                }
                Ast::LoadByte(x, kk) => self.registers[x as usize] = kk,
                Ast::LoadReg(x, y) => self.registers[x as usize] = self.registers[y as usize],
                Ast::LoadPointer(addr) => self.pointer = addr,
                Ast::LoadPointerOffset(x, addr) => {
                    self.pointer = addr + self.registers[x as usize] as u16
                }
                Ast::LoadDigits(x) => {
                    let v = self.registers[x as usize];
                    self.memory.set(self.pointer, v / 100);
                    self.memory.set(self.pointer + 1, (v % 100) / 10);
                    self.memory.set(self.pointer + 2, v % 10);
                }
                Ast::LoadIntoRegs(x) => {
                    for i in 0..=x {
                        self.registers[i as usize] =
                            self.memory.get(self.pointer + i as u16).unwrap()
                    }
                }
                Ast::LoadFromRegs(x) => {
                    for i in 0..=x {
                        self.memory
                            .set(self.pointer + i as u16, self.registers[i as usize]);
                    }
                }
                Ast::AddByte(x, kk) => {
                    let (r, overflowed) = self.registers[x as usize].overflowing_add(kk);
                    self.registers[0xF] = if overflowed { 1 } else { 0 };
                    self.registers[x as usize] = r;
                }
                Ast::AddReg(x, y) => {
                    let (r, overflowed) =
                        self.registers[x as usize].overflowing_add(self.registers[y as usize]);
                    self.registers[0xF] = if overflowed { 1 } else { 0 };
                    self.registers[x as usize] = r;
                }
                Ast::AddToPointer(x) => {
                    self.pointer = self.pointer.wrapping_add(self.registers[x as usize] as u16)
                }
                Ast::Or(x, y) => self.registers[x as usize] |= self.registers[y as usize],
                Ast::And(x, y) => self.registers[x as usize] &= self.registers[y as usize],
                Ast::Xor(x, y) => self.registers[x as usize] ^= self.registers[y as usize],
                Ast::ShiftRight(x) => {
                    self.registers[0xF] = self.registers[x as usize] & 1;
                    self.registers[x as usize] >>= 1;
                }
                Ast::ShiftLeft(x) => {
                    let (r, overflowed) = self.registers[x as usize].overflowing_shl(1);
                    self.registers[0xF] = if overflowed { 1 } else { 0 };
                    self.registers[x as usize] = r;
                }
                Ast::Sub(x, y) => {
                    let (r, overflowed) =
                        self.registers[x as usize].overflowing_sub(self.registers[y as usize]);
                    self.registers[0xF] = if !overflowed { 1 } else { 0 };
                    self.registers[x as usize] = r;
                }
                Ast::SubNeg(x, y) => {
                    let (r, overflowed) =
                        self.registers[y as usize].overflowing_sub(self.registers[x as usize]);
                    self.registers[0xF] = if !overflowed { 1 } else { 0 };
                    self.registers[x as usize] = r;
                }
            }
        }
    }
}
