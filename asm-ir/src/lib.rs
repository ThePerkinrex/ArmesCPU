use std::io::Write;

#[derive(Debug)]
pub enum ParseError {
    UnknownInstruction(u16),
    MoreDataNecessary(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ast {
    Nop,
    Return,
    // System(u16),
    Jump(u16),
    JumpOffset(u8, u16),
    JumpPointer,
    Call(u16),
    CallOffset(u8, u16),
    CallPointer,
    SkipEqByte(u8, u8),
    SkipNotEqByte(u8, u8),
    SkipEqReg(u8, u8),
    SkipNotEqReg(u8, u8),
    LoadByte(u8, u8),
    LoadReg(u8, u8),
    LoadPointer(u16),
    LoadPointerOffset(u8, u16),
    // LoadFromDT(u8),
    // LoadKeyboard(u8),
    // LoadIntoDT(u8),
    // LoadIntoST(u8),
    // LoadFont(u8),
    LoadDigits(u8),
    LoadIntoRegs(u8),
    LoadFromRegs(u8),
    AddByte(u8, u8),
    AddReg(u8, u8),
    AddToPointer(u8),
    // Random(u8, u8),
    // Draw(u8, u8, u8),
    // SkipPressed(u8),
    // SkipNotPressed(u8),
    Or(u8, u8),
    And(u8, u8),
    Xor(u8, u8),
    ShiftRight(u8),
    ShiftLeft(u8),
    Sub(u8, u8),
    SubNeg(u8, u8),
}

impl Ast {
    pub fn parse(opcode: u16, next: &mut Option<u16>) -> Result<Self, ParseError> {
        let instr = ((opcode & 0xF000) >> 12) as u8;
        // let addr = opcode & 0x0FFF;
        let x = ((opcode & 0x0F00) >> 8) as u8;
        let y = ((opcode & 0x00F0) >> 4) as u8;
        let n = (opcode & 0x000F) as u8;
        let kk = (opcode & 0x00FF) as u8;
        use Ast::*;
        match (instr, x, y, n) {
            (0x0, 0, 0, 0) => Ok(Nop),        // NOP
            (0x0, 0, 0xE, 0xE) => Ok(Return), //RET
            (0x1, 0, 0, 0) => next
                .take()
                .map(Jump)
                .ok_or(ParseError::MoreDataNecessary(opcode)), // JP addr
            (0x1, _, 1, 0) => next
                .take()
                .map(|addr| JumpOffset(x, addr))
                .ok_or(ParseError::MoreDataNecessary(opcode)), // JP Vx, addr
            (0x1, 0, 2, 0) => Ok(JumpPointer), // JP I
            (0x1, 0, 0, 1) => next
                .take()
                .map(Call)
                .ok_or(ParseError::MoreDataNecessary(opcode)), // CALL addr
            (0x1, _, 1, 1) => next
                .take()
                .map(|addr| CallOffset(x, addr))
                .ok_or(ParseError::MoreDataNecessary(opcode)), // CALL Vx, addr
            (0x1, 0, 2, 1) => Ok(CallPointer), // CALL I
            (0x1, 0, 0, 2) => next
                .take()
                .map(LoadPointer)
                .ok_or(ParseError::MoreDataNecessary(opcode)), // LD I, addr
            (0x1, _, 1, 2) => next
                .take()
                .map(|addr| LoadPointerOffset(x, addr))
                .ok_or(ParseError::MoreDataNecessary(opcode)), // LD I, Vx, addr
            (0x3, _, _, _) => Ok(SkipEqByte(x, kk)), // SE Vx byte
            (0x4, _, _, _) => Ok(SkipNotEqByte(x, kk)), // SNE Vx byte
            (0x5, _, _, 0) => Ok(SkipEqReg(x, y)), // SE Vx, Vy
            (0x5, _, _, 1) => Ok(SkipNotEqReg(x, y)), // SNE Vx, Vy
            (0x6, _, _, _) => Ok(LoadByte(x, kk)), // LD Vx, byte
            (0x7, _, _, _) => Ok(AddByte(x, kk)), // ADD Vx, byte
            (0x8, _, _, 0) => Ok(LoadReg(x, y)), // LD Vx, Vy
            (0x8, _, _, 1) => Ok(Or(x, y)),   // OR Vx, Vy
            (0x8, _, _, 2) => Ok(And(x, y)),  // AND Vx, Vy
            (0x8, _, _, 3) => Ok(Xor(x, y)),  // XOR Vx, Vy
            (0x8, _, _, 4) => Ok(AddReg(x, y)), // ADD Vx, Vy
            (0x8, _, _, 5) => Ok(Sub(x, y)),  // SUB Vx, Vy
            (0x8, _, 0, 6) => Ok(ShiftRight(x)), // SHR Vx
            (0x8, _, 1, 6) => Ok(ShiftLeft(x)), // SHL Vx
            (0x8, _, _, 7) => Ok(SubNeg(x, y)), // SUBN Vx, Vy
            (0xF, _, 0x1, 0xE) => Ok(AddToPointer(x)), // ADD I, Vx
            (0xF, _, 0x3, 0x3) => Ok(LoadDigits(x)), // LD B, Vx
            (0xF, _, 0x5, 0x5) => Ok(LoadFromRegs(x)), // LD [I], Vx
            (0xF, _, 0x6, 0x5) => Ok(LoadIntoRegs(x)), // LD Vx, [I]
            _ => Err(ParseError::UnknownInstruction(opcode)),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BytecodeInstr {
    Single(u16),
    Double(u16, u16),
}

impl From<Ast> for BytecodeInstr {
    fn from(val: Ast) -> Self {
        use BytecodeInstr::*;
        match val {
            Ast::Nop => Single(0x0000),
            Ast::Return => Single(0x00EE),
            Ast::Jump(addr) => Double(0x1000, addr),
            Ast::JumpOffset(x, addr) => Double(0x1010 | ((x as u16) << 8), addr),
            Ast::JumpPointer => Single(0x1020),
            Ast::Call(addr) => Double(0x1001, addr),
            Ast::CallOffset(x, addr) => Double(0x1011 | ((x as u16) << 8), addr),
            Ast::CallPointer => Single(0x1021),
            Ast::LoadPointer(addr) => Double(0x1002, addr),
            Ast::LoadPointerOffset(x, addr) => Double(0x1012 | ((x as u16) << 8), addr),
            Ast::SkipEqByte(x, kk) => Single(0x3000 | ((x as u16) << 8) | kk as u16),
            Ast::SkipNotEqByte(x, kk) => Single(0x4000 | ((x as u16) << 8) | kk as u16),
            Ast::SkipEqReg(x, y) => Single(0x5000 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::SkipNotEqReg(x, y) => Single(0x5001 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::LoadByte(x, kk) => Single(0x6000 | ((x as u16) << 8) | kk as u16),
            Ast::LoadReg(x, y) => Single(0x8000 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::LoadDigits(x) => Single(0xF033 | ((x as u16) << 8)),
            Ast::LoadIntoRegs(x) => Single(0xF065 | ((x as u16) << 8)),
            Ast::LoadFromRegs(x) => Single(0xF055 | ((x as u16) << 8)),
            Ast::AddByte(x, kk) => Single(0x7000 | ((x as u16) << 8) | kk as u16),
            Ast::AddReg(x, y) => Single(0x8004 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::AddToPointer(x) => Single(0xF01E | ((x as u16) << 8)),
            Ast::Or(x, y) => Single(0x8001 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::And(x, y) => Single(0x8002 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::Xor(x, y) => Single(0x8003 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::ShiftRight(x) => Single(0x8006 | ((x as u16) << 8)),
            Ast::ShiftLeft(x) => Single(0x8016 | ((x as u16) << 8)),
            Ast::Sub(x, y) => Single(0x8005 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::SubNeg(x, y) => Single(0x8007 | ((x as u16) << 8) | ((y as u16) << 4)),
        }
    }
}

impl IntoIterator for BytecodeInstr {
    type Item = u8;

    type IntoIter = BytecodeIter;

    fn into_iter(self) -> Self::IntoIter {
        BytecodeIter { b: self, idx: 0 }
    }
}

pub struct BytecodeIter {
    b: BytecodeInstr,
    idx: usize,
}

impl Iterator for BytecodeIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let r = match (self.idx, self.b) {
            (0..=1, BytecodeInstr::Single(x) | BytecodeInstr::Double(x, _)) => {
                Some((x >> (self.idx * 8)) as u8)
            }
            (2..=3, BytecodeInstr::Double(_, y)) => Some((y >> ((self.idx - 2) * 8)) as u8),
            _ => None,
        };
        self.idx += 1;
        r
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Program {
    segments: Vec<(u16, Vec<Ast>)>,
}
// Binary layout:
// u8 numsegments
// segment*: u16 filepos, u16 mempos
// prog: u8*

impl Program {
    pub fn write<W: Write>(self, buf: &mut W) -> std::io::Result<()> {
        let segments = self
            .segments
            .into_iter()
            .map(|(off, instrs)| {
                (
                    off,
                    instrs
                        .into_iter()
                        // .inspect(|a| println!("{:?} {:?}", a, Into::<BytecodeInstr>::into(*a)))
                        .flat_map(|x| Into::<BytecodeInstr>::into(x).into_iter())
                        .collect::<Vec<u8>>(),
                )
            })
            .collect::<Vec<_>>();
        let mut header = Vec::<(u16, u16)>::with_capacity(segments.len());
        let mut p = 0;
        for (off, s) in &segments {
            header.push((p, *off));
            p += s.len() as u16;
        }
        buf.write_all(&[header.len() as u8])?;
        println!("Header: {:?}", header);
        for (filepos, mempos) in header {
            buf.write_all(&filepos.to_le_bytes())?;
            buf.write_all(&mempos.to_le_bytes())?;
        }
        for (_, p) in segments {
            buf.write_all(&p)?;
        }
        Ok(())
    }

    pub fn parse(mut data: &[u8]) -> Self {
        let num_segments = data[0] as usize;
        data = &data[1..];
        let mut segments = Vec::with_capacity(num_segments);
        for i in 0..num_segments {
            let filepos = data[i * 4] as u16 | ((data[i * 4 + 1] as u16) << 8);
            let mempos = data[i * 4 + 2] as u16 | ((data[i * 4 + 3] as u16) << 8);
            println!("Filepos: {}\tMempos: {}", filepos, mempos);
            let prog = if i + 1 < num_segments {
                let next_filepos = data[i * 4 + 4] as u16 | ((data[i * 4 + 5] as u16) << 8);
                println!("Next filepos: {}", next_filepos);
                &data[num_segments * 4..][filepos as usize..next_filepos as usize]
            } else {
                println!("Next filepos: EOF");
                &data[num_segments * 4..][filepos as usize..]
            };
            // let mut instr = Vec::new();
            let instr = prog
                .chunks(2)
                .map(|c| c[0] as u16 | ((c.get(1).copied().unwrap_or(0) as u16) << 8))
                .rev()
                .scan(None, |a, b| {
                    let r = (b, *a);
                    *a = Some(b);
                    Some(r)
                })
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                // .enumerate()
                // .inspect(|(i, (a, b))| println!("{} : {:04x} {:?}", i, a, b))
                .scan(false, |skip, (curr, mut next)| {
                    if *skip {
                        // println!("Skipping {}", i);
                        *skip = false;
                        Some(None)
                    } else {
                        // println!("Parsing {}", i);
                        match Ast::parse(curr, &mut next) {
                            Ok(a) => {
                                *skip = next.is_none();
                                Some(Some(a))
                            }
                            Err(e) => panic!("Error parsing instructions: {:?}", e),
                        }
                    }
                })
                .flatten()
                .collect();
            // .for_each(|_| ());
            segments.push((mempos, instr));
        }
        Self { segments }
    }
}

#[cfg(test)]
mod tests {
    use crate::Ast;
    use crate::BytecodeInstr;
    use crate::ParseError;
    use crate::Program;

    #[test]
    fn program_write_read() {
        let mut b = Vec::new();
        let p = Program {
            segments: vec![
                (0, vec![Ast::JumpOffset(0, 0x1000), Ast::Nop]),
                (
                    0x1000,
                    vec![
                        Ast::LoadPointer(0xF002),
                        Ast::LoadByte(0, b'H'),
                        Ast::LoadFromRegs(0),
                        Ast::LoadByte(0, b'i'),
                        Ast::LoadFromRegs(0),
                        Ast::LoadByte(0, b'\n'),
                        Ast::LoadFromRegs(0),
                        Ast::Jump(0x1008),
                    ],
                ),
            ],
        };
        p.clone().write(&mut b).expect("Error writing to buffer");
        println!("[");
        for s in b[1..].chunks(2) {
            print!("\t");
            for e in s {
                print!(" {:02X}", e)
            }
            println!(",")
        }
        println!("]");

        // println!("BUF: {:?}", b);
        let new_p = Program::parse(&b);
        assert_eq!(p, new_p);
    }

    #[test]
    fn fuzz_instruction_parser_and_writer() {
        let mut errors = 0;
        for b in 0u16..0xFFFF {
            let mut second = None;
            let instr = match Ast::parse(b, &mut None) {
                Ok(a) => a,
                Err(ParseError::MoreDataNecessary(_)) => {
                    second = Some(0);
                    Ast::parse(b, &mut Some(0)).unwrap()
                }
                _ => continue,
            };
            match (second, Into::<BytecodeInstr>::into(instr)) {
                (None, BytecodeInstr::Single(b2)) if b == b2 => (),
                (Some(b1), BytecodeInstr::Double(b2, b3)) if b == b2 && b1 == b3 => (),
                (second, bytecode) => {
                    errors += 1;
                    println!(
                        "FAIL: {:?} decoded to {:?} instead of ({} {:?})",
                        instr, bytecode, b, second
                    )
                }
            }
        }
        if errors > 0 {
            panic!("Instruction fuzz failed: {} errors", errors)
        }
    }
}
