pub enum ParseError {
    UnknownInstruction(u16),
    MoreDataNecessary(u16),
}

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
    pub fn parse(opcode: u16, next: Option<u16>) -> Result<Self, ParseError> {
        let instr = ((opcode & 0xF000) >> 12) as u8;
        // let addr = opcode & 0x0FFF;
        let x = ((opcode & 0x0F00) >> 8) as u8;
        let y = ((opcode & 0x00F0) >> 4) as u8;
        let n = (opcode & 0x000F) as u8;
        let kk = (opcode & 0x00FF) as u8;
        use Ast::*;
        match (instr, x, y, n) {
            (0x0, 0, 0, 0) => Ok(Nop),    // NOP
            (0x0, 0, 0xE, 0xE) => Ok(Return), //RET
            (0x1, _, 0, 0) => next.map(Jump).ok_or(ParseError::MoreDataNecessary(opcode)), // JP addr
            (0x1, _, 1, 0) => next
                .map(|addr| JumpOffset(x, addr))
                .ok_or(ParseError::MoreDataNecessary(opcode)), // JP Vx, addr
            (0x1, _, 2, 0) => Ok(JumpPointer),                                             // JP I
            (0x1, _, 0, 1) => next.map(Call).ok_or(ParseError::MoreDataNecessary(opcode)), // CALL addr
            (0x1, _, 1, 1) => next
                .map(|addr| CallOffset(x, addr))
                .ok_or(ParseError::MoreDataNecessary(opcode)), // CALL Vx, addr
            (0x1, _, 2, 1) => Ok(JumpPointer),                                             // CALL I
            (0x1, _, 0, 2) => next
                .map(LoadPointer)
                .ok_or(ParseError::MoreDataNecessary(opcode)), // LD I, addr
            (0x1, _, 1, 2) => next
                .map(|addr| LoadPointerOffset(x, addr))
                .ok_or(ParseError::MoreDataNecessary(opcode)), // LD I, Vx, addr
            (0x3, _, _, _) => Ok(SkipEqByte(x, kk)), // SE Vx byte
            (0x4, _, _, _) => Ok(SkipNotEqByte(x, kk)), // SNE Vx byte
            (0x5, _, _, 0) => Ok(SkipEqReg(x, y)),   // SE Vx, Vy
            (0x5, _, _, 1) => Ok(SkipNotEqReg(x, y)), // SNE Vx, Vy
            (0x6, _, _, _) => Ok(LoadByte(x, kk)),   // LD Vx, byte
            (0x7, _, _, _) => Ok(AddByte(x, kk)),    // ADD Vx, byte
            (0x8, _, _, 0) => Ok(LoadReg(x, y)),     // LD Vx, Vy
            (0x8, _, _, 1) => Ok(Or(x, y)),          // OR Vx, Vy
            (0x8, _, _, 2) => Ok(And(x, y)),         // AND Vx, Vy
            (0x8, _, _, 3) => Ok(Xor(x, y)),         // XOR Vx, Vy
            (0x8, _, _, 4) => Ok(AddReg(x, y)),      // ADD Vx, Vy
            (0x8, _, _, 5) => Ok(Sub(x, y)),         // SUB Vx, Vy
            (0x8, _, 0, 6) => Ok(ShiftRight(x)),     // SHR Vx
            (0x8, _, 1, 6) => Ok(ShiftLeft(x)),    // SHL Vx
            (0x8, _, _, 7) => Ok(SubNeg(x, y)),      // SUBN Vx, Vy
            (0xF, _, 0x1, 0xE) => Ok(AddToPointer(x)), // ADD I, Vx
            (0xF, _, 0x3, 0x3) => Ok(LoadDigits(x)), // LD B, Vx
            (0xF, _, 0x5, 0x5) => Ok(LoadFromRegs(x)), // LD [I], Vx
            (0xF, _, 0x6, 0x5) => Ok(LoadIntoRegs(x)), // LD Vx, [I]
            _ => Err(ParseError::UnknownInstruction(opcode)),
        }
    }
}

pub enum BytecodeInstr {
    Single(u16),
    Double(u16, u16),
}

impl From<Ast> for BytecodeInstr {
    fn from(val: Ast) -> Self {
        use BytecodeInstr::*;
        match val {
            Ast::Nop => Single(0x00E0),
            Ast::Return => Single(0x00EE),
            Ast::Jump(addr) => Double(0x1000, addr),
            Ast::JumpOffset(x, addr) => Double(0x1010 | ((x as u16) << 8), addr),
            Ast::JumpPointer => Single(0x1020),
            Ast::Call(addr) => Double(0x1001, addr),
            Ast::CallOffset(x, addr) => Double(0x1011 | ((x as u16) << 8), addr),
            Ast::CallPointer => Single(0x1120),
            Ast::LoadPointer(addr) => Double(0x1002, addr),
            Ast::LoadPointerOffset(x, addr) => Double(0x1012 | ((x as u16) << 8), addr),
            Ast::SkipEqByte(x, kk) => Single(0x3000 | ((x as u16) << 8) | kk as u16),
            Ast::SkipNotEqByte(x, kk) => Single(0x4000 | ((x as u16) << 8) | kk as u16),
            Ast::SkipEqReg(x, y) => Single(0x5000 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::SkipNotEqReg(x, y) => Single(0x9000 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::LoadByte(x, kk) => Single(0x6000 | ((x as u16) << 8) | kk as u16),
            Ast::LoadReg(x, y) => Single(0x8000 | ((x as u16) << 8) | ((y as u16) << 4)),
            Ast::LoadDigits(x) => Single(0xF033 | ((x as u16) << 8)),
            Ast::LoadIntoRegs(x) => Single(0xF055 | ((x as u16) << 8)),
            Ast::LoadFromRegs(x) => Single(0xF065 | ((x as u16) << 8)),
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

// #[cfg(test)]
// mod tests {
//     #[test]
//     fn it_works() {
//         let result = 2 + 2;
//         assert_eq!(result, 4);
//     }
// }
