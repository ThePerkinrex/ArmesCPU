use memio::{MemoryIO, MemorySection};

pub mod memio;

pub struct Cpu {
    memory: MemorySection,
    program_counter: u16,
    pointer: u16,
    registers: [u8; 0xF],
}
impl Default for Cpu {
    fn default() -> Self {
        let mut mem = MemorySection::new();
        mem.add([0; 0xF000]);
        mem.add(memio::serial::SerialTty);
        Self {
            memory: mem,
            program_counter: Default::default(),
            pointer: Default::default(),
            registers: Default::default(),
        }
    }
}

impl Cpu {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn load_program(&mut self, program: &[u8], offset: u16) {
        for (i, j) in ((offset as usize)..(offset as usize + program.len())).enumerate() {
            self.memory.set(j as u16, program[i]);
        }
    }

    pub fn cycle(&mut self) {
        let len = self.memory.get(0xF000).unwrap();
        let kind = self.memory.get(0xF001).unwrap();

        println!("L{} K{}", len, kind);
        self.memory.set(0xF002, b'H').unwrap();
        self.memory.set(0xF002, b'i').unwrap();
        self.memory.set(0xF002, b'\n').unwrap();
    }
}
