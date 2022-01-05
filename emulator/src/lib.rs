mod memio;

pub struct Cpu {
	memory: [u8; 0xEFFF], 
	program_counter: u16,
	pointer: u16,
	registers: [u8; 0xF],
}
impl Default for Cpu {
    fn default() -> Self {
        Self { memory: [0; 0xEFFF], program_counter: Default::default(), pointer: Default::default(), registers: Default::default() }
    }
}

impl Cpu {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn load_program(&mut self, program: &[u8], offset: u16) {
		self.memory[(offset as usize)..(offset as usize + program.len())].copy_from_slice(program)
	}


}