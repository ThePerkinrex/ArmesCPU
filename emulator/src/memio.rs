pub mod hdd;
pub mod serial;

pub trait MemoryIO {
    fn last_idx(&self) -> Option<u16>;
    fn get(&mut self, idx: u16) -> Option<u8>;
    fn set(&mut self, idx: u16, v: u8) -> Option<()>;
}

/// L must be in the u16 range
impl<const L: usize> MemoryIO for [u8; L] {
    fn last_idx(&self) -> Option<u16> {
        Some((L - 1) as u16)
    }

    fn get(&mut self, idx: u16) -> Option<u8> {
        if (0..=self.last_idx().unwrap()).contains(&idx) {
            Some(self[idx as usize])
        } else {
            None
        }
    }

    fn set(&mut self, idx: u16, v: u8) -> Option<()> {
        if (0..=self.last_idx().unwrap()).contains(&idx) {
            self[idx as usize] = v;
            Some(())
        } else {
            None
        }
    }
}

// impl<M> MemoryIO for Box<M> where M: MemoryIO {
//     fn last_idx(&self) -> Option<u16> {
//         self.deref().last_idx()
//     }

//     fn get(&mut self, idx: u16) -> Option<u8> {
//         self.deref().get(idx)
//     }

//     fn set(&mut self, idx: u16, v: u8) -> Option<()> {
// 		self.deref_mut().set(idx, v)
//     }
// }

#[derive(Default)]
pub struct MemorySection {
    data: Vec<(u16, Box<dyn MemoryIO>)>,
    last_idx: Option<u16>,
}

impl MemorySection {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add<M: MemoryIO + 'static>(&mut self, mem: M) {
        // let  i = self.data.iter().enumerate().take_while(|(_, (off, _))| &offset < off).last().map(|(i, _)|i).unwrap_or(0);
        // self.data.insert(i, (offset, Box::new(mem)));
        if let Some(last_idx) = mem.last_idx() {
            let off = self.last_idx.map(|x| x + 1).unwrap_or(0);
            self.last_idx = Some(off + last_idx);
            self.data.push((off, Box::new(mem)));
        } else {
            panic!("Can't add memory without length")
        }
    }
}

impl MemoryIO for MemorySection {
    fn last_idx(&self) -> Option<u16> {
        self.last_idx
    }

    fn get(&mut self, idx: u16) -> Option<u8> {
        for (off, mem) in &mut self.data {
            if (*off..=(*off + mem.last_idx().unwrap())).contains(&idx) {
                // println!("{:04X} {:04X} {:04X}", idx, off, idx - *off);
                return mem.get(idx - *off);
            }
        }
        None
    }

    fn set(&mut self, idx: u16, val: u8) -> Option<()> {
        for (off, mem) in &mut self.data {
            if (*off..=(*off + mem.last_idx().unwrap())).contains(&idx) {
                return mem.set(idx - *off, val);
            }
        }
        None
    }
}

pub trait IoPort {
    const KIND: u8;
    const LENGTH: u8;

    fn get_io(&mut self, idx: u16) -> Option<u8>;
    fn set_io(&mut self, idx: u16, v: u8) -> Option<()>;
}

impl<T> MemoryIO for T
where
    T: IoPort,
{
    fn last_idx(&self) -> Option<u16> {
        Some(Self::LENGTH as u16 - 1)
    }

    fn get(&mut self, idx: u16) -> Option<u8> {
        match idx {
            0 => Some(Self::LENGTH),
            1 => Some(Self::KIND),
            x if x < Self::LENGTH as u16 => self.get_io(x - 2),
            _ => None,
        }
    }

    fn set(&mut self, idx: u16, v: u8) -> Option<()> {
        match idx {
            0 => None,
            1 => None,
            x if x < Self::LENGTH as u16 => self.set_io(x - 2, v),
            _ => None,
        }
    }
}

#[repr(u8)]
pub enum IoKind {
    SerialTty,
    Keyboard,
    HDD,
}
