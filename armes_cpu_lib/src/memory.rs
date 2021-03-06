use std::fmt::Display;
use std::fmt;

pub struct Memory {
    pub values: Vec<usize>,
    addr_length: u32,
    data_length: u32,
}

impl Memory {
    pub fn new(addr_length: u32, data_length: u32) -> Memory {
        Memory {
            values: vec![0; 2_usize.pow(addr_length)],
            data_length: data_length,
            addr_length: addr_length
        }
    }

    pub fn set(&mut self, addr: usize, value: usize) {
        self.values[addr & (2_usize.pow(self.addr_length)-1)] = value & (2_usize.pow(self.data_length)-1);
        //println!("{}: {}\t{}: {}", addr, addr & (2_usize.pow(self.addr_length)-1), value,  value & (2_usize.pow(self.data_length)-1));
        //println!("V: {} - {} - {}:", self.values.len(), 2_usize.pow(self.addr_length), addr, /*self.get(addr)*/);
    }

    pub fn get(&self, addr: usize) -> usize {
        return self.values[addr]
    }

    pub fn get_lengths(&self) -> (u32, u32) {
        return (self.addr_length, self.data_length)
    }
}
impl Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut r = String::new();
        for i in 0..(2_usize.pow(self.addr_length)/4) {
            r += &format!("{:x}: {:x}\t", i, self.get(i));
            r += &format!("{:x}: {:x}\t", i+2_usize.pow(self.addr_length)/4, self.get(i+2_usize.pow(self.addr_length)/4));
            r += &format!("{:x}: {:x}\t", i+2_usize.pow(self.addr_length)/2, self.get(i+2_usize.pow(self.addr_length)/2));
            r += &format!("{:x}: {:x}\n", i+3*2_usize.pow(self.addr_length)/4, self.get(i+3*2_usize.pow(self.addr_length)/4));
        }
        write!(f, "{}",r)
    }
}