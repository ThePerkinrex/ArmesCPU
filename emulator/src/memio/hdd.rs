use std::{
    fs::File,
    io::{Error, Read, Seek, SeekFrom, Write, self},
    // mem::size_of,
    path::Path,
};
use thiserror::Error;

use super::{IoKind, IoPort};

pub struct Hdd {
    file: File,
    disk_size: u64,
    addr: u64,
    read_cache: Option<u8>,
}

#[derive(Debug, Error)]
pub enum HddReadError {
    #[error("HDD read error: IO error {0}")]
    IoError(#[from] Error),
    #[error("HDD file format is wrong")]
    FormatError,
    #[error("HDD specified isn't a file")]
    IsFileError,
}

impl Hdd {
	pub fn build<P: AsRef<Path>>(path: P, size: u64) -> Result<(), io::Error>{
		let mut f = File::create(path)?;
		f.write_all(&size.to_le_bytes())
	}

    pub fn read<P: AsRef<Path>>(path: P) -> Result<Self, HddReadError> {
        let path = path.as_ref();
        if !path.is_file() {
            return Err(HddReadError::IsFileError);
        }
        let mut f = File::open(path)?;
        let len = f.metadata()?.len();
        if len < 8 {
            Err(HddReadError::FormatError)
        } else {
            // let len = len - 8;
            let mut disk_size = [0; 8];
            f.read_exact(&mut disk_size)?;
            let disk_size = u64::from_le_bytes(disk_size);
            Ok(Self {
                disk_size,
                file: f,
                addr: 0,
                read_cache: None,
            })
        }
    }

    fn read_byte(&mut self) -> u8 {
        if let Some(v) = self.read_cache {
            v
        } else {
            self.file
                .seek(SeekFrom::Start(8 + self.addr))
                .expect("Error seeking HDD file");
            let mut b = [0; 1];
            self.file
                .read_exact(&mut b)
                .expect("Error reading HDD file");
            let b = b[0];
            self.read_cache = Some(b);
            b
        }
    }

    fn write_byte(&mut self, v: u8) {
        self.file
            .seek(SeekFrom::Start(8 + self.addr))
            .expect("Error seeking HDD file");
        self.file
            .write_all(&[v])
            .expect("Error writing to HDD file");
        self.read_cache = Some(v);
    }

    fn set_addr(&mut self, addr: u64) {
        self.addr = addr;
        self.read_cache = None;
    }
}

impl IoPort for Hdd {
    const KIND: u8 = IoKind::HDD as u8;

    // #0  R  DISK_SIZE u64
    // #8  RW ADDR u64
    // #16 RW DATA u8
    const LENGTH: u8 = 17;

    fn get_io(&mut self, idx: u16) -> Option<u8> {
        match idx {
            0..=7 => Some((self.disk_size >> (7 - idx)) as u8 & 1),
            8..=15 => Some((self.addr >> (15 - idx)) as u8 & 1),
            16 => Some(self.read_byte()),
            _ => unreachable!(),
        }
    }

    fn set_io(&mut self, idx: u16, v: u8) -> Option<()> {
        match idx {
            0..=7 => None,
            8..=15 => {
                let i = u64::MAX ^ (0xff << ((idx - 8) * 8));
                self.set_addr((self.addr & i)  + ((v as u64) << ((idx - 8) * 8)));
                Some(())
            }
            16 => {
                self.write_byte(v);
                Some(())
            }
            _ => unreachable!(),
        }
    }
}
