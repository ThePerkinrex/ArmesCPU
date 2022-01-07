use std::io::Write;

use super::{IoKind, IoPort};

pub trait Serial {
    const KIND: u8;

    // fn read(&mut self) -> u8;
    fn write(&mut self, v: u8);
}

impl<S> IoPort for S
where
    S: Serial,
{
    const KIND: u8 = S::KIND;

    const LENGTH: u8 = 3;

    fn get_io(&mut self, _: u16) -> Option<u8> {
        // Some(self.read())
        None
    }

    fn set_io(&mut self, _: u16, v: u8) -> Option<()> {
        self.write(v);
        Some(())
    }
}

pub struct SerialTty;

impl Serial for SerialTty {
    const KIND: u8 = IoKind::SerialTty as u8;

    // fn read(&mut self) -> u8 {
    // 	let stdin = std::io::stdin();
    //     let mut lock = stdin.lock();
    // 	let mut buf = [0];
    // 	lock.read_exact(&mut buf).expect("Stdin is readable");
    // 	buf[0]
    // }

    fn write(&mut self, v: u8) {
        let stdout = std::io::stdout();
        let mut lock = stdout.lock();
        lock.write_all(&[v]).expect("Stdout is writable");
        lock.flush().expect("Stdout is flushable");
    }
}

pub struct SerialNumberInterface;

impl Serial for SerialNumberInterface {
    const KIND: u8 = IoKind::SerialTty as u8;

    fn write(&mut self, v: u8) {
        println!(">> {}", v);
    }
}
