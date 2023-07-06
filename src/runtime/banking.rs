use std::fmt::Debug;
use std::fs::File;
use std::io::Read;

pub trait MemoryBankController: Debug + Send {
    fn load_rom(rom_path: &str) -> Self
    where
        Self: Sized;
    fn get_mem(&self, addr: u16) -> u8;
    fn set_mem(&mut self, addr: u16, data: u8);
}

/// No Memory Bank Controller
#[derive(Debug)]
pub struct NoMBC {
    rom: [u8; 0x8000],
}

impl MemoryBankController for NoMBC{
    fn load_rom(rom_path: &str) -> Self
    where
        Self: Sized {
        let mut f = File::open(rom_path).unwrap();
        let mut buf: [u8; 0x8000] = [0; 0x8000];
        f.read_exact(&mut buf).unwrap();
        NoMBC { rom: buf }
    }

    fn get_mem(&self, addr: u16) -> u8 {
        if (0x0000..=0x7FFF).contains(&addr){
            self.rom[addr as usize]
        } else{
            0xFF
        }
    }

    fn set_mem(&mut self, addr: u16, _data: u8) {
        // ignore, we are a ROM
        println!("Invalid Write to NoMBC ROM with addr: {:04x}", addr);
        return;
    }
}