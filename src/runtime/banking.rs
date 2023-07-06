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

impl MemoryBankController for NoMBC {
    fn load_rom(rom_path: &str) -> Self
    where
        Self: Sized,
    {
        let mut f = File::open(rom_path).unwrap();
        let mut buf: [u8; 0x8000] = [0; 0x8000];
        f.read_exact(&mut buf).unwrap();
        NoMBC { rom: buf }
    }

    fn get_mem(&self, addr: u16) -> u8 {
        if (0x0000..=0x7FFF).contains(&addr) {
            self.rom[addr as usize]
        } else {
            0xFF
        }
    }

    fn set_mem(&mut self, addr: u16, _data: u8) {
        // ignore, we are a ROM
        println!("Invalid Write to NoMBC ROM with addr: {:04x}", addr);
    }
}

/// MBC1
#[derive(Debug)]
pub struct MBC1 {
    data: Vec<u8>,
    extended_rom: bool,
    has_ram: bool,
    ram: Vec<u8>,
    ram_enable: bool,
    rom_bank_number: u8,
    ram_bank_number: u8,
    rom_banking_mode_advanced: bool,
}

impl MemoryBankController for MBC1 {
    fn load_rom(rom_path: &str) -> Self
    where
        Self: Sized,
    {
        let mut f = File::open(rom_path).unwrap();
        let mut buf: Vec<u8> = vec![];
        let num_bytes = f.read_to_end(&mut buf).unwrap();

        MBC1 {
            has_ram: buf[0x0147] & 0b0000_0010 > 0, // $02 or $03
            ram: vec![0; match buf[0x0149] {
                0x0 => 0,
                0x1 => panic!("Invalid Ram Amount"),
                0x2 => 0x2000,
                0x3 => 0x8000,
                0x4 => 0x20000,
                0x5 => 0x10000,
                _ => unreachable!(),
            }],
            data: buf,
            extended_rom: num_bytes >= 1024 * 1024,
            ram_enable: false,
            rom_bank_number: 0,
            ram_bank_number: 0,
            rom_banking_mode_advanced: false,
        }
    }

    fn get_mem(&self, addr: u16) -> u8 {
        if (0x0000..=0x3FFF).contains(&addr) {
            // ROM Bank X0
            // Normally first 16Kb, but if
            if self.rom_banking_mode_advanced {
                self.data[0x4000 * ((self.ram_bank_number << 5) as usize) + addr as usize]
            } else {
                self.data[addr as usize]
            }
        } else if (0x4000..=0x7FFF).contains(&addr) {
            // ROM Bank 01-7F
            let effective_rom_banking_number = if self.rom_bank_number == 0 {
                1
            } else {
                self.rom_bank_number
            };
            let addr: usize = if self.extended_rom {
                let rom_number = (self.ram_bank_number << 5) + effective_rom_banking_number;

                0x4000 * (rom_number as usize) + addr as usize - 0x4000
            } else {
                0x4000 * (effective_rom_banking_number as usize) + addr as usize - 0x4000
            };

            self.data[addr]
        } else {
            // RAM Bank 00-03, if any
            if self.has_ram && self.ram_enable {
                self.ram[0x2000 * self.ram_bank_number as usize + addr as usize - 0xA000]
            } else {
                0xFF
            }
        }
    }

    fn set_mem(&mut self, addr: u16, data: u8) {
        if (0x0000..=0x1FFF).contains(&addr) {
            // RAM Enable
            if data & 0x0F == 0x0A {
                self.ram_enable = true;
                println!("RAM enable: {}", data);
            } else {
                self.ram_enable = false;
                println!("RAM disable: {}", data);
            }
        } else if (0x2000..=0x3FFF).contains(&addr) {
            // ROM Bank Number
            println!("Switched to ROM bank number {}", data & 0b0001_1111);
            self.rom_bank_number = data & 0b0001_1111;
        } else if (0x4000..=0x5FFF).contains(&addr) {
            // RAM Bank Number or Upper Bits of ROM Bank Number
            self.ram_bank_number = data & 0b0000_0011;
            println!(
                "Switched to RAM/Upper-Bits of ROM bank number {}",
                data & 0b0000_0011
            );
        } else if (0x6000..=0x7FFF).contains(&addr) {
            // Banking Mode Select
            self.rom_banking_mode_advanced = data & 0b0000_0001 > 0;
            println!("Advanced ROM Banking MBC1? {}", data & 0b0000_0001 > 0);
        } else {
            // RAM:
            if self.ram_enable && !self.extended_rom{
                self.ram[0x2000 * self.ram_bank_number as usize + addr as usize - 0xA000] = data;   
            }
        }
    }
}
