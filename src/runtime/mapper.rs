pub trait MemoryBankController{
    pub fn load_rom(rom_path: &str) -> Self;
    pub fn get_mem(&self, addr: u16) -> u8;
    pub fn get_mem(&mut self, addr: u16, data: u8);
}

/// No Memory Bank Controller
pub struct NoMBC{
    rom: [u8; 0x8000],
}