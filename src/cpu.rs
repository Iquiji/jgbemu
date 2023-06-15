pub const BOOT_ROM_GB: [u8; 256] = [
    0x31, 0xfe, 0xff, 0xaf, 0x21, 0xff, 0x9f, 0x32, 0xcb, 0x7c, 0x20, 0xfb, 0x21, 0x26, 0xff, 0x0e,
    0x11, 0x3e, 0x80, 0x32, 0xe2, 0x0c, 0x3e, 0xf3, 0xe2, 0x32, 0x3e, 0x77, 0x77, 0x3e, 0xfc, 0xe0,
    0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1a, 0xcd, 0x95, 0x00, 0xcd, 0x96, 0x00, 0x13, 0x7b,
    0xfe, 0x34, 0x20, 0xf3, 0x11, 0xd8, 0x00, 0x06, 0x08, 0x1a, 0x13, 0x22, 0x23, 0x05, 0x20, 0xf9,
    0x3e, 0x19, 0xea, 0x10, 0x99, 0x21, 0x2f, 0x99, 0x0e, 0x0c, 0x3d, 0x28, 0x08, 0x32, 0x0d, 0x20,
    0xf9, 0x2e, 0x0f, 0x18, 0xf3, 0x67, 0x3e, 0x64, 0x57, 0xe0, 0x42, 0x3e, 0x91, 0xe0, 0x40, 0x04,
    0x1e, 0x02, 0x0e, 0x0c, 0xf0, 0x44, 0xfe, 0x90, 0x20, 0xfa, 0x0d, 0x20, 0xf7, 0x1d, 0x20, 0xf2,
    0x0e, 0x13, 0x24, 0x7c, 0x1e, 0x83, 0xfe, 0x62, 0x28, 0x06, 0x1e, 0xc1, 0xfe, 0x64, 0x20, 0x06,
    0x7b, 0xe2, 0x0c, 0x3e, 0x87, 0xe2, 0xf0, 0x42, 0x90, 0xe0, 0x42, 0x15, 0x20, 0xd2, 0x05, 0x20,
    0x4f, 0x16, 0x20, 0x18, 0xcb, 0x4f, 0x06, 0x04, 0xc5, 0xcb, 0x11, 0x17, 0xc1, 0xcb, 0x11, 0x17,
    0x05, 0x20, 0xf5, 0x22, 0x23, 0x22, 0x23, 0xc9, 0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b,
    0x03, 0x73, 0x00, 0x83, 0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e,
    0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99, 0xbb, 0xbb, 0x67, 0x63, 0x6e, 0x0e, 0xec, 0xcc,
    0xdd, 0xdc, 0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e, 0x3c, 0x42, 0xb9, 0xa5, 0xb9, 0xa5, 0x42, 0x3c,
    0x21, 0x04, 0x01, 0x11, 0xa8, 0x00, 0x1a, 0x13, 0xbe, 0x20, 0xfe, 0x23, 0x7d, 0xfe, 0x34, 0x20,
    0xf5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xfb, 0x86, 0x20, 0xfe, 0x3e, 0x01, 0xe0, 0x50,
];


/*
Memory Map:
0000 - 3FFF	16 KiB ROM bank 00 From cartridge, usually a fixed bank
4000 - 7FFF	16 KiB ROM Bank 01~NN	From cartridge, switchable bank via mapper (if any)
8000 - 9FFF	8 KiB Video RAM (VRAM)	In CGB mode, switchable bank 0/1
A000 - BFFF	8 KiB External RAM	From cartridge, switchable bank if any
C000 - CFFF	4 KiB Work RAM (WRAM)
D000 - DFFF	4 KiB Work RAM (WRAM)	In CGB mode, switchable bank 1~7
E000 - FDFF	Mirror of C000~DDFF (ECHO RAM)	Nintendo says use of this area is prohibited.
FE00 - FE9F	Sprite attribute table (OAM)
FEA0 - FEFF	Not Usable	Nintendo says use of this area is prohibited
FF00 - FF7F	I/O Registers
FF80 - FFFE	High RAM (HRAM)
FFFF - FFFF	Interrupt Enable register (IE)
 */

/*
Interrupt Locations:
0000h,0008h,0010h,0018h,0020h,0028h,0030h,0038h – For RST instruction of CPU.
0040h,0048h,0050h,0058h,0060h – Interrupt Vectors (VBL,LCD,Timer,Serial,Joypad)
*/

/*
AF	A	-	Accumulator & Flags
BC	B	C	BC
DE	D	E	DE
HL	H	L	HL
SP	-	-	Stack Pointer
PC	-	-	Program Counter/Pointer

The Flags Register (lower 8 bits of AF register)
Bit	Name	Explanation
7	z	Zero flag
6	n	Subtraction flag (BCD)
5	h	Half Carry flag (BCD)
4	c	Carry flag

 */
#[derive(Debug, Clone, Default)]
pub struct Registers {
    status: u8,
    stack_pointer: u16,
    program_counter: u16,
    reg_a: u8,
    reg_b: u8,
    reg_c: u8,
    reg_d: u8,
    reg_e: u8,
    reg_h: u8,
    reg_l: u8,
}
impl Registers {
    pub fn get_pc(&self) -> u16 {
        self.program_counter
    }
    pub fn get_stack_pointer(&self) -> u16 {
        self.stack_pointer
    }
    pub fn get_status(&self) -> u8 {
        self.status
    }
    pub fn get_status_zero(&self) -> bool {
        (self.status & 0b1000_0000) != 0
    }
    pub fn get_status_carry(&self) -> bool {
        (self.status & 0b0001_0000) != 0
    }
    pub fn get_reg_a(&self) -> u8 {
        self.reg_a
    }
    pub fn get_reg_b(&self) -> u8 {
        self.reg_b
    }
    pub fn get_reg_c(&self) -> u8 {
        self.reg_c
    }
    pub fn get_reg_d(&self) -> u8 {
        self.reg_d
    }
    pub fn get_reg_e(&self) -> u8 {
        self.reg_e
    }
    pub fn get_reg_h(&self) -> u8 {
        self.reg_h
    }
    pub fn get_reg_l(&self) -> u8 {
        self.reg_l
    }
    pub fn get_reg_af(&self) -> u16 {
        u16::from_le_bytes([self.status, self.reg_a])
    }
    pub fn get_reg_bc(&self) -> u16 {
        u16::from_le_bytes([self.reg_c, self.reg_b])
    }
    pub fn get_reg_de(&self) -> u16 {
        u16::from_le_bytes([self.reg_e, self.reg_d])
    }
    pub fn get_reg_hl(&self) -> u16 {
        u16::from_le_bytes([self.reg_l, self.reg_h])
    }
    pub fn set_pc(&mut self, val: u16) {
        self.program_counter = val;
    }
    pub fn set_stack_pointer(&mut self, val: u16) {
        self.stack_pointer = val;
    }
    pub fn set_status(&mut self, val: u8) {
        self.status = val;
    }
    pub fn set_status_zero(&mut self, set_bit: bool) {
        self.status &= if set_bit { 0b1000_0000 } else {0};
    }
    pub fn set_status_carry(&mut self, set_bit: bool) {
        self.status &= if set_bit { 0b0001_0000 } else {0};
    }
    pub fn set_reg_a(&mut self, val: u8) {
        self.reg_a = val;
    }
    pub fn set_reg_b(&mut self, val: u8) {
        self.reg_b = val;
    }
    pub fn set_reg_c(&mut self, val: u8) {
        self.reg_c = val;
    }
    pub fn set_reg_d(&mut self, val: u8) {
        self.reg_d = val;
    }
    pub fn set_reg_e(&mut self, val: u8) {
        self.reg_e = val;
    }
    pub fn set_reg_h(&mut self, val: u8) {
        self.reg_h = val;
    }
    pub fn set_reg_l(&mut self, val: u8) {
        self.reg_l = val;
    }
    pub fn set_reg_af(&mut self, val: u16) {
        [self.status, self.reg_a] = val.to_le_bytes();
    }
    pub fn set_reg_bc(&mut self, val: u16) {
        [self.reg_c, self.reg_b] = val.to_le_bytes();
    }
    pub fn set_reg_de(&mut self, val: u16) {
        [self.reg_e, self.reg_d] = val.to_le_bytes();
    }
    pub fn set_reg_hl(&mut self, val: u16) {
        [self.reg_l, self.reg_h] = val.to_le_bytes();
    }
}

#[derive(Debug, Clone, Default)]
pub struct CPU {
    pub reg: Registers,
}
