use std::{fs::File, io::Read, sync::{Arc, Mutex}, thread::sleep, time::Duration};

use log::error;

use crate::{
    runtime::graphics::GraphicsController,
    runtime::instr::{
        ArithmeticInstruction, ControlInstruction, Instruction, JumpInstruction, Location16Bit,
        Location8Bit, MemoryInstruction, PRes, ShiftRotateInstruction, SingleBitInstruction,
    }, ui::UserInput,
};

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
    pub fn get_status_half_carry(&self) -> bool {
        (self.status & 0b0010_0000) != 0
    }
    pub fn get_status_negative(&self) -> bool {
        (self.status & 0b0100_0000) != 0
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
    pub fn set_status_half_carry(&mut self, set_bit: bool) {
        if set_bit {
            self.status |= 0b0010_0000;
        } else {
            self.status &= 0b1101_1111;
        }
    }
    pub fn set_status_negative(&mut self, set_bit: bool) {
        if set_bit {
            self.status |= 0b0100_0000;
        } else {
            self.status &= 0b1011_1111;
        }
    }
    pub fn set_status_zero(&mut self, set_bit: bool) {
        if set_bit {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }
    pub fn set_status_carry(&mut self, set_bit: bool) {
        if set_bit {
            self.status |= 0b0001_0000;
        } else {
            self.status &= 0b1110_1111;
        }
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
        self.status &= 0xF0;
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

#[derive(Debug, Clone)]
pub struct CPU {
    pub cycle: u64,
    pub halted_flag: bool,
    /// TODO:
    pub stopped_flag: bool,
    pub last_tima_increment_cycle: u64,
    pub last_div_increment_cycle: u64,
    pub enable_interrupt_req: bool,
    pub reg: Registers,
    pub mem: [u8; 65536],
    pub graphics_controller: GraphicsController,
}

impl CPU {
    pub fn new(screen_buffer: Arc<Mutex<Box<[[u8; 160]; 144]>>>) -> Self {
        CPU {
            cycle: 0,
            halted_flag: false,
            stopped_flag: false,
            last_tima_increment_cycle: 0,
            last_div_increment_cycle: 0,
            enable_interrupt_req: false,
            reg: Registers::default(),
            mem: [0; 65536],
            graphics_controller: GraphicsController::new(screen_buffer),
        }
    }
}

impl CPU {
    pub fn load_boot_rom(&mut self) {
        for (i, byte) in BOOT_ROM_GB.iter().enumerate() {
            self.mem[i] = *byte;
        }
        if self.mem[0x0147] != 0{
            error!("Cartridge Mapper Type unsuported... running regardless:");
        }
    }
    pub fn load_blargg_test_rom(&mut self, path: &str) {
        let mut f = File::open(path).unwrap();
        let mut buf: Vec<u8> = vec![];
        let _num_bytes = f.read_to_end(&mut buf).unwrap();
        // eprintln!("{:?}", num_bytes);
        for (idx, byte) in buf.iter().enumerate().skip(100) {
            self.mem[idx] = *byte;
        }
    }
    pub fn unload_boot_rom(&mut self, path: &str) {
        let mut f = File::open(path).unwrap();
        let mut buf: Vec<u8> = vec![];
        let _num_bytes = f.read_to_end(&mut buf).unwrap();
        // eprintln!("{:?}", num_bytes);
        for (idx, byte) in buf.iter().enumerate() {
            if *byte != 0 {
                self.mem[idx] = *byte;
            }
            if idx > 100 {
                break;
            }
        }
    }
    pub fn run_till_0x100(&mut self) {
        while self.reg.get_pc() != 0x100 {
            self.do_enable_interrupts_on_req();
            self.handle_interrupts();
            self.handle_timer();
            let next_instr: Instruction = self.next_instr();
            self.execute_instr(next_instr.clone());
            if self.graphics_controller.tick(self.cycle) {
                self.set_mem(0xFF0F, self.get_mem(0xFF0F) | 0b0000_0010);
            };
        }
        assert_eq!(self.reg.get_reg_a(), 0x01);
        assert_eq!(self.reg.get_reg_b(), 0x00);
        assert_eq!(self.reg.get_reg_c(), 0x13);
        assert_eq!(self.reg.get_reg_d(), 0x00);
        assert_eq!(self.reg.get_reg_e(), 0xd8);
        assert_eq!(self.reg.get_reg_h(), 0x01);
        assert_eq!(self.reg.get_reg_l(), 0x4d);
        assert!(self.reg.get_status_zero());
        assert!(!self.reg.get_status_negative());
    }
    pub fn get_mem(&self, addr: u16) -> u8 {
        if (0xFF40..=0xFF4B).contains(&addr)
            || (0x8000..=0x9FFF).contains(&addr)
            || (0xFE00..=0xFE9F).contains(&addr)
        {
            self.graphics_controller.memory_get(addr)
        } else {
            self.mem[addr as usize]
        }
    }
    pub fn set_mem(&mut self, addr: u16, byte: u8) {
        // if addr > 0xDD00 && addr < 0xDD05{
        //     println!("[{:04x}] = {:02x}", addr, byte);
        // }
        // if 0xdd04 >= addr && addr >= 0xdd00{
        //     println!("!!!! {:#04x}={:02x}", addr, byte);
        // }
        // if addr == 0xdd02{
        //     println!("!!!! 0xdd02={:02x}", byte);
        // }
        // OAM DMA Transfer
        if addr == 0xFF46 {
            println!("OAM DMA Transfer from {:04x}", (byte as u16) << 8);
            for i in 0_u16..0x100{
                let src = ((byte as u16) << 8) + i;
                let dst = 0xFE00 + i;
                self.set_mem(dst, self.get_mem(src));
            }

            return;
        }

        if (0xFFB6..=0xFFBF).contains(&addr){
            println!("WRITING {:02x} TO {:04x}", byte, addr)
        }

        if (0xFF40..=0xFF4B).contains(&addr)
            || (0x8000..=0x9FFF).contains(&addr)
            || (0xFE00..=0xFE9F).contains(&addr)
        {
            self.graphics_controller.memory_set(addr, byte)
        } else {
            self.mem[addr as usize] = byte;
        }
    }
    pub fn get_loc8(&self, loc: Location8Bit) -> u8 {
        match loc {
            Location8Bit::A => self.reg.get_reg_a(),
            Location8Bit::B => self.reg.get_reg_b(),
            Location8Bit::C => self.reg.get_reg_c(),
            Location8Bit::D => self.reg.get_reg_d(),
            Location8Bit::E => self.reg.get_reg_e(),
            Location8Bit::H => self.reg.get_reg_h(),
            Location8Bit::L => self.reg.get_reg_l(),
            Location8Bit::Indirect => self.get_mem(self.reg.get_reg_hl()),
        }
    }
    pub fn get_loc16(&self, loc: Location16Bit) -> u16 {
        match loc {
            Location16Bit::AF => self.reg.get_reg_af(),
            Location16Bit::BC => self.reg.get_reg_bc(),
            Location16Bit::DE => self.reg.get_reg_de(),
            Location16Bit::HL => self.reg.get_reg_hl(),
            Location16Bit::SP => self.reg.get_stack_pointer(),
        }
    }
    pub fn set_loc8(&mut self, loc: Location8Bit, data: u8) {
        match loc {
            Location8Bit::A => self.reg.set_reg_a(data),
            Location8Bit::B => self.reg.set_reg_b(data),
            Location8Bit::C => self.reg.set_reg_c(data),
            Location8Bit::D => self.reg.set_reg_d(data),
            Location8Bit::E => self.reg.set_reg_e(data),
            Location8Bit::H => self.reg.set_reg_h(data),
            Location8Bit::L => self.reg.set_reg_l(data),
            Location8Bit::Indirect => self.set_mem(self.reg.get_reg_hl(), data),
        }
    }
    pub fn set_loc16(&mut self, loc: Location16Bit, data: u16) {
        match loc {
            Location16Bit::AF => self.reg.set_reg_af(data),
            Location16Bit::BC => self.reg.set_reg_bc(data),
            Location16Bit::DE => self.reg.set_reg_de(data),
            Location16Bit::HL => self.reg.set_reg_hl(data),
            Location16Bit::SP => self.reg.set_stack_pointer(data),
        }
    }
    pub fn print_status(&self) -> String {
        format!("A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}",
            self.reg.get_reg_a(),
            self.reg.get_status(),
            self.reg.get_reg_b(),
            self.reg.get_reg_c(),
            self.reg.get_reg_d(),
            self.reg.get_reg_e(),
            self.reg.get_reg_h(),
            self.reg.get_reg_l(),
            self.reg.get_stack_pointer(),
            self.reg.get_pc(),
            self.get_mem(self.reg.get_pc()),
            self.get_mem(self.reg.get_pc().wrapping_add(1)),
            self.get_mem(self.reg.get_pc().wrapping_add(2)),
            self.get_mem(self.reg.get_pc().wrapping_add(3)),
        )
    }

    pub fn disable_all_interrupts(&mut self) {
        self.set_mem(0xFFFF, 0x00);
    }
    pub fn enable_all_interrupts(&mut self) {
        self.enable_interrupt_req = true;
    }
    pub fn do_enable_interrupts_on_req(&mut self) {
        if self.enable_interrupt_req {
            self.set_mem(0xFFFF, 0x1F);
            self.enable_interrupt_req = false;
        }
    }
    pub fn handle_interrupts(&mut self) {
        // Interrupt requested?

        // Superduper hacked version of this
        // TCAGBD.pdf: 4.10:
        if self.halted_flag
            && (self.get_mem(0xFFFF) & self.get_mem(0xFF0F)) == 0
            && self.get_mem(0xFF0F) != 0
        {
            self.halted_flag = false;
            return;
        }

        let allowed_interrupts = self.get_mem(0xFFFF) & self.get_mem(0xFF0F);
        if allowed_interrupts != 0 {
            // println!("INTERRUPT! Allowed: {:08b}", self.get_mem(0xFFFF) & self.get_mem(0xFF0F));
            // println!("INTERRUPT! Requested (Not necessarily allowed): {:08b}", self.get_mem(0xFF0F));
            self.halted_flag = false;
            let address = if (allowed_interrupts & 0b0000_0001) == 0b0000_0001 {
                // VBlank - INT $40
                self.set_mem(0xFF0F, self.get_mem(0xFF0F) & (0xFF ^ 0b0000_0001));
                0x40
            } else if (allowed_interrupts & 0b0000_0010) == 0b0000_0010 {
                // LCD STAT - INT $48
                self.set_mem(0xFF0F, self.get_mem(0xFF0F) & (0xFF ^ 0b0000_0010));
                0x48
            } else if (allowed_interrupts & 0b0000_0100) == 0b0000_0100 {
                // Timer - INT $50
                self.set_mem(0xFF0F, self.get_mem(0xFF0F) & (0xFF ^ 0b0000_0100));
                0x50
            } else if (allowed_interrupts & 0b0000_1000) == 0b0000_1000 {
                // Serial - INT $58
                self.set_mem(0xFF0F, self.get_mem(0xFF0F) & (0xFF ^ 0b0000_1000));
                0x58
            } else if (allowed_interrupts & 0b0001_0000) == 0b0001_0000 {
                // Joypad - INT $60
                self.set_mem(0xFF0F, self.get_mem(0xFF0F) & (0xFF ^ 0b0001_0000));
                0x60
            } else {
                unreachable!()
            };
            self.disable_all_interrupts();
            // println!("INTERRUPT! After get right: {:08b}", self.get_mem(0xFF0F));

            self.reg
                .set_stack_pointer((self.reg.get_stack_pointer() as i32 - 2) as u16);
            let [lower_byte, upper_byte] = self.reg.get_pc().to_le_bytes();
            self.set_mem(self.reg.get_stack_pointer(), lower_byte);
            self.set_mem(self.reg.get_stack_pointer() + 1, upper_byte);

            // println!("INTERRUPT {:X} -- BACK ADDR: {:x} {:x}", address, self.get_mem(self.reg.get_stack_pointer()), self.get_mem(self.reg.get_stack_pointer() + 1));

            self.reg.set_pc(address);
            self.cycle += 5 * 4;
        }
    }
    pub fn handle_timer(&mut self) {
        // TODO:
        let timer_modulo = self.get_mem(0xFF06);
        let timer_control = self.get_mem(0xFF07);
        let timer_enable_flag = (timer_control & 0b0000_0100) == 0b0000_0100;
        let timer_speed_select = timer_control & 0b0000_0011;

        let timer_speed_divisor = [10, 4, 6, 8][timer_speed_select as usize];

        let delta_tima = self.cycle - self.last_tima_increment_cycle;

        if timer_enable_flag {
            let current = self.get_mem(0xFF05);
            let new = current.wrapping_add(((delta_tima) >> timer_speed_divisor) as u8);
            self.set_mem(0xFF05, new);
            if new != current {
                // println!("Cycle: {}, TIMA: {:02x}, divisor: {}, delta: {}", self.cycle, self.get_mem(0xFF05), 1 << timer_speed_divisor, delta_tima);
                self.last_tima_increment_cycle +=
                    ((delta_tima) >> timer_speed_divisor) * (1 << timer_speed_divisor);
            }
            if new < current {
                // println!("Setting Timer Interrupt Request.");
                // println!("allowed Interrupts: {:08b}", self.get_mem(0xFFFF));
                // println!(" + Speed: {}", 0x01 << timer_speed_divisor);
                self.set_mem(0xFF05, timer_modulo);
                self.set_mem(0xFF0F, self.get_mem(0xFF0F) | 0b0000_0100);
            }
        } else {
            // this is all a big hack!
            self.last_tima_increment_cycle = self.cycle;
        }

        // let current = self.get_mem(0xFF04);
        let new = (self.cycle % 256) as u8; //current.wrapping_add(((self.cycle - self.last_div_increment_cycle) / timer_speed_divisor) as u8);
        self.set_mem(0xFF04, new);
        self.last_div_increment_cycle = self.cycle;
    }

    pub fn handle_user_input(&mut self, user_input: UserInput) {
        let select_action = self.get_mem(0xFF00) & 0b0010_0000 > 0;
        let direction_action = self.get_mem(0xFF00) & 0b0001_0000 > 0;

        let mut bits = self.get_mem(0xFF00) | 0b0000_1111;
        if !direction_action {
            bits ^= !if user_input.start {0b0000_1000} else {0};
            bits ^= !if user_input.select {0b0000_0100} else {0};
            bits ^= !if user_input.b {0b0000_0010} else {0};
            bits ^= !if user_input.a {0b0000_0001} else {0};
        } 
        if !select_action {
            bits &= !if user_input.down {0b0000_1000} else {0};
            bits &= !if user_input.up {0b0000_0100} else {0};
            bits &= !if user_input.left {0b0000_0010} else {0};
            bits &= !if user_input.right {0b0000_0001} else {0};
        }

        self.set_mem(0xFF00, bits);
        // println!("{:08b}", bits);
        
        if (bits & 0b0000_1111) != 0b0000_1111{
            // println!("{:?}", user_input);
            self.stopped_flag = false;


            // TODO: Joypad Interrupt
            // self.set_mem(0xFF0F, self.get_mem(0xFF0F) | 0b0001_0000);
        }
    }
    pub fn next_instr(&mut self) -> Instruction {
        if self.halted_flag {
            return Instruction {
                cycles: 4,
                itype: crate::runtime::instr::InstructionType::Control(ControlInstruction::HALT),
            };
        }

        match Instruction::parse_from_bytes(
            &self.mem[(self.reg.get_pc() as usize)..(self.reg.get_pc() as usize + 1)],
        ) {
            PRes::NeedMoreBytes => {
                let instr_res = Instruction::parse_from_bytes(
                    &self.mem[(self.reg.get_pc()) as usize..(self.reg.get_pc() + 1 + 1) as usize],
                );
                match instr_res {
                    PRes::Instruction(instr) => {
                        // println!(
                        //     "CB Instruction at byte {:x} : {:?}",
                        //     self.reg.get_pc(),
                        //     instr.itype
                        // );
                        self.reg.set_pc(self.reg.get_pc() + 2);
                        instr
                    }
                    _ => panic!(),
                }
            }
            PRes::NeedMoreBytesSpecific(n_bytes) => {
                let instr_res = Instruction::parse_from_bytes(
                    &self.mem[(self.reg.get_pc()) as usize
                        ..(self.reg.get_pc() + n_bytes as u16 + 1) as usize],
                );
                match instr_res {
                    PRes::Instruction(instr) => {
                        // println!(
                        //     "Instruction at byte {:x} : {:?}",
                        //     self.reg.get_pc(),
                        //     instr.itype
                        // );
                        self.reg.set_pc(self.reg.get_pc() + 1 + n_bytes as u16);
                        instr
                    }
                    _ => panic!(),
                }
            }
            PRes::Instruction(instr) => {
                // println!(
                //     "Instruction at byte {:x} : {:?}",
                //     self.reg.get_pc(),
                //     instr.itype
                // );
                self.reg.set_pc(self.reg.get_pc() + 1);
                instr
            }
        }
    }
    pub fn execute_instr(&mut self, instr: Instruction) {
        match instr.itype {
            crate::runtime::instr::InstructionType::Mem(mem_instr) => {
                self.execute_mem_instr(instr.cycles, mem_instr)
            }
            crate::runtime::instr::InstructionType::Arithmetic(arith_inst) => {
                self.execute_arithmetic_instr(instr.cycles, arith_inst)
            }
            crate::runtime::instr::InstructionType::ShiftRotate(shift_instr) => {
                self.execute_shift_rotate_instr(instr.cycles, shift_instr)
            }
            crate::runtime::instr::InstructionType::SingleBit(single_bit_instr) => {
                self.execute_single_bit_instr(instr.cycles, single_bit_instr)
            }
            crate::runtime::instr::InstructionType::Control(control_instr) => {
                self.execute_control_instr(instr.cycles, control_instr)
            }
            crate::runtime::instr::InstructionType::Jump(jump_instr) => {
                self.execute_jump_instr(instr.cycles, jump_instr)
            }
            crate::runtime::instr::InstructionType::Unknown => panic!(),
        }

        if instr.cycles == 64 {
            // We do this in the conditional
        } else if instr.cycles == 128 {
            // Halt instruction reached here, so we add 4 and then we give back nop every call ig
            self.cycle += 4;
        } else {
            self.cycle += instr.cycles as u64;
        }
    }
    pub fn execute_mem_instr(&mut self, cycles: u8, instr: MemoryInstruction) {
        match instr {
            MemoryInstruction::LoadImmediate8(location8, constant) => match location8 {
                crate::runtime::instr::Location8Bit::A => self.reg.set_reg_a(constant),
                crate::runtime::instr::Location8Bit::B => self.reg.set_reg_b(constant),
                crate::runtime::instr::Location8Bit::C => self.reg.set_reg_c(constant),
                crate::runtime::instr::Location8Bit::D => self.reg.set_reg_d(constant),
                crate::runtime::instr::Location8Bit::E => self.reg.set_reg_e(constant),
                crate::runtime::instr::Location8Bit::H => self.reg.set_reg_h(constant),
                crate::runtime::instr::Location8Bit::L => self.reg.set_reg_l(constant),
                crate::runtime::instr::Location8Bit::Indirect => {
                    self.set_mem(self.reg.get_reg_hl(), constant)
                }
            },
            MemoryInstruction::LoadImmediate16(location16, constant) => match location16 {
                crate::runtime::instr::Location16Bit::AF => self.reg.set_reg_af(constant),
                crate::runtime::instr::Location16Bit::BC => self.reg.set_reg_bc(constant),
                crate::runtime::instr::Location16Bit::DE => self.reg.set_reg_de(constant),
                crate::runtime::instr::Location16Bit::HL => self.reg.set_reg_hl(constant),
                crate::runtime::instr::Location16Bit::SP => self.reg.set_stack_pointer(constant),
            },
            MemoryInstruction::LoadIndirectToA(location16, increment_flag, decrement_flag) => {
                match location16 {
                    crate::runtime::instr::Location16Bit::BC => {
                        self.reg.set_reg_a(self.get_mem(self.reg.get_reg_bc()))
                    }
                    crate::runtime::instr::Location16Bit::DE => {
                        self.reg.set_reg_a(self.get_mem(self.reg.get_reg_de()))
                    }
                    crate::runtime::instr::Location16Bit::HL => {
                        self.reg.set_reg_a(self.get_mem(self.reg.get_reg_hl()));
                        self.reg.set_reg_hl(
                            ((self.reg.get_reg_hl() as i32)
                                + if increment_flag {
                                    1
                                } else if decrement_flag {
                                    -1
                                } else {
                                    0
                                }) as u16,
                        )
                    }
                    _ => unreachable!(),
                }
            }
            MemoryInstruction::LoadIndirectFromA(location16, increment_flag, decrement_flag) => {
                match location16 {
                    crate::runtime::instr::Location16Bit::BC => {
                        self.set_mem(self.reg.get_reg_bc(), self.reg.get_reg_a())
                    }
                    crate::runtime::instr::Location16Bit::DE => {
                        self.set_mem(self.reg.get_reg_de(), self.reg.get_reg_a())
                    }
                    crate::runtime::instr::Location16Bit::HL => {
                        self.set_mem(self.reg.get_reg_hl(), self.reg.get_reg_a());
                        self.reg.set_reg_hl(
                            ((self.reg.get_reg_hl() as i32)
                                + if increment_flag {
                                    1
                                } else if decrement_flag {
                                    -1
                                } else {
                                    0
                                }) as u16,
                        )
                    }
                    _ => unreachable!(),
                }
            }
            MemoryInstruction::Load(dest, source) => {
                use crate::runtime::instr::Location8Bit::*;
                match (dest, source) {
                    (A, A) => self.reg.set_reg_a(self.reg.get_reg_a()),
                    (A, B) => self.reg.set_reg_a(self.reg.get_reg_b()),
                    (A, C) => self.reg.set_reg_a(self.reg.get_reg_c()),
                    (A, D) => self.reg.set_reg_a(self.reg.get_reg_d()),
                    (A, E) => self.reg.set_reg_a(self.reg.get_reg_e()),
                    (A, H) => self.reg.set_reg_a(self.reg.get_reg_h()),
                    (A, L) => self.reg.set_reg_a(self.reg.get_reg_l()),
                    (A, Indirect) => self.reg.set_reg_a(self.get_mem(self.reg.get_reg_hl())),
                    // B
                    (B, A) => self.reg.set_reg_b(self.reg.get_reg_a()),
                    (B, B) => self.reg.set_reg_b(self.reg.get_reg_b()),
                    (B, C) => self.reg.set_reg_b(self.reg.get_reg_c()),
                    (B, D) => self.reg.set_reg_b(self.reg.get_reg_d()),
                    (B, E) => self.reg.set_reg_b(self.reg.get_reg_e()),
                    (B, H) => self.reg.set_reg_b(self.reg.get_reg_h()),
                    (B, L) => self.reg.set_reg_b(self.reg.get_reg_l()),
                    (B, Indirect) => self.reg.set_reg_b(self.get_mem(self.reg.get_reg_hl())),
                    // C
                    (C, A) => self.reg.set_reg_c(self.reg.get_reg_a()),
                    (C, B) => self.reg.set_reg_c(self.reg.get_reg_b()),
                    (C, C) => self.reg.set_reg_c(self.reg.get_reg_c()),
                    (C, D) => self.reg.set_reg_c(self.reg.get_reg_d()),
                    (C, E) => self.reg.set_reg_c(self.reg.get_reg_e()),
                    (C, H) => self.reg.set_reg_c(self.reg.get_reg_h()),
                    (C, L) => self.reg.set_reg_c(self.reg.get_reg_l()),
                    (C, Indirect) => self.reg.set_reg_c(self.get_mem(self.reg.get_reg_hl())),
                    // D
                    (D, A) => self.reg.set_reg_d(self.reg.get_reg_a()),
                    (D, B) => self.reg.set_reg_d(self.reg.get_reg_b()),
                    (D, C) => self.reg.set_reg_d(self.reg.get_reg_c()),
                    (D, D) => self.reg.set_reg_d(self.reg.get_reg_d()),
                    (D, E) => self.reg.set_reg_d(self.reg.get_reg_e()),
                    (D, H) => self.reg.set_reg_d(self.reg.get_reg_h()),
                    (D, L) => self.reg.set_reg_d(self.reg.get_reg_l()),
                    (D, Indirect) => self.reg.set_reg_d(self.get_mem(self.reg.get_reg_hl())),
                    // E
                    (E, A) => self.reg.set_reg_e(self.reg.get_reg_a()),
                    (E, B) => self.reg.set_reg_e(self.reg.get_reg_b()),
                    (E, C) => self.reg.set_reg_e(self.reg.get_reg_c()),
                    (E, D) => self.reg.set_reg_e(self.reg.get_reg_d()),
                    (E, E) => self.reg.set_reg_e(self.reg.get_reg_e()),
                    (E, H) => self.reg.set_reg_e(self.reg.get_reg_h()),
                    (E, L) => self.reg.set_reg_e(self.reg.get_reg_l()),
                    (E, Indirect) => self.reg.set_reg_e(self.get_mem(self.reg.get_reg_hl())),
                    // H
                    (H, A) => self.reg.set_reg_h(self.reg.get_reg_a()),
                    (H, B) => self.reg.set_reg_h(self.reg.get_reg_b()),
                    (H, C) => self.reg.set_reg_h(self.reg.get_reg_c()),
                    (H, D) => self.reg.set_reg_h(self.reg.get_reg_d()),
                    (H, E) => self.reg.set_reg_h(self.reg.get_reg_e()),
                    (H, H) => self.reg.set_reg_h(self.reg.get_reg_h()),
                    (H, L) => self.reg.set_reg_h(self.reg.get_reg_l()),
                    (H, Indirect) => self.reg.set_reg_h(self.get_mem(self.reg.get_reg_hl())),
                    // L
                    (L, A) => self.reg.set_reg_l(self.reg.get_reg_a()),
                    (L, B) => self.reg.set_reg_l(self.reg.get_reg_b()),
                    (L, C) => self.reg.set_reg_l(self.reg.get_reg_c()),
                    (L, D) => self.reg.set_reg_l(self.reg.get_reg_d()),
                    (L, E) => self.reg.set_reg_l(self.reg.get_reg_e()),
                    (L, H) => self.reg.set_reg_l(self.reg.get_reg_h()),
                    (L, L) => self.reg.set_reg_l(self.reg.get_reg_l()),
                    (L, Indirect) => self.reg.set_reg_l(self.get_mem(self.reg.get_reg_hl())),
                    // Indirect
                    (Indirect, A) => self.set_mem(self.reg.get_reg_hl(), self.reg.get_reg_a()),
                    (Indirect, B) => self.set_mem(self.reg.get_reg_hl(), self.reg.get_reg_b()),
                    (Indirect, C) => self.set_mem(self.reg.get_reg_hl(), self.reg.get_reg_c()),
                    (Indirect, D) => self.set_mem(self.reg.get_reg_hl(), self.reg.get_reg_d()),
                    (Indirect, E) => self.set_mem(self.reg.get_reg_hl(), self.reg.get_reg_e()),
                    (Indirect, H) => self.set_mem(self.reg.get_reg_hl(), self.reg.get_reg_h()),
                    (Indirect, L) => self.set_mem(self.reg.get_reg_hl(), self.reg.get_reg_l()),
                    (Indirect, Indirect) => unreachable!(),
                }
            }
            MemoryInstruction::LoadPortN(n) => self.reg.set_reg_a(self.get_mem(0xff00 + n as u16)),
            MemoryInstruction::StorePortN(n) => {
                self.set_mem(0xff00 + n as u16, self.reg.get_reg_a())
            }
            MemoryInstruction::LoadPortC => self
                .reg
                .set_reg_a(self.get_mem(0xff00 + self.reg.get_reg_c() as u16)),
            MemoryInstruction::StorePortC => {
                self.set_mem(0xff00 + self.reg.get_reg_c() as u16, self.reg.get_reg_a())
            }
            MemoryInstruction::Push(location16) => {
                self.reg
                    .set_stack_pointer((self.reg.get_stack_pointer() as i32 - 2) as u16);
                let [lower_byte, upper_byte] = match location16 {
                    crate::runtime::instr::Location16Bit::AF => self.reg.get_reg_af().to_le_bytes(),
                    crate::runtime::instr::Location16Bit::BC => self.reg.get_reg_bc().to_le_bytes(),
                    crate::runtime::instr::Location16Bit::DE => self.reg.get_reg_de().to_le_bytes(),
                    crate::runtime::instr::Location16Bit::HL => self.reg.get_reg_hl().to_le_bytes(),
                    crate::runtime::instr::Location16Bit::SP => self.reg.get_stack_pointer().to_le_bytes(),
                };
                self.set_mem(self.reg.get_stack_pointer(), lower_byte);
                self.set_mem(self.reg.get_stack_pointer() + 1, upper_byte);
            }
            MemoryInstruction::Pop(location16) => {
                let [lower_byte, upper_byte] = [
                    self.get_mem(self.reg.get_stack_pointer()),
                    self.get_mem(self.reg.get_stack_pointer() + 1),
                ];
                let combined = u16::from_le_bytes([lower_byte, upper_byte]);
                match location16 {
                    crate::runtime::instr::Location16Bit::AF => self.reg.set_reg_af(combined),
                    crate::runtime::instr::Location16Bit::BC => self.reg.set_reg_bc(combined),
                    crate::runtime::instr::Location16Bit::DE => self.reg.set_reg_de(combined),
                    crate::runtime::instr::Location16Bit::HL => self.reg.set_reg_hl(combined),
                    crate::runtime::instr::Location16Bit::SP => unreachable!(),
                };
                self.reg.set_stack_pointer(self.reg.get_stack_pointer() + 2);
            }
            MemoryInstruction::LoadSPFromHL => self.reg.set_stack_pointer(self.reg.get_reg_hl()),
            MemoryInstruction::LoadSPIntoConstantAddress(nn) => {
                let [lower_byte, upper_byte] = self.reg.get_stack_pointer().to_le_bytes();
                self.set_mem(nn, lower_byte);
                self.set_mem(nn + 1, upper_byte);
            }
            MemoryInstruction::LoadHLFromSPOffset(offset) => {
                let before_val = self.reg.get_stack_pointer();
                let val = before_val.wrapping_add(offset as u16);
                self.reg.set_reg_hl(val);

                // Should be correct but isn't?!
                self.reg.set_status_carry(
                    (before_val & 0x00FF).wrapping_add(offset as u16 & 0x00FF) & 0x100 > 0,
                );
                self.reg.set_status_half_carry(
                    (((before_val & 0x000F).wrapping_add((offset as u16) & 0x000F)) & 0x10) == 0x10,
                );
                self.reg.set_status_zero(false);
                self.reg.set_status_negative(false);
                // let res = self.reg.get_stack_pointer() as i32 + offset as i32;
                // self.reg.set_reg_hl(res as u16);

                // self.reg.set_status_zero(false);
                // self.reg.set_status_negative(false);
                // self.reg.set_status_half_carry((self.reg.get_stack_pointer() & 0x000f).wrapping_add(offset as u16) & 0x10 > 0);
                // self.reg.set_status_carry((self.reg.get_stack_pointer() & 0x00Ff).wrapping_add(offset as u8 as u16) & 0x100 > 0);
            }
            MemoryInstruction::LoadIndirectConstantToA(nn) => {
                // println!("ld a, [{:04x}]", nn);
                // if self.get_mem(nn) == 0x01{
                //     println!("ld a, [{:04x}] == 1", nn);
                // }
                // if self.get_mem(nn) == 0x00{
                //     println!("ld a, [{:04x}] == 0", nn);
                // }
                self.reg.set_reg_a(self.get_mem(nn))
            }
            MemoryInstruction::LoadIndirectConstantFromA(nn) => {
                self.set_mem(nn, self.reg.get_reg_a())
            }
        }
    }
    fn execute_arithmetic_instr(&mut self, cycles: u8, instr: ArithmeticInstruction) {
        match instr {
            ArithmeticInstruction::ALUReg8(op, other) => {
                let a_val = self.reg.get_reg_a() as i32;
                let other_val = self.get_loc8(other) as i32;
                let cy = if self.reg.get_status_carry() { 1_i32 } else { 0 };

                match op {
                    crate::runtime::instr::ALUOpTypes::ADD => {
                        let res = a_val + other_val;
                        self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res > 0xFF);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf) + (other_val as u8 & 0xf)) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::ADC => {
                        let res = a_val + other_val + cy;
                        self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res > 0xFF);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf) + (other_val as u8 & 0xf) + cy as u8) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::SUB => {
                        let res = a_val - other_val;
                        self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res < 0);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf).wrapping_sub(other_val as u8 & 0xf)) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(true);
                    }
                    crate::runtime::instr::ALUOpTypes::SBC => {
                        let res = a_val - other_val - cy;
                        self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res < 0);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf).wrapping_sub(other_val as u8 & 0xf).wrapping_sub(cy as u8)) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(true);
                    }
                    crate::runtime::instr::ALUOpTypes::AND => {
                        let res = a_val as u8 & other_val as u8;
                        self.reg.set_reg_a(res);

                        self.reg.set_status_carry(false);
                        self.reg.set_status_half_carry(true);
                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::XOR => {
                        let res = a_val as u8 ^ other_val as u8;
                        self.reg.set_reg_a(res);

                        self.reg.set_status_carry(false);
                        self.reg.set_status_half_carry(false);
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::OR => {
                        let res = a_val as u8 | other_val as u8;
                        self.reg.set_reg_a(res);

                        self.reg.set_status_carry(false);
                        self.reg.set_status_half_carry(false);
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::CP => {
                        // TODO:
                        let res = a_val - other_val;
                        // println!("HL: {:x}", self.reg.get_reg_hl());
                        // println!("[HL]: {:x}",self.get_mem(self.reg.get_reg_hl()));
                        // println!("[0x104]: {:x}",self.mem[0x104]);
                        // println!("Boot[104]: {:x}", BOOT_ROM_GB[0x104]);
                        // println!("{:x} - {:x} = {:x}", a_val, other_val, res);
                        // self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res < 0);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf).wrapping_sub(other_val as u8 & 0xf)) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(true);
                    }
                };
            }
            ArithmeticInstruction::ALUImediate(op, other_constant) => {
                let a_val = self.reg.get_reg_a() as i32;
                let other_val = other_constant as i32;
                let cy = if self.reg.get_status_carry() { 1 } else { 0 } as i32;

                match op {
                    crate::runtime::instr::ALUOpTypes::ADD => {
                        let res = a_val + other_val;
                        self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res > 0xFF);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf) + (other_val as u8 & 0xf)) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::ADC => {
                        let res = a_val + other_val + cy;
                        self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res > 0xFF);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf) + (other_val as u8 & 0xf) + cy as u8) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::SUB => {
                        let a_val = self.reg.get_reg_a();
                        let other_val = other_constant;
                        let res = a_val.wrapping_sub(other_val);
                        self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res > a_val);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf).wrapping_sub(other_val as u8 & 0xf)) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(true);
                    }
                    crate::runtime::instr::ALUOpTypes::SBC => {
                        let res = a_val - other_val - cy;
                        self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res < 0);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf)
                                .wrapping_sub(other_val as u8 & 0xf)
                                .wrapping_sub(cy as u8))
                                & 0x10
                                > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(true);
                    }
                    crate::runtime::instr::ALUOpTypes::AND => {
                        let res = a_val as u8 & other_val as u8;
                        self.reg.set_reg_a(res);

                        self.reg.set_status_carry(false);
                        self.reg.set_status_half_carry(true);
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::XOR => {
                        let res = a_val as u8 ^ other_val as u8;
                        self.reg.set_reg_a(res);

                        self.reg.set_status_carry(false);
                        self.reg.set_status_half_carry(false);
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::OR => {
                        let res = a_val as u8 | other_val as u8;
                        self.reg.set_reg_a(res);

                        self.reg.set_status_carry(false);
                        self.reg.set_status_half_carry(false);
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(false);
                    }
                    crate::runtime::instr::ALUOpTypes::CP => {
                        // TODO:
                        let res = a_val - other_val;
                        // self.reg.set_reg_a(res as u8);

                        self.reg.set_status_carry(res < 0);
                        self.reg.set_status_half_carry(
                            ((a_val as u8 & 0xf).wrapping_sub(other_val as u8 & 0xf)) & 0x10 > 0,
                        );
                        self.reg.set_status_zero(res as u8 == 0);
                        self.reg.set_status_negative(true);
                    }
                };
            }
            ArithmeticInstruction::INC8(loc8) => {
                let before_val = self.get_loc8(loc8);
                let val = before_val.wrapping_add(1);
                self.set_loc8(loc8, val);

                self.reg
                    .set_status_half_carry(((before_val as u8 & 0xf) + (1_u8 & 0xf)) & 0x10 > 0);
                self.reg.set_status_zero(val == 0);
                self.reg.set_status_negative(false);
            }
            ArithmeticInstruction::DEC8(loc8) => {
                let before_val = self.get_loc8(loc8);
                let val = before_val.wrapping_sub(1);
                self.set_loc8(loc8, val);

                // TODO?
                // if self.reg.get_pc() > 0x200{
                //     println!("{:X} - DEC8 {:08b} -- {:?}",self.reg.get_pc(), before_val,(before_val as u8 & 0xf).wrapping_sub(1_u8) & 0x10 > 0);
                // }
                self.reg
                    .set_status_half_carry((before_val as u8 & 0xf).wrapping_sub(1_u8) & 0x10 > 0);
                self.reg.set_status_zero(val == 0);
                self.reg.set_status_negative(true);
            }
            ArithmeticInstruction::ADDHL16(loc16) => {
                let before_val = self.reg.get_reg_hl();
                let before_other_val = self.get_loc16(loc16);
                let val = before_val.wrapping_add(before_other_val);
                self.reg.set_reg_hl(val);

                self.reg.set_status_negative(false);
                self.reg.set_status_carry(val < before_val);
                // TODO
                self.reg.set_status_half_carry(
                    (before_val & 0x0FFF).wrapping_add(before_other_val & 0x0FFF) & 0x1000 > 0,
                );
            }
            ArithmeticInstruction::INC16(loc16) => {
                let before_val = self.get_loc16(loc16);
                let val = before_val.wrapping_add(1);
                self.set_loc16(loc16, val);
            }
            ArithmeticInstruction::DEC16(loc16) => {
                let before_val = self.get_loc16(loc16);
                let val = before_val.wrapping_sub(1);
                self.set_loc16(loc16, val);
            }
            ArithmeticInstruction::ADDSPRelative(d) => {
                let before_val = self.reg.get_stack_pointer();
                let val = before_val as i32 + d as i32;
                self.reg.set_stack_pointer(val as u16);

                // Should be correct
                self.reg.set_status_carry(
                    (before_val & 0x00FF).wrapping_add(d as u16 & 0x00FF) & 0x100 > 0,
                );
                self.reg.set_status_half_carry(
                    (before_val & 0x000F).wrapping_add(d as u16 & 0x000F) & 0x10 > 0,
                );
                self.reg.set_status_zero(false);
                self.reg.set_status_negative(false);
            }
            ArithmeticInstruction::DAA => {
                // Fix BCD after sub or add
                let value = self.reg.get_reg_a();
                let mut correction = 0;

                if self.reg.get_status_half_carry()
                    || (!self.reg.get_status_negative() && ((value & 0x0f) > 9))
                {
                    correction |= 0x06;
                }

                if self.reg.get_status_carry()
                    || (!self.reg.get_status_negative() && (value > 0x99))
                {
                    correction |= 0x60;
                    self.reg.set_status_carry(true);
                }

                let res = if self.reg.get_status_negative() {
                    value as i32 - correction as i32
                } else {
                    value as i32 + correction as i32
                };
                let res_value = res as u8;

                self.reg.set_status_half_carry(false);
                self.reg.set_status_zero(res_value == 0);

                self.reg.set_reg_a(res_value);
            }
            ArithmeticInstruction::CPL => {
                self.reg.set_reg_a(self.reg.get_reg_a() ^ 0xFF);

                self.reg.set_status_negative(true);
                self.reg.set_status_half_carry(true);
            }
        }
    }
    fn execute_shift_rotate_instr(&mut self, cycles: u8, instr: ShiftRotateInstruction) {
        match instr {
            ShiftRotateInstruction::ShiftRotate(rotate_type, loc8) => {
                match rotate_type {
                    crate::runtime::instr::ShiftRotateType::RLC => {
                        let loc_val = self.get_loc8(loc8);
                        let res = loc_val << 1 | loc_val >> 7;
                        self.set_loc8(loc8, res);

                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_carry(loc_val >= 0b1000_0000);
                    }
                    crate::runtime::instr::ShiftRotateType::RRC => {
                        let loc_val = self.get_loc8(loc8);
                        let res = loc_val >> 1 | loc_val << 7;
                        self.set_loc8(loc8, res);

                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_carry(loc_val & 0x01 == 1);
                    }
                    crate::runtime::instr::ShiftRotateType::RL => {
                        let loc_val = self.get_loc8(loc8);
                        let res =
                            (loc_val << 1) | (if self.reg.get_status_carry() { 1 } else { 0 });
                        self.set_loc8(loc8, res);

                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_carry(loc_val >= 0b1000_0000);
                    }
                    crate::runtime::instr::ShiftRotateType::RR => {
                        let loc_val = self.get_loc8(loc8);
                        let res =
                            (loc_val >> 1) | (if self.reg.get_status_carry() { 0x80 } else { 0 });
                        self.set_loc8(loc8, res);

                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_carry(loc_val & 0x01 == 1);
                    }
                    crate::runtime::instr::ShiftRotateType::SLA => {
                        let loc_val = self.get_loc8(loc8);
                        let res = loc_val << 1;
                        self.set_loc8(loc8, res);

                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_carry(loc_val >= 0b1000_0000);
                    }
                    crate::runtime::instr::ShiftRotateType::SRA => {
                        let loc_val = self.get_loc8(loc8);
                        let res = (loc_val >> 1) | (loc_val & 0x80);
                        self.set_loc8(loc8, res);

                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_carry(loc_val & 0x01 == 1);
                    }
                    crate::runtime::instr::ShiftRotateType::SWAP => {
                        let loc_val = self.get_loc8(loc8);
                        let res = (loc_val << 4) | (loc_val >> 4);
                        self.set_loc8(loc8, res);

                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_carry(false);
                    }
                    crate::runtime::instr::ShiftRotateType::SRL => {
                        let loc_val = self.get_loc8(loc8);
                        let res = loc_val >> 1;
                        self.set_loc8(loc8, res);

                        self.reg.set_status_zero(res == 0);
                        self.reg.set_status_carry(loc_val & 0x01 == 1);
                    }
                }

                self.reg.set_status_negative(false);
                self.reg.set_status_half_carry(false);
            }
            ShiftRotateInstruction::RLCA => {
                let loc_val = self.reg.get_reg_a();
                let res = loc_val << 1 | loc_val >> 7;
                self.reg.set_reg_a(res);

                self.reg.set_status_zero(false);
                self.reg.set_status_carry(loc_val >= 0b1000_0000);
                self.reg.set_status_negative(false);
                self.reg.set_status_half_carry(false);
            }
            ShiftRotateInstruction::RRCA => {
                let loc_val = self.reg.get_reg_a();
                let res = loc_val >> 1 | loc_val << 7;
                self.reg.set_reg_a(res);

                self.reg.set_status_zero(false);
                self.reg.set_status_carry(loc_val & 0x01 == 1);
                self.reg.set_status_negative(false);
                self.reg.set_status_half_carry(false);
            }
            ShiftRotateInstruction::RLA => {
                let loc_val = self.reg.get_reg_a();
                let res = (loc_val << 1) + (if self.reg.get_status_carry() { 1 } else { 0 });
                self.reg.set_reg_a(res);

                self.reg.set_status_zero(false);
                self.reg.set_status_carry(loc_val >= 0b1000_0000);
                self.reg.set_status_negative(false);
                self.reg.set_status_half_carry(false);
            }
            ShiftRotateInstruction::RRA => {
                let loc_val = self.reg.get_reg_a();
                let res = (loc_val >> 1) + (if self.reg.get_status_carry() { 0x80 } else { 0 });
                self.reg.set_reg_a(res);

                self.reg.set_status_zero(false);
                self.reg.set_status_carry(loc_val & 0x01 == 1);
                self.reg.set_status_negative(false);
                self.reg.set_status_half_carry(false);
            }
        }
    }
    fn execute_single_bit_instr(&mut self, cycles: u8, instr: SingleBitInstruction) {
        match instr {
            SingleBitInstruction::TestBit(n_bit, loc8) => {
                let test = self.get_loc8(loc8) & (0x01 << n_bit);
                // println!("test: {:?} | {:x} & {:b}", test, self.get_loc8(loc8), (0x01 << n_bit));

                self.reg.set_status_zero(test == 0);
                self.reg.set_status_negative(false);
                self.reg.set_status_half_carry(true);
                // println!("{:?}",self.reg.get_status_zero());
            }
            SingleBitInstruction::ResetBit(n_bit, loc8) => {
                let res = self.get_loc8(loc8) & (0xFF ^ (0x01 << n_bit));
                self.set_loc8(loc8, res);
            }
            SingleBitInstruction::SetBit(n_bit, loc8) => {
                let res = self.get_loc8(loc8) | (0x01 << n_bit);
                self.set_loc8(loc8, res);
            }
        }
    }
    fn execute_control_instr(&mut self, cycles: u8, instr: ControlInstruction) {
        match instr {
            ControlInstruction::CCF => {
                self.reg.set_status_half_carry(false);
                self.reg.set_status_negative(false);
                self.reg.set_status_carry(!self.reg.get_status_carry());
            }
            ControlInstruction::SCF => {
                self.reg.set_status_half_carry(false);
                self.reg.set_status_negative(false);
                self.reg.set_status_carry(true);
            }
            ControlInstruction::NOP => {}
            ControlInstruction::HALT => self.halted_flag = true,
            ControlInstruction::STOP => {
                self.stopped_flag = true;
            },
            ControlInstruction::DI => self.disable_all_interrupts(),
            ControlInstruction::EI => self.enable_all_interrupts(),
        }
    }
    fn execute_jump_instr(&mut self, cycles: u8, instr: JumpInstruction) {
        match instr {
            JumpInstruction::JumpConstant(nn) => self.reg.set_pc(nn),
            JumpInstruction::JumpHL => self.reg.set_pc(self.reg.get_reg_hl()),
            JumpInstruction::JumpConstantConditional(condition, nn) => {
                let jump_flag = match condition {
                    crate::runtime::instr::JumpCondition::NZ => !self.reg.get_status_zero(),
                    crate::runtime::instr::JumpCondition::Z => self.reg.get_status_zero(),
                    crate::runtime::instr::JumpCondition::NC => !self.reg.get_status_carry(),
                    crate::runtime::instr::JumpCondition::C => self.reg.get_status_carry(),
                };
                if jump_flag {
                    self.reg.set_pc(nn);
                    self.cycle += 16;
                } else {
                    self.cycle += 12;
                }
            }
            JumpInstruction::JumpRelative(d) => self
                .reg
                .set_pc((self.reg.get_pc() as i32 + d as i32) as u16),
            JumpInstruction::JumpRelativeConditional(cond, d) => {
                let jump_flag = match cond {
                    crate::runtime::instr::JumpCondition::NZ => !self.reg.get_status_zero(),
                    crate::runtime::instr::JumpCondition::Z => self.reg.get_status_zero(),
                    crate::runtime::instr::JumpCondition::NC => !self.reg.get_status_carry(),
                    crate::runtime::instr::JumpCondition::C => self.reg.get_status_carry(),
                };
                if jump_flag {
                    self.reg
                        .set_pc((self.reg.get_pc() as i32 + d as i32) as u16);
                    self.cycle += 12;
                } else {
                    self.cycle += 8;
                }
            }
            JumpInstruction::CallConstant(nn) => {

                self.reg
                    .set_stack_pointer((self.reg.get_stack_pointer() as i32 - 2) as u16);
                let [lower_byte, upper_byte] = self.reg.get_pc().to_le_bytes();
                self.set_mem(self.reg.get_stack_pointer(), lower_byte);
                self.set_mem(self.reg.get_stack_pointer() + 1, upper_byte);

                // println!("CALL {:X} -- BACK ADDR: {:x} {:x}", nn, self.get_mem(self.reg.get_stack_pointer()), self.get_mem(self.reg.get_stack_pointer() + 1));

                self.reg.set_pc(nn)
            }
            JumpInstruction::CallConstantConditional(cond, nn) => {
                let jump_flag = match cond {
                    crate::runtime::instr::JumpCondition::NZ => !self.reg.get_status_zero(),
                    crate::runtime::instr::JumpCondition::Z => self.reg.get_status_zero(),
                    crate::runtime::instr::JumpCondition::NC => !self.reg.get_status_carry(),
                    crate::runtime::instr::JumpCondition::C => self.reg.get_status_carry(),
                };
                if jump_flag {
                    self.reg
                        .set_stack_pointer((self.reg.get_stack_pointer() as i32 - 2) as u16);
                    let [lower_byte, upper_byte] = self.reg.get_pc().to_le_bytes();
                    self.set_mem(self.reg.get_stack_pointer(), lower_byte);
                    self.set_mem(self.reg.get_stack_pointer() + 1, upper_byte);

                    self.reg.set_pc(nn);
                    self.cycle += 24;
                } else {
                    self.cycle += 12;
                }
            }
            JumpInstruction::Return => {
                let [lower, higher] = [
                    self.get_mem(self.reg.get_stack_pointer()),
                    self.get_mem(self.reg.get_stack_pointer() + 1),
                ];
                let addr = u16::from_le_bytes([lower, higher]);
                // println!("RETURN {:x} -- lower: {:x} higher: {:x}", addr, lower, higher);

                self.reg.set_pc(addr);
                self.reg.set_stack_pointer(self.reg.get_stack_pointer() + 2);
            }
            JumpInstruction::ReturnConditional(cond) => {
                let jump_flag = match cond {
                    crate::runtime::instr::JumpCondition::NZ => !self.reg.get_status_zero(),
                    crate::runtime::instr::JumpCondition::Z => self.reg.get_status_zero(),
                    crate::runtime::instr::JumpCondition::NC => !self.reg.get_status_carry(),
                    crate::runtime::instr::JumpCondition::C => self.reg.get_status_carry(),
                };
                if jump_flag {
                    let [lower, higher] = [
                        self.get_mem(self.reg.get_stack_pointer()),
                        self.get_mem(self.reg.get_stack_pointer() + 1),
                    ];
                    self.reg.set_pc(u16::from_le_bytes([lower, higher]));
                    self.reg.set_stack_pointer(self.reg.get_stack_pointer() + 2);

                    self.cycle += 20;
                } else {
                    self.cycle += 8;
                }
            }
            JumpInstruction::ReturnEnableInterrupts => {
                self.enable_all_interrupts();
                let [lower, higher] = [
                    self.get_mem(self.reg.get_stack_pointer()),
                    self.get_mem(self.reg.get_stack_pointer() + 1),
                ];
                let addr = u16::from_le_bytes([lower, higher]);
                // println!("RETURN {:x} -- lower: {:x} higher: {:x}", addr, lower, higher);

                self.reg.set_pc(addr);
                self.reg.set_stack_pointer(self.reg.get_stack_pointer() + 2);
            }
            JumpInstruction::Reset(n) => {
                self.reg
                    .set_stack_pointer((self.reg.get_stack_pointer() as i32 - 2) as u16);
                let [lower_byte, upper_byte] = self.reg.get_pc().to_le_bytes();
                self.set_mem(self.reg.get_stack_pointer(), lower_byte);
                self.set_mem(self.reg.get_stack_pointer() + 1, upper_byte);

                self.reg.set_pc(n as u16);
            }
        }
    }
}
