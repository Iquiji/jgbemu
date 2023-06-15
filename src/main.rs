use crate::cpu::*;
use crate::instr::*;

pub mod cpu;
pub mod instr;

fn main() {
    let mut cpu: CPU = CPU::default();

    println!("Hello, world!");
    println!("First Instruction: {:?}", Instruction::parse_from_bytes(&BOOT_ROM_GB[0..1]));
    println!("Second Instruction: {:?}", Instruction::parse_from_bytes(&BOOT_ROM_GB[3..4]));
}
