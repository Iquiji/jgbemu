use std::fs::File;
use std::io::Write;

use crate::cpu::*;
use crate::instr::*;

pub mod cpu;
pub mod instr;

fn main() {
    let mut cpu: CPU = CPU::new();
    cpu.load_blargg_test_rom("./blargg/cpu_instrs/individual/01-special.gb");
    cpu.load_boot_rom();

    while cpu.reg.get_pc() != 0x100 {
        let next_instr: Instruction = cpu.next_instr();
        cpu.execute_instr(next_instr);
    }
    println!("{}", cpu.print_status());
    println!(
        "Z?: {:?} N: {:?} H: {:?} C: {:?}",
        cpu.reg.get_status_zero(),
        cpu.reg.get_status_negative(),
        cpu.reg.get_status_half_carry(),
        cpu.reg.get_status_carry()
    );

    let mut output = File::create("blargg_1.log").unwrap();
    writeln!(output, "{}", cpu.print_status()).unwrap();
    for _ in 0..100 {
        let next_instr: Instruction = cpu.next_instr();
        cpu.execute_instr(next_instr);
        writeln!(output, "{}", cpu.print_status()).unwrap();
    }
}
