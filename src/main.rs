use std::fs::File;
use std::io::Write;

use crate::cpu::*;
use crate::instr::*;

pub mod cpu;
pub mod instr;

fn main() {
    let test_rom_paths = vec![
        "./blargg/cpu_instrs/individual/01-special.gb",
        "./blargg/cpu_instrs/individual/02-interrupts.gb",
        "./blargg/cpu_instrs/individual/03-op sp,hl.gb",
    ];

    let mut cpu: CPU = CPU::new();
    cpu.load_blargg_test_rom("./blargg/cpu_instrs/individual/01-special.gb");
    cpu.load_boot_rom();
    cpu.run_till_0x100();

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
    let mut last_mem = 0;
    for _ in 0..128000000 {
        let next_instr: Instruction = cpu.next_instr();
        cpu.execute_instr(next_instr);
        writeln!(output, "{}", cpu.print_status()).unwrap();
        if cpu.get_mem(0xFF01) != last_mem{
            println!("'{}' -{:02x} {:08b}",cpu.get_mem(0xFF01) as char, cpu.get_mem(0xFF01),cpu.get_mem(0xFF02));
            last_mem = cpu.get_mem(0xFF01);
        }
    }
}
