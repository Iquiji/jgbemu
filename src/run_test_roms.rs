use std::fs::File;
use std::io::Write;

use crate::cpu::*;
use crate::instr::*;

pub fn run() {
    let rom_num = 2;
    let test_rom_paths = vec![
        "./blargg/cpu_instrs/individual/01-special.gb",
        "./blargg/cpu_instrs/individual/02-interrupts.gb",
        "./blargg/cpu_instrs/individual/03-op sp,hl.gb",
        "./blargg/cpu_instrs/individual/04-op r,imm.gb",
        "./blargg/cpu_instrs/individual/05-op rp.gb",
        "./blargg/cpu_instrs/individual/06-ld r,r.gb",
        "./blargg/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb",
        "./blargg/cpu_instrs/individual/08-misc instrs.gb",
        "./blargg/cpu_instrs/individual/09-op r,r.gb",
        "./blargg/cpu_instrs/individual/10-bit ops.gb",
        "./blargg/cpu_instrs/individual/11-op a,(hl).gb",
    ];

    let mut cpu: CPU = CPU::new();
    cpu.load_blargg_test_rom(test_rom_paths[rom_num - 1]);
    cpu.load_boot_rom();
    cpu.run_till_0x100();
    cpu.unload_boot_rom(test_rom_paths[rom_num - 1]);

    println!("{}", cpu.print_status());
    println!(
        "Z?: {:?} N: {:?} H: {:?} C: {:?}",
        cpu.reg.get_status_zero(),
        cpu.reg.get_status_negative(),
        cpu.reg.get_status_half_carry(),
        cpu.reg.get_status_carry()
    );

    let mut output = File::create(format!("blargg_{}.log", rom_num)).unwrap();
    writeln!(output, "{}", cpu.print_status()).unwrap();
    let mut current_word = String::new();
    let mut last_mem = 0;
    for _i in 0.. {
        cpu.do_enable_interrupts_on_req();
        cpu.handle_interrupts();
        cpu.handle_timer();
        let next_instr: Instruction = cpu.next_instr();
        cpu.execute_instr(next_instr.clone());
        writeln!(output, "{}", cpu.print_status()).unwrap();

        if cpu.get_mem(0xFF01) != last_mem {
            println!(
                "'{}' -{:02x} {:08b}",
                cpu.get_mem(0xFF01) as char,
                cpu.get_mem(0xFF01),
                cpu.get_mem(0xFF02)
            );
            last_mem = cpu.get_mem(0xFF01);
            current_word.push(cpu.get_mem(0xFF01) as char);
        }
    }
}

pub fn run_rom(path: &str) {
    let mut cpu: CPU = CPU::new();
    cpu.load_blargg_test_rom(path);
    cpu.load_boot_rom();
    cpu.run_till_0x100();
    cpu.unload_boot_rom(path);

    // println!("{}", cpu.print_status());
    // println!(
    //     "Z?: {:?} N: {:?} H: {:?} C: {:?}",
    //     cpu.reg.get_status_zero(),
    //     cpu.reg.get_status_negative(),
    //     cpu.reg.get_status_half_carry(),
    //     cpu.reg.get_status_carry()
    // );

    // let mut output = File::create(format!("blargg_{}.log", rom_num)).unwrap();
    // writeln!(output, "{}", cpu.print_status()).unwrap();
    let mut current_word = String::new();
    let mut last_mem = 0;
    for _i in 0.. {
        cpu.do_enable_interrupts_on_req();
        cpu.handle_interrupts();
        cpu.handle_timer();
        let next_instr: Instruction = cpu.next_instr();
        cpu.execute_instr(next_instr.clone());
        // writeln!(output, "{}", cpu.print_status()).unwrap();

        if cpu.get_mem(0xFF01) != last_mem {
            // println!("'{}' -{:02x} {:08b}",cpu.get_mem(0xFF01) as char, cpu.get_mem(0xFF01),cpu.get_mem(0xFF02));
            last_mem = cpu.get_mem(0xFF01);
            current_word.push(cpu.get_mem(0xFF01) as char);
            print!("{}", cpu.get_mem(0xFF01) as char);
        }
    }
}
