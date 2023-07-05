use std::{env, fs::File, io::Read};

use runtime::{instr::Instruction, run_test_roms};

pub mod runtime;
pub mod ui;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args[1] == "ui" {
        ui::run_ui(&args[2]).unwrap();
    } else if args[1] == "run" {
        run_test_roms::run_rom(&args[2]);
    } else if args[1] == "hex" {
        let mut f = File::open(&args[2]).unwrap();
        let mut buf: Vec<u8> = vec![];
        let _num_bytes = f.read_to_end(&mut buf).unwrap();
        // eprintln!("{:?}", num_bytes);
        for (idx, byte) in buf.iter().enumerate().skip(256) {
            println!("Byte {:04x} = {:02x}", idx as u16, *byte);
        }
    } else if args[1] == "test" {
        let bytes: [u8; 1] = [0xAF];
        let instr = Instruction::parse_from_bytes(&bytes);
        println!("{:?}", instr);
    } else {
        run_test_roms::run();
    }
}
