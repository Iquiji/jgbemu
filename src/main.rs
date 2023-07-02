use std::env;

pub mod cpu;
pub mod graphics;
pub mod instr;
pub mod run_test_roms;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args[1] == "run" {
        run_test_roms::run_rom(&args[2]);
    } else {
        run_test_roms::run();
    }
}
