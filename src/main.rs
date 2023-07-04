use std::env;

use runtime::run_test_roms;

pub mod runtime;
pub mod ui;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args[1] == "ui"{
        ui::run_ui(&args[2]).unwrap();
    } else if args[1] == "run" {
        run_test_roms::run_rom(&args[2]);
    } else {
        run_test_roms::run();
    }
}
