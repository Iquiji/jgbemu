use std::sync::{Arc, Mutex};

use crate::ui::UserInput;

use self::{cpu::CPU, instr::Instruction};

pub mod cpu;
pub mod graphics;
pub mod instr;
pub mod run_test_roms;

#[derive(Debug)]
pub struct GameBoyEmulatorRuntime{
    pub cpu: CPU,
    pub screen_buffer: Arc<Mutex<Box<[[u8; 160]; 144]>>>,
    pub user_input: Arc<Mutex<UserInput>>,
}

impl GameBoyEmulatorRuntime{
    pub fn new() -> Self{
        let screen_buffer = Arc::new(Mutex::new(Box::new([[0u8; 160]; 144])));
        GameBoyEmulatorRuntime { cpu: CPU::new(screen_buffer.clone()), screen_buffer, user_input: Arc::new(Mutex::new(UserInput::default()))}
    }

    pub fn init(&mut self, path: &str) {
        self.cpu.load_blargg_test_rom(path);
        self.cpu.load_boot_rom();
        self.cpu.run_till_0x100();
        self.cpu.unload_boot_rom(path);
    }

    pub fn run_tick(&mut self) {
        if !self.cpu.stopped_flag{
            self.cpu.handle_user_input(self.user_input.lock().unwrap().clone());
            self.cpu.do_enable_interrupts_on_req();
            self.cpu.handle_interrupts();
            self.cpu.handle_timer();
            let next_instr: Instruction = self.cpu.next_instr();
            self.cpu.execute_instr(next_instr.clone());
            if self.cpu.graphics_controller.tick(self.cpu.cycle) {
                self.cpu.set_mem(0xFF0F, self.cpu.get_mem(0xFF0F) | 0b0000_0010);
            };

            // println!("{:?}", next_instr.itype);
            // println!(" + {}", self.cpu.print_status());
        } else {
            println!("STOPPED!");
        }
    }
}

