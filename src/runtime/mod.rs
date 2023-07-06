use std::sync::{Arc, Mutex};

use crate::ui::UserInput;

use self::{cpu::CPU, instr::Instruction};

pub mod banking;
pub mod cpu;
pub mod graphics;
pub mod instr;
pub mod run_test_roms;

#[derive(Debug)]
pub struct GameBoyEmulatorRuntime {
    pub cpu: CPU,
    pub screen_buffer: Arc<Mutex<Box<[[u8; 160]; 144]>>>,
    pub user_input: Arc<Mutex<UserInput>>,
}

impl GameBoyEmulatorRuntime {
    pub fn new() -> Self {
        let screen_buffer = Arc::new(Mutex::new(Box::new([[0u8; 160]; 144])));
        GameBoyEmulatorRuntime {
            cpu: CPU::new(screen_buffer.clone()),
            screen_buffer,
            user_input: Arc::new(Mutex::new(UserInput::default())),
        }
    }

    pub fn init(&mut self, path: &str) {
        self.cpu.load_rom_from_disk(path);
    }

    pub fn run_tick(&mut self) {
        if !self.cpu.stopped_flag {
            self.cpu
                .handle_user_input(self.user_input.lock().unwrap().clone());
            self.cpu.handle_interrupts();
            self.cpu.handle_timer();
            let next_instr: Instruction = self.cpu.next_instr();
            self.cpu.execute_instr(next_instr.clone());

            let (vblank_irq, stat_irq) = self.cpu.graphics_controller.tick(self.cpu.cycle);
            if stat_irq {
                self.cpu
                    .set_mem(0xFF0F, self.cpu.get_mem(0xFF0F) | 0b0000_0010);
            };
            if vblank_irq {
                self.cpu
                    .set_mem(0xFF0F, self.cpu.get_mem(0xFF0F) | 0b0000_0001);
            }

            println!("{:?}", next_instr.itype);
            println!(" + {}", self.cpu.print_status());
        } else {
            println!("STOPPED!");
        }
    }
}
