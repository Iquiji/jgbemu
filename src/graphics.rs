#[derive(Debug, Clone)]
pub struct GraphicsController {
    /// LCD Control, Status, Position, Scrolling, and Palettes
    graphics_status: [u8; 12],
    vram: [u8; 0x2000],
    oam: [u8; 160],
}

impl GraphicsController {
    pub fn new() -> GraphicsController {
        GraphicsController {
            graphics_status: [0x00; 12],
            vram: [0x00; 0x2000],
            oam: [0x00; 160],
        }
    }
}

impl GraphicsController {
    pub fn tick(&mut self, cycle: u64) {

    }

    pub fn stat_interrupt(&mut self) -> bool{
        unimplemented!()
    }

    pub fn print_current_frame(&mut self) {
        unimplemented!()
    }
    /// Executed by CPU if:
    /// if (0xFF40..=0xFF4B).contains(&addr)
    ///     || (0x8000..=0x9FFF).contains(&addr)
    ///     || (0xFE00..=0xFE9F).contains(&addr)
    pub fn memory_get(&self, addr: u16) -> u8 {
        unimplemented!()
    }
    pub fn memory_set(&mut self, addr: u16, data: u8) {
        unimplemented!()
    }
}

impl Default for GraphicsController {
    fn default() -> Self {
        GraphicsController::new()
    }
}
