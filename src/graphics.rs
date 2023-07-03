use image::ImageBuffer;

#[derive(Debug, Clone)]
pub struct GraphicsController {
    /// LCD Control, Status, Position, Scrolling, and Palettes
    /// $FF40 - $FF4B
    /// + FF40 — LCDC: LCD control:
    /// 
    /// 7  LCD and PPU enable  0=Off, 1=On
    /// 6  Window tile map area  0=9800-9BFF, 1=9C00-9FFF
    /// 5  Window enable  0=Off, 1=On
    /// 4  BG and Window tile data area  0=8800-97FF, 1=8000-8FFF
    /// 3  BG tile map area  0=9800-9BFF, 1=9C00-9FFF
    /// 2  OBJ size  0=8x8, 1=8x16
    /// 1  OBJ enable  0=Off, 1=On
    /// 0  BG and Window enable/priority  0=Off, 1=On
    /// + FF44 - LY: LCD Y coordinate [read-only]
    /// LY indicates the current horizontal line, which might be about to be drawn, being drawn, or just been drawn. 
    /// LY can hold any value from 0 to 153, with values from 144 to 153 indicating the VBlank period.
    /// + FF45 — LYC: LY compare
    /// The Game Boy constantly compares the value of the LYC and LY registers. 
    /// When both values are identical, the “LYC=LY” flag in the STAT register is set, and (if enabled) a STAT interrupt is requested.
    /// + FF41 - STAT: LCD status:
    /// Bit 6 - LYC=LY STAT Interrupt source         (1=Enable) (Read/Write)
    /// Bit 5 - Mode 2 OAM STAT Interrupt source     (1=Enable) (Read/Write)
    /// Bit 4 - Mode 1 VBlank STAT Interrupt source  (1=Enable) (Read/Write)
    /// Bit 3 - Mode 0 HBlank STAT Interrupt source  (1=Enable) (Read/Write)
    /// Bit 2 - LYC=LY Flag                          (0=Different, 1=Equal) (Read Only)
    /// Bit 1-0 - Mode Flag                          (Mode 0-3, see below) (Read Only)
    ///           0: HBlank
    ///           1: VBlank
    ///           2: Searching OAM
    ///           3: Transferring Data to LCD Controller
    graphics_status: [u8; 12],
    vram: [u8; 0x2000],
    oam: [u8; 160],
    /// Last Ticked cycle, for updating ppu in the correct spped
    last_tick_cycle: u64,
}

impl GraphicsController {
    pub fn new() -> GraphicsController {
        GraphicsController {
            graphics_status: [0x00; 12],
            /// VRAM Tile Data:
            /// 0 $8000 - $87FF
            /// 1 $8800 - $8FFF
            /// 2 $9000 - $97FF
            /// Tile Maps:
            /// Each tile map contains the 1-byte indexes of the tiles to be displayed.
            /// Tiles are obtained from the Tile Data Table using either of the two addressing modes (described
            ///  in VRAM Tile Data), which can be selected via the LCDC register.
            /// Since one tile has 8x8 pixels, each map holds a 256x256 pixels picture. 
            ///  Only 160x144 of those pixels are displayed on the LCD at any given time.
            /// 1. $9800-$9BFF
            /// 2. $9C00-$9FFF
            vram: [0x00; 0x2000],
            oam: [0x00; 160],
            last_tick_cycle: 0,
        }
    }
}

impl GraphicsController {
    pub fn tick(&mut self, cycle: u64) -> bool{
        self.last_tick_cycle = cycle;

        let mut req_stat_interrupt_res = false;

        // 154 Scanlines of which 0 to 153 are active and 144 to 153 are VBlank
        // 154 scanlines = 70224 cycles, 1 scanline = 456 Cycles
        let current_scanline = (cycle / 154) as u8;
        self.graphics_status[0x04] = current_scanline;
        if current_scanline == self.graphics_status[0x05]{
            // LY = LYC
            self.graphics_status[0x01] |= 0b0000_0100;
            // Bit 6 - LYC=LY STAT Interrupt source         (1=Enable) (Read/Write)
            if self.graphics_status[0x01] & 0b0100_0000 > 0{
                req_stat_interrupt_res = true;
            }
        } else {
            self.graphics_status[0x01] &= !0b0000_0100;
        }

        if current_scanline == 144 {
            self.print_current_frame();
        }

        // Rest of STAT Interrupt and Bit 1-0 of FF41 LCD status unimplemented
        // TODO:

        req_stat_interrupt_res
    }

    pub fn print_current_frame(&mut self) {
        let mut image_buffer = [[0_u8; 256] ; 256];

        let bg_tile_area: u16 = if self.graphics_status[0x00] & 0b0000_1000 > 0{ 0x9C00 } else { 0x9800 };
        let bg_window_addressing_mode = self.graphics_status[0x00] & 0b0001_0000 > 0; 
        
        // println!("Status: {:08b}", self.graphics_status[0x00]);

        for x in 0..32{
            for y in 0..32{
                let idx = x + 32 * y;
                let tile_idx = self.vram[(bg_tile_area - 0x8000 + idx) as usize];
                let tile_addr = if bg_window_addressing_mode{
                    (0x8000_u16 +  (tile_idx as u16) * 16) as usize - 0x8000
                } else {
                    (0x9000_i32 + (tile_idx as i32) * 16) as u16 as usize - 0x8000
                };

                for y_pixel in 0..8{
                    let first_byte = self.vram[tile_addr + y_pixel * 2];
                    // if first_byte != 0{
                    //     println!("first byte != 0: {:02x} at address {:02x} relative VRAM", first_byte, tile_addr + y_pixel);
                    // }

                    let second_byte = self.vram[tile_addr + y_pixel * 2 + 1];
                    for x_pixel in 0..8{
                        // No inverted Tiles
                        let x_pixel = 7 - x_pixel;
                        let pixel_color_idx = ((first_byte & (1 << x_pixel)) >> x_pixel) | (((second_byte & (1 << x_pixel)) >> x_pixel) << 1);
                        let pixel_color: u8 = [0x00, 0x55, 0xAA, 0xFF][pixel_color_idx as usize];

                        image_buffer[(y * 8 + y_pixel as u16) as usize][(x * 8 + x_pixel) as usize] = pixel_color;
                    }
                }
            }
        }

        // Save Image to Disk for now
        let img = ImageBuffer::from_fn(256, 256, |x, y| {
            let px = image_buffer[y as usize][x as usize];
            image::Rgb([px, px, px])
        });
        img.save("img_out/gb_screen.png").unwrap();
    }

    /// Executed by CPU if:
    /// if (0xFF40..=0xFF4B).contains(&addr)
    ///     || (0x8000..=0x9FFF).contains(&addr)
    ///     || (0xFE00..=0xFE9F).contains(&addr)
    pub fn memory_get(&self, addr: u16) -> u8 {
        if (0xFF40..=0xFF4B).contains(&addr) {
            // graphics_status
            self.graphics_status[(addr - 0xFF40) as usize]
        } else if (0x8000..=0x9FFF).contains(&addr) {
            // VRAM
            self.vram[(addr - 0x8000) as usize]
        } else if (0xFE00..=0xFE9F).contains(&addr) {
            // Object Attribute Memory
            self.oam[(addr - 0xFE00) as usize]
        } else {
            unreachable!()
        }
    }
    pub fn memory_set(&mut self, addr: u16, data: u8) {
        if (0xFF40..=0xFF4B).contains(&addr) {
            // graphics_status
            self.graphics_status[(addr - 0xFF40) as usize] = data;
        } else if (0x8000..=0x9FFF).contains(&addr) {
            // VRAM
            // println!("{:04x} = {:02x}", addr, data);
            self.vram[(addr - 0x8000) as usize] = data;
        } else if (0xFE00..=0xFE9F).contains(&addr) {
            // Object Attribute Memory
            self.oam[(addr - 0xFE00) as usize] = data;
        } else {
            unreachable!()
        }
    }
}

impl Default for GraphicsController {
    fn default() -> Self {
        GraphicsController::new()
    }
}
