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
    image_buffer: [[u8; 160]; 144],
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
            image_buffer: [[0; 160]; 144],
        }
    }
}

impl GraphicsController {
    pub fn tick(&mut self, cycle: u64) -> bool {
        let mut req_stat_interrupt_res = false;

        // 154 Scanlines of which 0 to 153 are active and 144 to 153 are VBlank
        // 154 scanlines = 70224 cycles, 1 scanline = 456 Cycles
        let current_scanline = ((cycle / 456) % 154) as u8;
        self.graphics_status[0x04] = current_scanline;
        // Only on new Scanline
        if current_scanline > ((self.last_tick_cycle / 456) % 154) as u8 {
            if current_scanline == self.graphics_status[0x05] {
                // LY = LYC
                self.graphics_status[0x01] |= 0b0000_0100;
                // Bit 6 - LYC=LY STAT Interrupt source         (1=Enable) (Read/Write)
                if self.graphics_status[0x01] & 0b0100_0000 > 0 {
                    req_stat_interrupt_res = true;
                }
            } else {
                self.graphics_status[0x01] &= !0b0000_0100;
            }
        }

        // this is executed too often but lets fix that later
        // I mean multiple times per scanline potentially
        if current_scanline < 144 {
            self.render_line(current_scanline);
        }

        if current_scanline == 144 {
            self.print_current_frame();
        }

        // Rest of STAT Interrupt Bit 1-0 of FF41 LCD status unimplemented
        if current_scanline < 144 {
            // Mode 3 and 0 inaccurate for now
            if cycle % 456 <= 80 {
                // Mode 2 - Searching OAM
                self.graphics_status[0x01] &= 0b1111_1100;
                self.graphics_status[0x01] |= 0b0000_0010;
                if self.graphics_status[0x01] & 0b0010_0000 > 0 {
                    req_stat_interrupt_res = true;
                }
            } else if cycle % 456 <= 380 {
                // Mode 3 - Transferring Data to LCD Controller
                self.graphics_status[0x01] &= 0b1111_1100;
                self.graphics_status[0x01] |= 0b0000_0011;
            } else {
                // Hblank
                self.graphics_status[0x01] &= 0b1111_1100;
                self.graphics_status[0x01] |= 0b0000_0000;
                if self.graphics_status[0x01] & 0b0000_1000 > 0 {
                    req_stat_interrupt_res = true;
                }
            }
        } else {
            // Mode VBlank
            self.graphics_status[0x01] &= 0b1111_1100;
            self.graphics_status[0x01] |= 0b0000_0001;
            if self.graphics_status[0x01] & 0b0001_0000 > 0 {
                req_stat_interrupt_res = true;
            }
        }

        self.last_tick_cycle = cycle;
        req_stat_interrupt_res
    }

    /// Render a line
    /// May need to be pixel/dot based in the future for more precision
    pub fn render_line(&mut self, line: u8) {
        let bg_window_enable = self.graphics_status[0x00] & 0b0000_0001 > 0;
        let window_enable = self.graphics_status[0x00] & 0b0010_0000 > 0;
        let obj_enable = self.graphics_status[0x00] & 0b0000_0010 > 0;

        let obj_size_big = self.graphics_status[0x00] & 0b0000_0100 > 0;

        let window_tile_area = if self.graphics_status[0x00] & 0b0000_1000 > 0 {
            0x9C00
        } else {
            0x9800
        };
        let bg_tile_area: u16 = if self.graphics_status[0x00] & 0b0000_1000 > 0 {
            0x9C00
        } else {
            0x9800
        };
        let bg_window_addressing_mode = self.graphics_status[0x00] & 0b0001_0000 > 0;

        let bg_scroll_y = self.graphics_status[0x02];
        let bg_scroll_x = self.graphics_status[0x03];

        let window_pos_y = self.graphics_status[0x0A];
        let window_pos_x = self.graphics_status[0x0B];

        let bg_palette_data = self.graphics_status[0x07];
        // Palette Arrangement Readout: https://gbdev.io/pandocs/Palettes.html
        let color_idx_idx: [u8; 4] = [
            (bg_palette_data & 0b0000_0011),
            (bg_palette_data & 0b0000_1100) >> 2,
            (bg_palette_data & 0b0011_0000) >> 4,
            (bg_palette_data & 0b1100_0000) >> 6,
        ];
        let color_lookup: [u8; 4] = [0xFF, 0xAA, 0x55, 0x00];

        // Render Background
        let bg_y = line.wrapping_add(bg_scroll_y);

        for x in 0_u8..160 {
            if bg_window_enable {
                let bg_x = x.wrapping_add(bg_scroll_x);

                let tile_idx_x = bg_x / 8;
                let tile_idx_y = bg_y / 8;

                let tile_idx = self.vram[(bg_tile_area - 0x8000_u16
                    + tile_idx_x as u16
                    + 32 * tile_idx_y as u16) as usize];
                let tile_addr: u16 = if bg_window_addressing_mode {
                    tile_idx as u16 * 16
                } else {
                    (0x1000_i32 + (tile_idx as i8 as i32) * 16) as u16
                };
                let first_byte = self.vram[(tile_addr + (line as u16 % 8) * 2) as usize];
                let second_byte = self.vram[(tile_addr + (line as u16 % 8) * 2 + 1) as usize];

                let bit = 7 - (bg_x % 8);
                let first_bit = first_byte & (1 << bit);
                let second_bit = second_byte & (1 << bit);
                let pixel_color_idx = (first_bit >> bit) | ((second_bit >> bit) << 1);

                self.image_buffer[line as usize][x as usize] =
                    color_lookup[color_idx_idx[pixel_color_idx as usize] as usize];
            } else {
                self.image_buffer[line as usize][x as usize] = color_lookup[0];
            }
        }

        // Render Window
        if bg_window_enable && window_enable && line >= window_pos_y {
            // the sub part is still unsure
            for x in (window_pos_x.saturating_sub(7))..160 {
                let tile_idx_x = x / 8;
                let tile_idx_y = line / 8;

                let tile_idx = self.vram[(window_tile_area - 0x8000_u16
                    + tile_idx_x as u16
                    + 32 * tile_idx_y as u16) as usize];
                let tile_addr: u16 = if bg_window_addressing_mode {
                    (0x8000_u16 + (tile_idx as u16) * 16) - 0x8000
                } else {
                    (0x9000_i32 + (tile_idx as i32) * 16) as u16 - 0x8000
                };
                let first_byte = self.vram[(tile_addr + (line as u16 % 8) * 2) as usize];
                let second_byte = self.vram[(tile_addr + (line as u16 % 8) * 2 + 1) as usize];

                let bit = 7 - (x % 8);
                let first_bit = first_byte & (1 << bit);
                let second_bit = second_byte & (1 << bit);
                let pixel_color_idx = (first_bit >> bit) | ((second_bit >> bit) << 1);

                self.image_buffer[line as usize][x as usize] =
                    color_lookup[color_idx_idx[pixel_color_idx as usize] as usize];
            }
        }

        // Render Objects
        let obj_palette_data_0 = self.graphics_status[0x08];
        // Palette Arrangement Readout: https://gbdev.io/pandocs/Palettes.html
        let obj_palette_0_color_idx_idx: [u8; 4] = [
            0x00,
            (obj_palette_data_0 & 0b0000_1100) >> 2,
            (obj_palette_data_0 & 0b0011_0000) >> 4,
            (obj_palette_data_0 & 0b1100_0000) >> 6,
        ];
        let obj_palette_data_1 = self.graphics_status[0x09];
        // Palette Arrangement Readout: https://gbdev.io/pandocs/Palettes.html
        let obj_palette_1_color_idx_idx: [u8; 4] = [
            0x00,
            (obj_palette_data_1 & 0b0000_1100) >> 2,
            (obj_palette_data_1 & 0b0011_0000) >> 4,
            (obj_palette_data_1 & 0b1100_0000) >> 6,
        ];

        if obj_enable {
            // if obj_size_big {
            //     unimplemented!("OBJ Size 8x16 unimplemented in renderer");
            // }

            // Fill all ten OBJ slots for scanline
            let mut first_ten_filled_counter = 0;
            let mut first_ten_scanline_obj = [0; 10];
            for obj in 0u16..40 {
                if first_ten_filled_counter >= 10 {
                    break;
                }

                let addr = obj * 4;
                let pos_y = self.oam[(addr) as usize];

                if (obj_size_big && ((pos_y as i32 - line as i32 - 16) > 0 && (pos_y as i32 - line as i32 - 16) < 16))
                    || ((pos_y as i32 - line as i32 - 8) > 0 && (pos_y as i32 - line as i32 - 16) < 8)
                {
                    first_ten_scanline_obj[first_ten_filled_counter] = addr;
                    first_ten_filled_counter += 1;
                }
            }

            for obj_addr in first_ten_scanline_obj.iter().take(first_ten_filled_counter) {
                let pos_x = self.oam[(*obj_addr + 1) as usize] as i16 - 8;
                let tile_idx = self.oam[(*obj_addr + 2) as usize];
                let flags = self.oam[(*obj_addr + 3) as usize];

                let flags_bg_over_obj = (flags & 0b1000_0000) > 0;
                let y_flip = (flags & 0b0100_0000) > 0;
                let x_flip = (flags & 0b0010_0000) > 0;
                let palette_number = (flags & 0b0001_0000) >> 4;

                for pixel_x in 0..8 {
                    if (pos_x + pixel_x) > 0 {
                        let pos_x = (pos_x + pixel_x) as u8;
                        
                        // Render Tile
                        let tile_idx = self.vram[tile_idx as usize];
                        let tile_addr: u16 = (tile_idx as u16) * 16;
                        
                        // Distinction from big obj not thought through
                        let first_byte = 
                            self.vram[(tile_addr + (line as u16 % 8) * 2) as usize];
                        let second_byte =
                            self.vram[(tile_addr + (line as u16 % 8) * 2 + 1) as usize];

                        let bit = 7 - (pixel_x % 8);
                        let first_bit = first_byte & (1 << bit);
                        let second_bit = second_byte & (1 << bit);
                        let pixel_color_idx = (first_bit >> bit) | ((second_bit >> bit) << 1);
                        
                        
                        // Consider Transparency
                        if palette_number == 0
                            && pixel_color_idx != 0
                        {
                            self.image_buffer[line as usize][pos_x as usize] = color_lookup
                                [obj_palette_0_color_idx_idx[pixel_color_idx as usize]
                                    as usize];
                        } else if pixel_color_idx != 0 {
                            self.image_buffer[line as usize][pos_x as usize] = color_lookup
                                [obj_palette_1_color_idx_idx[pixel_color_idx as usize]
                                    as usize];
                        }
                    }
                }
            }
        }
    }

    pub fn print_current_frame(&mut self) {
        // Save Image to Disk for now
        let img = ImageBuffer::from_fn(160, 144, |x, y| {
            let px = self.image_buffer[y as usize][x as usize];
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
