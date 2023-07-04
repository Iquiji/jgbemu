use std::{thread::sleep, time::{Duration, Instant}};

use crate::runtime::{GameBoyEmulatorRuntime, run_test_roms::run};

use error_iter::ErrorIter as _;
use log::{debug, error, info};
use pixels::{Error, Pixels, SurfaceTexture};
use winit::{
    dpi::LogicalSize,
    event::{Event, VirtualKeyCode},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};
use winit_input_helper::WinitInputHelper;

const WIDTH: u32 = 160;
const HEIGHT: u32 = 144;


pub fn run_ui(rom: &str) -> Result<(), Box<dyn std::error::Error>>{
    env_logger::init();
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();

    let window = {
        let size = LogicalSize::new(WIDTH as f64, HEIGHT as f64);
        let scaled_size = LogicalSize::new(WIDTH as f64 * 3.0, HEIGHT as f64 * 3.0);
        WindowBuilder::new()
            .with_title("JGBEmu")
            .with_inner_size(scaled_size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };

    let mut pixels = {
        let window_size = window.inner_size();
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        Pixels::new(WIDTH, HEIGHT, surface_texture)?
    };

    let runtime: GameBoyEmulatorRuntime = GameBoyEmulatorRuntime::new();
    let screen_buffer_share = runtime.screen_buffer.clone();
    
    let rom = rom.to_string();
    let _thread_handle = std::thread::spawn(move || {
        let mut runtime = runtime;
        
        runtime.init(&rom); 

        let mut last_time = Instant::now();
        let mut last_cycle: u64 = runtime.cpu.cycle;

        let one_cycle_time = Duration::from_secs(1) / 4_194_304;

        loop{
            runtime.run_tick();
            // let current_cycle: u64 = runtime.cpu.cycle;
            // let new_time = Instant::now();

            // let delta_cycle = (current_cycle - last_cycle) as u32;
            // let expected_time = one_cycle_time * delta_cycle;
            
            // let delta_time = new_time - last_time;

            // last_cycle = current_cycle;
            // last_time = new_time;
            // // sleep(expected_time - delta_time);
            // println!("expected_time: {:?}, delta_time: {:?}, diff: {:?}",expected_time, delta_time, expected_time - delta_time);
        }
    });




    event_loop.run(move |event, _, control_flow| {
        if let Event::RedrawRequested(_) = event {
            // let cycle_before = runtime.cpu.cycle;

            let unflat_buffer = screen_buffer_share.lock().unwrap();
            // let unflat_buffer_slice = unflat_buffer.fl();
            let frame = pixels.frame_mut();
            
            // pixel_buffer.fill(0x00);

            for (idx, pixel) in frame.chunks_exact_mut(4).enumerate(){
                let x = idx % 160;
                let y = idx / 160;

                pixel[0] = unflat_buffer[y][x];
                pixel[1] = unflat_buffer[y][x];
                pixel[2] = unflat_buffer[y][x];
                pixel[3] = 0xFF;
            }
            drop(unflat_buffer);

            info!("Redraw finished");
            if let Err(err) = pixels.render() {
                log_error("pixels.render", err);
                *control_flow = ControlFlow::Exit;
                return;
            }
        }
        if input.update(&event){
            // if let Some(size) = input.window_resized() {
            //     if let Err(err) = pixels.resize_surface(size.width, size.height) {
            //         log_error("pixels.resize_surface", err);
            //         *control_flow = ControlFlow::Exit;
            //         return;
            //     }
            // }
            window.request_redraw();
        }
    });
}


/// :)
fn log_error<E: std::error::Error + 'static>(method_name: &str, err: E) {
    error!("{method_name}() failed: {err}");
    for source in err.sources().skip(1) {
        error!("  Caused by: {source}");
    }
}