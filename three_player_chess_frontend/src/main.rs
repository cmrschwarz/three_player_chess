extern crate chrono;
extern crate gl;
extern crate glutin;
extern crate nanovg;

use glutin::GlContext;

const INIT_WINDOW_SIZE: (u32, u32) = (480, 480);

pub mod frontend;
use crate::frontend::{DrawContext, Frontend};

fn main() {
    let mut events_loop = glutin::EventsLoop::new();
    let window = glutin::WindowBuilder::new()
        .with_title("NanoVG Clock")
        .with_dimensions(INIT_WINDOW_SIZE.0, INIT_WINDOW_SIZE.1);
    let context = glutin::ContextBuilder::new()
        .with_vsync(true)
        .with_multisampling(4)
        .with_srgb(true);
    let gl_window = glutin::GlWindow::new(window, context, &events_loop).unwrap();

    unsafe {
        gl_window.make_current().unwrap();
        gl::load_with(|symbol| gl_window.get_proc_address(symbol) as *const _);
        gl::ClearColor(0.0, 0.0, 0.0, 1.0);
    }

    let context = nanovg::ContextBuilder::new()
        .stencil_strokes()
        .build()
        .expect("Initialization of NanoVG failed!");

    let mut running = true;

    let mut frontend = Frontend::new(&context);
    loop {
        events_loop.poll_events(|event| match event {
            glutin::Event::WindowEvent { event, .. } => match event {
                glutin::WindowEvent::Closed => running = false,
                glutin::WindowEvent::Resized(w, h) => {
                    gl_window.resize(w, h);
                }
                _ => {}
            },
            _ => {}
        });
        if !running {
            break;
        };
        let (width, height) = gl_window.get_inner_size().unwrap();
        context.frame(
            (width as f32, height as f32),
            gl_window.hidpi_factor(),
            |frame| {
                unsafe {
                    gl::Viewport(0, 0, width as i32, height as i32);
                    gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT | gl::STENCIL_BUFFER_BIT);
                }
                let dc = DrawContext {
                    frame,
                    width: width,
                    height: height,
                };
                frontend.render(&dc);
            },
        );

        gl_window.swap_buffers().unwrap();
    }
}
