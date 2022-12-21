#[macro_use]
extern crate lazy_static;

use gl::types::*;
use glutin::{
    event::{ElementState, Event, KeyboardInput, MouseButton, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
    GlProfile,
};
use skia_safe::{
    gpu::{gl::FramebufferInfo, BackendRenderTarget, SurfaceOrigin},
    ColorType, Surface,
};

mod frontend;
use frontend::*;
use three_player_chess::board::ThreePlayerChess;
use three_player_chess_board_eval::calculate_position_score;
use three_player_chess_engine::score_str;
fn main() {
    type WindowedContext = glutin::ContextWrapper<glutin::PossiblyCurrent, glutin::window::Window>;

    let el = EventLoop::new();
    let wb = WindowBuilder::new().with_title("rust-skia-gl-window");

    let cb = glutin::ContextBuilder::new()
        .with_depth_buffer(0)
        .with_stencil_buffer(8)
        .with_pixel_format(24, 8)
        .with_gl_profile(GlProfile::Core);

    #[cfg(not(feature = "wayland"))]
    let cb = cb.with_double_buffer(Some(true));

    let windowed_context = cb.build_windowed(wb, &el).unwrap();

    let windowed_context = unsafe { windowed_context.make_current().unwrap() };

    gl::load_with(|s| windowed_context.get_proc_address(s));

    let mut gr_context = skia_safe::gpu::DirectContext::new_gl(None, None).unwrap();

    let fb_info = {
        let mut fboid: GLint = 0;
        unsafe { gl::GetIntegerv(gl::FRAMEBUFFER_BINDING, &mut fboid) };

        FramebufferInfo {
            fboid: fboid.try_into().unwrap(),
            format: skia_safe::gpu::gl::Format::RGBA8.into(),
        }
    };

    windowed_context
        .window()
        .set_inner_size(glutin::dpi::Size::new(glutin::dpi::LogicalSize::new(
            1024.0, 1024.0,
        )));

    fn create_surface(
        windowed_context: &WindowedContext,
        fb_info: &FramebufferInfo,
        gr_context: &mut skia_safe::gpu::DirectContext,
    ) -> skia_safe::Surface {
        let pixel_format = windowed_context.get_pixel_format();
        let size = windowed_context.window().inner_size();
        let backend_render_target = BackendRenderTarget::new_gl(
            (
                size.width.try_into().unwrap(),
                size.height.try_into().unwrap(),
            ),
            pixel_format.multisampling.map(|s| s.try_into().unwrap()),
            pixel_format.stencil_bits.try_into().unwrap(),
            *fb_info,
        );
        Surface::from_backend_render_target(
            gr_context,
            &backend_render_target,
            SurfaceOrigin::BottomLeft,
            ColorType::RGBA8888,
            None,
            None,
        )
        .unwrap()
    }

    let surface = create_surface(&windowed_context, &fb_info, &mut gr_context);
    // let sf = windowed_context.window().scale_factor() as f32;
    // surface.canvas().scale((sf, sf));
    // Guarantee the drop order inside the FnMut closure. `WindowedContext` _must_ be dropped after
    // `DirectContext`.
    //
    // https://github.com/rust-skia/rust-skia/issues/476
    struct Env {
        surface: Surface,
        gr_context: skia_safe::gpu::DirectContext,
        windowed_context: WindowedContext,
    }

    let mut env = Env {
        surface,
        gr_context,
        windowed_context,
    };

    let mut fe = Frontend::new();

    el.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        #[allow(deprecated)]
        match event {
            Event::LoopDestroyed => {}
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(physical_size) => {
                    env.surface =
                        create_surface(&env.windowed_context, &fb_info, &mut env.gr_context);
                    env.windowed_context.resize(physical_size)
                }
                WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                WindowEvent::MouseInput {
                    state,
                    button: MouseButton::Left,
                    ..
                } => {
                    match state {
                        ElementState::Pressed => fe.clicked(),
                        ElementState::Released => fe.released(),
                    }
                    env.windowed_context.window().request_redraw();
                }
                WindowEvent::CursorMoved { position, .. } => {
                    fe.cursor_pos =
                        nalgebra::Vector2::<i32>::new(position.x as i32, position.y as i32);
                    env.windowed_context.window().request_redraw();
                }
                WindowEvent::KeyboardInput {
                    input:
                        KeyboardInput {
                            virtual_keycode,
                            state: ElementState::Pressed,
                            modifiers,
                            ..
                        },
                    ..
                } => {
                    match virtual_keycode {
                        Some(VirtualKeyCode::Q) => {
                            *control_flow = ControlFlow::Exit;
                        }
                        Some(VirtualKeyCode::F) => {
                            fe.transformed_pieces ^= true;
                            println!("set transform pieces to {}", fe.transformed_pieces);
                        }
                        Some(VirtualKeyCode::C) => {
                            fe.recolor();
                        }
                        Some(VirtualKeyCode::S) => {
                            fe.reset();
                        }
                        Some(VirtualKeyCode::W) => {
                            println!(
                                "eval: {}",
                                score_str(calculate_position_score(&mut fe.board).0)
                            );
                        }
                        Some(VirtualKeyCode::R) => {
                            fe.rotate();
                        }
                        Some(VirtualKeyCode::A) => {
                            fe.autoplay ^= true;
                            println!("set autoplay to {}", fe.autoplay);
                        }
                        Some(VirtualKeyCode::D) => {
                            // #deep
                            fe.go_infinite ^= true;
                            println!("set infinite search to {}", fe.go_infinite);
                        }
                        Some(VirtualKeyCode::Plus | VirtualKeyCode::Asterisk) => {
                            if modifiers.ctrl() {
                                fe.engine_depth = fe.engine_depth.saturating_add(1);
                                println!("set engine depth to {}", fe.engine_depth);
                            } else {
                                fe.engine_time_secs = fe.engine_time_secs.saturating_add(1);
                                println!("set engine time to {} s", fe.engine_time_secs);
                            }
                        }
                        Some(VirtualKeyCode::Minus) => {
                            if modifiers.ctrl() {
                                fe.engine_depth = fe.engine_depth.saturating_sub(1);
                                println!("set engine depth to {}", fe.engine_depth);
                            } else {
                                fe.engine_time_secs = fe.engine_time_secs.saturating_sub(1);
                                println!("set engine time to {} s", fe.engine_time_secs);
                            }
                        }
                        Some(VirtualKeyCode::E) => {
                            println!(
                                "running {} engine ... (depth: {}, time: {})",
                                if fe.use_paranoid_engine {
                                    "paranoid"
                                } else {
                                    "sane"
                                },
                                fe.engine_depth,
                                if fe.go_infinite {
                                    format!("infinite")
                                } else {
                                    format!("{} s", fe.engine_time_secs)
                                }
                            );
                            fe.do_engine_move();
                        }
                        Some(VirtualKeyCode::P) => {
                            fe.use_paranoid_engine ^= true;
                            println!(
                                "switching to {} engine",
                                if fe.use_paranoid_engine {
                                    "paranoid"
                                } else {
                                    "sane"
                                }
                            );
                        }
                        Some(VirtualKeyCode::T) => {
                            fe.transform_dragged_pieces ^= true;
                            println!(
                                "set transform dragged pieces to {}",
                                fe.transform_dragged_pieces
                            );
                        }
                        Some(VirtualKeyCode::U) => {
                            fe.undo_move();
                        }
                        Some(VirtualKeyCode::L) => {
                            fe.engine.debug_log ^= true;
                            println!("set debug log to {}", fe.engine.debug_log);
                        }
                        Some(VirtualKeyCode::X) => {
                            println!("{}", fe.board.state_string());
                        }
                        Some(VirtualKeyCode::Z) => {
                            let mut zh = fe.board.zobrist_hash;
                            debug_assert!(
                                fe.board.zobrist_hash.value == zh.recalc_zobrist(&fe.board)
                            );
                            println!("zobrist hash: {:#018x}", fe.board.zobrist_hash.value);
                        }
                        Some(VirtualKeyCode::I) => {
                            let mut buffer = String::new();
                            println!("please input state string:");
                            let stdin = std::io::stdin();
                            stdin.read_line(&mut buffer).unwrap();
                            let board = ThreePlayerChess::from_str(&buffer.trim());
                            if let Ok(board) = board {
                                fe.reset();
                                fe.board = board;
                                println!("input string accepted");
                            } else {
                                println!("failed to parse: {}", board.err().unwrap());
                            }
                        }
                        _ => (),
                    }
                    env.windowed_context.window().request_redraw();
                }
                _ => (),
            },
            Event::RedrawRequested(_) => {
                {
                    let canvas = env.surface.canvas();
                    canvas.save();
                    fe.render(canvas);
                    canvas.restore();
                }
                env.surface.canvas().flush();
                env.windowed_context.swap_buffers().unwrap();
                fe.post_render_event();
            }
            _ => (),
        }
    });
}
