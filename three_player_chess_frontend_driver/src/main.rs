use gl::types::*;
use glutin::{
    dpi::PhysicalSize,
    event::{ElementState, Event, KeyboardInput, MouseButton, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
    GlProfile,
};
use skia_safe::{
    gpu::{gl::FramebufferInfo, BackendRenderTarget, SurfaceOrigin},
    ColorType, Surface,
};

use three_player_chess::board::ThreePlayerChess;
use three_player_chess_board_eval::calculate_position_score;
use three_player_chess_engine::{score_str, Engine};
use three_player_chess_frontend::*;
use three_player_chess_paranoid_engine::ParanoidEngine;

struct FrontendDriver {
    engine: Engine,
    paranoid_engine: ParanoidEngine,
    autoplay: bool,
    autoplay_count: usize,
    autoplay_remaining: usize,
    fe: Frontend,
    engine_time_secs: u32,
    engine_depth: u16,
    go_infinite: bool,
    use_paranoid_engine: bool,
}

impl FrontendDriver {
    fn do_engine_move(&mut self) {
        let time = if self.go_infinite {
            10e6f32
        } else {
            self.engine_time_secs as f32
        };
        let result = if self.use_paranoid_engine {
            self.paranoid_engine
                .search_position(&self.fe.board, self.engine_depth, time, true)
        } else {
            self.engine
                .search_position(&self.fe.board, self.engine_depth, time, true)
        };
        if let Some(mov) = result {
            self.fe.apply_move_with_history(mov, self.fe.board.turn);
        }
    }
}

type WindowedContext = glutin::ContextWrapper<glutin::PossiblyCurrent, glutin::window::Window>;
struct Env {
    surface: Surface,
    gr_context: skia_safe::gpu::DirectContext,
    windowed_context: WindowedContext,
}

fn create_surface(
    windowed_context: &WindowedContext,
    fb_info: &FramebufferInfo,
    gr_context: &mut skia_safe::gpu::DirectContext,
    size: PhysicalSize<u32>,
) -> skia_safe::Surface {
    let pixel_format = windowed_context.get_pixel_format();

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

fn main() {
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

    let size = windowed_context.window().inner_size();

    let surface = create_surface(&windowed_context, &fb_info, &mut gr_context, size);
    // let sf = windowed_context.window().scale_factor() as f32;
    // surface.canvas().scale((sf, sf));
    // Guarantee the drop order inside the FnMut closure. `WindowedContext` _must_ be dropped after
    // `DirectContext`.
    //
    // https://github.com/rust-skia/rust-skia/issues/476

    let mut env = Env {
        surface,
        gr_context,
        windowed_context,
    };

    let mut fd = FrontendDriver {
        fe: Frontend::new(),
        engine: Engine::new(),
        paranoid_engine: ParanoidEngine::new(),
        autoplay: false,
        autoplay_count: 0,
        autoplay_remaining: 0,
        engine_time_secs: 3,
        engine_depth: 3,
        go_infinite: false,
        use_paranoid_engine: false,
    };

    fd.fe.update_dimensions(
        0,
        0,
        size.width.try_into().unwrap(),
        size.height.try_into().unwrap(),
    );

    el.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        #[allow(deprecated)]
        match event {
            Event::LoopDestroyed => {}
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(physical_size) => {
                    env.surface = create_surface(
                        &env.windowed_context,
                        &fb_info,
                        &mut env.gr_context,
                        physical_size,
                    );
                    env.windowed_context.resize(physical_size);
                    fd.fe.update_dimensions(
                        0,
                        0,
                        physical_size.width.try_into().unwrap(),
                        physical_size.height.try_into().unwrap(),
                    );
                }
                WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,
                WindowEvent::MouseInput { state, button, .. } => {
                    match state {
                        ElementState::Pressed => fd.fe.mouse_clicked(button == MouseButton::Right),
                        ElementState::Released => {
                            fd.fe.mouse_released(button == MouseButton::Right)
                        }
                    }
                    env.windowed_context.window().request_redraw();
                }
                WindowEvent::CursorMoved { position, .. } => {
                    fd.fe.mouse_moved(nalgebra::Vector2::<i32>::new(
                        position.x as i32,
                        position.y as i32,
                    ));
                    env.windowed_context.window().request_redraw();
                }
                WindowEvent::ModifiersChanged(modifiers) => {
                    if modifiers.ctrl() {
                        fd.fe.ctrl_pressed();
                    } else {
                        fd.fe.ctrl_released();
                    }
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
                        Some(VirtualKeyCode::V) => {
                            fd.fe.recolor();
                        }
                        Some(VirtualKeyCode::R) => {
                            fd.fe.reset();
                        }
                        Some(VirtualKeyCode::W) => {
                            println!(
                                "eval: {}",
                                score_str(calculate_position_score(&mut fd.fe.board))
                            );
                        }
                        Some(VirtualKeyCode::F) => {
                            fd.fe.rotate();
                        }
                        Some(VirtualKeyCode::D) => {
                            if modifiers.ctrl() {
                                fd.fe.highlight_capturable ^= true;
                                println!(
                                    "set highlighting of capturable pieces to {}",
                                    fd.fe.highlight_capturable
                                );
                            } else {
                                fd.fe.highlight_attacked ^= true;
                                println!(
                                    "set highlighting of attacked pieces to {}",
                                    fd.fe.highlight_attacked
                                );
                            }
                        }
                        Some(VirtualKeyCode::N) => {
                            fd.fe.show_notation ^= true;
                            println!("set show notation to {}", fd.fe.highlight_attacked);
                        }
                        Some(VirtualKeyCode::A) => {
                            if modifiers.shift() {
                                fd.autoplay_count = fd.autoplay_count.wrapping_sub(1);
                                println!("set autoplay count to {}", fd.autoplay_count);
                            } else if modifiers.ctrl() {
                                fd.autoplay_count = fd.autoplay_count.wrapping_add(1);
                                println!("set autoplay count to {}", fd.autoplay_count);
                            } else {
                                fd.autoplay ^= true;
                                println!("set autoplay to {}", fd.autoplay);
                            }
                        }
                        Some(VirtualKeyCode::I) => {
                            fd.go_infinite ^= true;
                            println!("set infinite search to {}", fd.go_infinite);
                        }
                        Some(VirtualKeyCode::Plus | VirtualKeyCode::Asterisk) => {
                            if modifiers.ctrl() {
                                fd.engine_depth = fd.engine_depth.saturating_add(1);
                                println!("set engine depth to {}", fd.engine_depth);
                            } else {
                                fd.engine_time_secs = fd.engine_time_secs.saturating_add(1);
                                println!("set engine time to {} s", fd.engine_time_secs);
                            }
                        }
                        Some(VirtualKeyCode::Minus) => {
                            if modifiers.ctrl() {
                                fd.engine_depth = fd.engine_depth.saturating_sub(1);
                                println!("set engine depth to {}", fd.engine_depth);
                            } else {
                                fd.engine_time_secs = fd.engine_time_secs.saturating_sub(1);
                                println!("set engine time to {} s", fd.engine_time_secs);
                            }
                        }
                        Some(VirtualKeyCode::E) => {
                            println!(
                                "running {} engine ... (depth: {}, time: {})",
                                if fd.use_paranoid_engine {
                                    "paranoid"
                                } else {
                                    "sane"
                                },
                                fd.engine_depth,
                                if fd.go_infinite {
                                    format!("infinite")
                                } else {
                                    format!("{} s", fd.engine_time_secs)
                                }
                            );
                            fd.do_engine_move();
                            if fd.autoplay {
                                fd.autoplay_remaining = fd.autoplay_count.saturating_sub(1);
                            }
                        }
                        Some(VirtualKeyCode::P) => {
                            fd.use_paranoid_engine ^= true;
                            println!(
                                "switching to {} engine",
                                if fd.use_paranoid_engine {
                                    "paranoid"
                                } else {
                                    "sane"
                                }
                            );
                        }
                        Some(VirtualKeyCode::T) => {
                            if modifiers.ctrl() {
                                fd.fe.transform_dragged_pieces ^= true;
                                println!(
                                    "set transform dragged pieces to {}",
                                    fd.fe.transform_dragged_pieces
                                );
                            } else {
                                fd.fe.transformed_pieces ^= true;
                                println!("set transform pieces to {}", fd.fe.transformed_pieces);
                            }
                        }
                        Some(VirtualKeyCode::U) => {
                            fd.fe.undo_move();
                        }
                        Some(VirtualKeyCode::L) => {
                            fd.engine.debug_log ^= true;
                            fd.paranoid_engine.debug_log ^= true;
                            println!("set debug log to {}", fd.engine.debug_log);
                        }

                        Some(VirtualKeyCode::Y) => {
                            fd.engine.transposition_table.clear();
                            fd.paranoid_engine.transposition_table.clear();
                            println!("transposition table cleared");
                        }
                        Some(VirtualKeyCode::Z) => {
                            let mut zh = fd.fe.board.zobrist_hash;
                            debug_assert!(
                                fd.fe.board.zobrist_hash.value == zh.recalc_zobrist(&fd.fe.board)
                            );
                            println!("zobrist hash: {:#018x}", fd.fe.board.zobrist_hash.value);
                        }
                        Some(VirtualKeyCode::S) => {
                            if modifiers.ctrl() {
                                println!("state: {}", fd.fe.board.state_string());
                            } else {
                                let mut buffer = String::new();
                                println!("please input state string:");
                                let stdin = std::io::stdin();
                                stdin.read_line(&mut buffer).unwrap();
                                let board = ThreePlayerChess::from_str(&buffer.trim());
                                if let Ok(board) = board {
                                    fd.fe.reset();
                                    fd.fe.board = board;
                                    println!("input string accepted");
                                } else {
                                    println!("failed to parse: {}", board.err().unwrap());
                                }
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
                    fd.fe.render(canvas);
                    canvas.restore();
                }
                env.surface.canvas().flush();
                env.windowed_context.swap_buffers().unwrap();
                if fd.autoplay_remaining > 0 {
                    if fd.fe.board.game_status() != three_player_chess::board::GameStatus::Ongoing {
                        fd.autoplay_remaining = 0;
                    } else {
                        fd.autoplay_remaining -= 1;
                        fd.do_engine_move();
                        env.windowed_context.window().request_redraw();
                    }
                }
            }
            _ => (),
        }
    });
}
