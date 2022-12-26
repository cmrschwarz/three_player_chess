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
    // for testing purposes, we usually set some interesting position here for engine debugging
    fn setup_board(&mut self) {
        // G2F3A5///C6Hc//E2//:KA7L5I9///J7/A1/B7//:G3F4ILaGJ9///Lc//Eb//:135:141 white eval busted?
        // F2C3H4A5///H1//G1//:LKJCBA7/J6/C6/LC8/Hc/B8//:FLbJKaG9//F3Ia/Jc/Ec/Ib//:88:88 black hangs mate?
        // BDGH2A3D4/G3/CF1/BH1/Ic/D1//:JCA7I6B5I9//D7D6/C8/L8/B8//:G4FLbHEaJ9//Eb/Hc/Ib/Ka//:73:77
        // AF2I5Eb//H4///G9//:B4LB7IC6K9//C8Fc/KA8/H1/I8//:HbKa///J7/H2/Kc//:103:103 black refuses to take blues queen
        // FH2ABE3//D2/C1G2/Jb/A2//:LKBA7J6/D5/C6/LA8//B8//:HFKLb///K9//Fc//:79:80 blue fails to save himself
        // BCFG2A3H4//G9/DE1/E2/G1//:AD4KD7B5L9/I5/DC6/LI8//K8//:HGJbLa/E9/K5Ib/ELc/Ia/Gc//:96:96 paranoid hangs his bishop
        // BCFG2AD3H4/E9/G9/DE1/E2/H2//:A4KD7CB5L9/I5/D6Kb/LI8//K8//:HGJbLa/Fa/K5Ib/ELc/Ia/Gc//:88:90 Qh9??
        // BCFG2ADH3/E9/G9/DE1/E2/G1//:A4KD7CB5L9/I5/B7D6/LB8//K8//:HGJKbLa/Fa/K5Ea/JLc/Eb/Gc//:75:82 weird knight trade
        // ABCDFGH2E9/BG1/C1J7/AH1/D1/E1/AH/:LKDCBA7Ea/B8I7/JC8/LA8/I8/D8/LA/:HGFIJKLb/KcJa/K5Jc/HLc/Ec/Ic/HL/:11:12 white refuses to take queen
        // BDFGH2ACE3/B1H4/CF1/AH1/F3/E1/AH/:LJDCA7K6IB5/C6L5/JC8/LA8/I8/D8/LA/:GIJKLbFaHE9/KcEb/JcIa/HLc/Fb/Ic/HL/:14:18 white self destructs
        // AF2H3I5Ea//H4//B4/G3//:LB7IC6A5K9//C8Gb/KA8/H1/I8//:HbKa///J7Fc/D6/Kc//:93:95 blue self destructs
        // ABCF2GH3D4Ea/G1B5/CF1/AH1/C4/E1/AH/:LJDCBA7I6K5/C6I9/C8I7/LA8/I8/D8/LA/:I5HGJLbKa/GcJa/H4Kb/HLc/Ec/Ic/HL/:28:28  Nxi5 !!
        // BCFH2A3E4F9/B5/D2/DE1/H4/G1//:LCA7C6J5//D7E9/I8//B7//:HGFKbKLa/Fa/L6/HcJb/Ja/Kc//:76:76   movegen bug repro
        // BCFH2AH3/E9/D3Ia/A1La//B1//://///D4//:L7/////Ea//:189:336
        // BFGH2E3D5//F1/H1/C8/E1/H/:D4LKJ7/B4K9/I7J5/L8//B6//:GFIKLbHaEI9/C3/Jc/HKc/Ec/Ic/H/:59:59
        // BFGH2E3D5/F3/F1/H1/I8/E1/H/:LKJ7C5/B4K9/I7J5/L8//C7//:GFIKLbHEaI9/C3C8/Jc/HKc/Ec/Ic/H/:54:54
        // ABEG2G3D7///D1Ib//C1//:CBA7LK5///K8J7//B8//K6:G4GLbLaE9/////L9//:68:68
        // ABEG2G3D7///D2Ib//C1//:CBA7K5///J8J7//B8//:G4L7GLbE9/////K9//:84:84 white has to sac a pawn/queen to checkmate
        // ABCFGH2DE4/B1H3/CF1/AH1/B5/E1/AH/:LKJICBA7D5/J6I5/JC8/LA8//D8/LA/:GFEJKbHLaI9/GcJa/FcEa/HLc/Ec/Ic/HL/:10:15
        // ABCEFGH2D3/B5G9/CF1/AH1/D1/E1/AH/:LKJDCBA7I9/JC6/JC8/KA8/I8/D8/A/:GFEJKLbIaH9/FLa/FJc/GLc/Ec/Ic/L/:15:15
        // CEFGH2A5E4D5H4C5/BG1/CF1/AH1/D4/E1/AH/:LKIDCBA7J6/KB8/JC8/LA8/L5/D8/LA/:HGFEILbJaK9B2/GKc/FJc/HLc/Ec/Ic/HL/Ka:0:0
        // ABCEFGH2D3/B5G9/F1D2/AH1/D1/E1/AH/:LKJDCBA7I9/JC6/JC8/LA8/I8/D8/A/:GFEJKLbH9/FLa/FJc/GLc/Ec/Ic/L/:15:17
        // ABG2H3/E2C3/A6/B1Fb/B3/E1//:LKJC7B6I9///IC8/Lc/B8//:GJLbHEa//JcKa/Gc/E9/Ic//:66:66
        // ABG2H3/E2C3/C8/B1Jb/B3/E1//:LKJC7B6///I9/Ja/B8//:GLbHa//JcKa/Gc/E9/Ic//:71:71
        self.fe.board = ThreePlayerChess::from_str(
            "G2F3A5///C6Hc//E2//:KA7L5I9///J7/A1/B7//:G3F4ILaGJ9///Lc//Eb//:135:141",
        )
        .unwrap();
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
    fd.setup_board();

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
                                if (!fd.autoplay) {
                                    fd.autoplay_remaining = 0;
                                }
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
