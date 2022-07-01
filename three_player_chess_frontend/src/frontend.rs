use chrono::prelude::*;
use nanovg as nv;
use nanovg::{
    Alignment, BasicCompositeOperation, CompositeOperation, Context, Font, Frame, Gradient,
    PathOptions, StrokeOptions, TextOptions, Transform,
};
use rand::Rng;
use rand_chacha::rand_core::{RngCore, SeedableRng};
use vecmath::{vec2_add, vec2_cast, vec2_mul, vec2_scale, vec2_sub, Vector2};

use std::f32::consts::PI;
use std::{thread, time};

use three_player_chess::board::*;

pub struct DrawContext<'a> {
    pub frame: Frame<'a>,
    pub width: u32,
    pub height: u32,
}

pub const FONT: &'static [u8] = include_bytes!("../res/Roboto-Regular.ttf");
pub struct Frontend<'a> {
    font: Font<'a>,
    board: ThreePlayerChess,
    prev_second: f32,
}
const HBRC: f32 = HB_ROW_COUNT as f32;

fn point(vec2: Vector2<f32>) -> (f32, f32) {
    (vec2[0], vec2[1])
}

impl<'a> Frontend<'a> {
    pub fn new(ctx: &'a Context) -> Frontend<'a> {
        Frontend {
            prev_second: -1.0,
            board: Default::default(),
            font: nanovg::Font::from_memory(&ctx, "Roboto", FONT)
                .expect("Failed to load font 'Roboto-Regular.ttf'"),
        }
    }
    pub fn render(&mut self, dc: &DrawContext<'a>) {
        for c in Color::iter() {
            for right in [true, false] {
                self.render_hex_board(dc, *c, right);
            }
        }
    }
    pub fn render_hex_board(&mut self, dc: &DrawContext, hb: Color, right: bool) {
        let hex_num = (u8::from(hb) * 2 + u8::from(right) + 5) % 6; // bottom right is hex 0
        let rotation = (hex_num as f32 / 6.0) * 2.0 * PI;
        let radius = std::cmp::min(dc.width, dc.height) as f32 * 0.45;
        let center_angle_half = 2. * PI / 12.;
        let side_len = center_angle_half.sin() * radius;
        let hex_height = center_angle_half.cos() * radius;

        let bottom_right = [side_len, hex_height];

        let bottom_left = [0., hex_height];
        let top_right = [side_len + (radius - side_len) / 2., hex_height / 2.];
        let right_flank = vec2_sub(top_right, bottom_right);

        let transform = Some(
            Transform::new()
                .translate(dc.width as f32 / 2., dc.height as f32 / 2.)
                .rotate(rotation),
        );
        let frac = 1. / HB_ROW_COUNT as f32;
        dc.frame.path(
            |path| {
                path.move_to(point([0., 0.]));
                path.line_to(point(bottom_left));
                path.line_to(point(bottom_right));
                path.line_to(point(top_right));
                path.line_to(point([0., 0.]));
                path.fill(
                    nv::Color::from_rgb(255, 255, 255),
                    nv::FillOptions { antialias: true },
                );
            },
            PathOptions {
                composite_operation: CompositeOperation::Basic(BasicCompositeOperation::Lighter),
                alpha: 1.0, //elapsed.cos() * 0.5 + 0.5,
                transform,
                ..Default::default()
            },
        );
        for file in 0..HB_ROW_COUNT {
            for rank in 0..HB_ROW_COUNT {
                let mut rng = rand_chacha::ChaChaRng::seed_from_u64(
                    hex_num as u64 + file as u64 + rank as u64 * 4,
                );

                dc.frame.path(
                    |path| {
                        let f = file as f32;
                        let r = rank as f32;
                        let center_bot = [0., hex_height * (HBRC - r) * frac];
                        let center_top = [0., hex_height * (HBRC - r - 1.) * frac];
                        let file_vec_bot = vec2_sub(
                            vec2_add(bottom_right, vec2_scale(right_flank, r * frac)),
                            center_bot,
                        );
                        let file_vec_top = vec2_sub(
                            vec2_add(bottom_right, vec2_scale(right_flank, (r + 1.) * frac)),
                            center_top,
                        );
                        let tl = vec2_add(center_top, vec2_scale(file_vec_top, f * frac));
                        let tr = vec2_add(center_top, vec2_scale(file_vec_top, (f + 1.) * frac));
                        let bl = vec2_add(center_bot, vec2_scale(file_vec_bot, f * frac));
                        let br = vec2_add(center_bot, vec2_scale(file_vec_bot, (f + 1.) * frac));
                        path.move_to(point(bl));
                        path.line_to(point(br));
                        path.line_to(point(tr));
                        path.line_to(point(tl));
                        path.line_to(point(bl));
                        path.fill(
                            nv::Color::from_rgb(
                                rng.next_u32() as u8,
                                rng.next_u32() as u8,
                                rng.next_u32() as u8,
                            ),
                            nv::FillOptions { antialias: true },
                        );
                    },
                    PathOptions {
                        alpha: 1.0,
                        transform,
                        ..Default::default()
                    },
                );
            }
        }
    }
    pub fn render_clock(&mut self, dc: &DrawContext) {
        let dt: DateTime<Local> = Local::now(); // e.g. `2014-11-28T21:45:59.324310806+09:00`
        let hour = dt.hour();
        let am = hour < 12;
        let hour: f32 = f64::from(hour % 12) as f32;
        let minute: f32 = f64::from(dt.minute()) as f32;
        let second: f32 = f64::from(dt.second()) as f32;
        let year: i32 = dt.year();
        let month: u32 = dt.month();
        let day: u32 = dt.day();

        // don't bother re-draw unless time has changed
        if second == self.prev_second {
            let frame_time = time::Duration::from_millis(33);
            thread::sleep(frame_time);
        } else {
            self.prev_second = second;
        }

        unsafe {
            gl::Viewport(0, 0, dc.width as i32, dc.height as i32);
            gl::Clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT | gl::STENCIL_BUFFER_BIT);
        }

        // round clock size is minimum of height and width
        let clock_size = dc.width.min(dc.height) - 2;

        let font_size = 24.0;

        let origin = (0.0, 0.0); // upper-left corner
        let dial_center = (
            f64::from(dc.width) as f32 / 2.0,
            f64::from(dc.height) as f32 / 2.0,
        );
        let dial_radius: f32 = f64::from(clock_size / 2) as f32;
        let second_hand_len = dial_radius * 0.9;
        let minute_hand_len = dial_radius * 0.8;
        let hour_hand_len = dial_radius * 0.6;

        let two_pi = 2.0 * PI;
        let radians_per_sec = two_pi / 60.0;
        let radians_per_hour = two_pi / 12.0;

        let white: nv::Color = nv::Color::new(1.0, 1.0, 1.0, 1.0);
        let silver: nv::Color = nv::Color::from_rgb(196, 199, 206);
        let darksilver: nv::Color = nv::Color::from_rgb(148, 152, 161);
        let darkgray: nv::Color = nv::Color::from_rgb(169, 169, 169);
        let dial_color = nv::Color::new(0.2, 0.0, 0.8, 1.0);
        // hour/minute markers

        //let sigils = ["XII", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"];
        let sigils: Vec<String> = (0..13).map(|n| format!("{}", n)).collect();
        for h in 1..13 {
            let j = h as f32;
            let x = dial_center.0 + (second_hand_len * (j * radians_per_hour).sin());
            let y = dial_center.1 - (second_hand_len * (j * radians_per_hour).cos());
            dc.frame.text(
                self.font,
                (x, y),
                &sigils[h as usize],
                TextOptions {
                    color: silver,
                    size: font_size,
                    align: Alignment::new().center().middle(),
                    ..Default::default()
                },
            );
        }
        'ticks: for m in 1..61 {
            if m % 5 == 0 {
                continue 'ticks;
            }
            let m = f64::from(m) as f32;
            let ticks_radius = dial_radius * 0.925;
            let tick_len = 3.0;
            let tick_width = 1.0;
            dc.frame.path(
                |path| {
                    path.move_to((0.0, -ticks_radius));
                    path.line_to((0.0, -ticks_radius - tick_len));
                    path.close();
                    path.stroke(
                        white,
                        StrokeOptions {
                            width: tick_width,
                            ..Default::default()
                        },
                    );
                    path.fill(white, Default::default());
                },
                PathOptions {
                    composite_operation: CompositeOperation::Basic(
                        BasicCompositeOperation::Lighter,
                    ),
                    alpha: 1.0,
                    transform: Some(
                        Transform::new()
                            .translate(dial_center.0, dial_center.1)
                            .rotate(m * radians_per_sec),
                    ),
                    ..Default::default()
                },
            );
        }

        // time-string
        let show_time_string = false;
        if show_time_string {
            dc.frame.text(
                self.font,
                (dial_center.0, dial_center.1 + dial_radius * 0.7 - font_size),
                format!(
                    "{}:{:02}:{:02} {}",
                    hour,
                    minute,
                    second,
                    if am { "AM" } else { "PM" }
                ),
                TextOptions {
                    color: silver,
                    size: font_size,
                    align: Alignment::new().center().baseline(),
                    ..Default::default()
                },
            );
        }
        // date-string
        dc.frame.text(
            self.font,
            (dial_center.0, dial_center.1 + dial_radius * 0.7),
            format!("{:4}-{:02}-{:02}", year, month, day),
            TextOptions {
                color: silver,
                size: font_size,
                align: Alignment::new().center().baseline(),
                ..Default::default()
            },
        );

        //Draw the dial
        dc.frame.path(
            |path| {
                path.circle(origin, dial_radius);
                path.stroke(
                    silver,
                    StrokeOptions {
                        width: 3.0,
                        ..Default::default()
                    },
                );
                path.fill(dial_color, Default::default());
            },
            PathOptions {
                composite_operation: CompositeOperation::Basic(BasicCompositeOperation::Lighter),
                alpha: 1.0, //elapsed.cos() * 0.5 + 0.5,
                transform: Some(Transform::new().translate(dial_center.0, dial_center.1)),
                ..Default::default()
            },
        );

        let draw_hand = |theta: f32, length: f32, width: f32| {
            dc.frame.path(
                |path| {
                    path.move_to(origin);
                    path.line_to((0.0, -length));
                    path.close();
                    path.stroke(
                        white,
                        StrokeOptions {
                            width: width,
                            ..Default::default()
                        },
                    );
                    path.fill(white, Default::default());
                },
                PathOptions {
                    composite_operation: CompositeOperation::Basic(
                        BasicCompositeOperation::Lighter,
                    ),
                    alpha: 1.0,
                    transform: Some(
                        Transform::new()
                            .translate(dial_center.0, dial_center.1)
                            .rotate(theta),
                    ),
                    ..Default::default()
                },
            );
        };

        // draw the hands

        //let hour_angle = hour*radians_per_hour + minute*PI/360.0;
        let hour_angle = (((hour * 60.0 + minute) / 60.0) / 12.0) * two_pi;
        let minute_angle = minute * radians_per_sec;
        let second_angle = second * radians_per_sec;

        draw_hand(second_angle, second_hand_len, 1.0);
        draw_hand(minute_angle, minute_hand_len, 3.0);
        draw_hand(hour_angle, hour_hand_len, 5.0);

        //Draw the boss
        dc.frame.path(
            |path| {
                let boss_rad = 6.0;
                path.circle(origin, boss_rad);
                path.stroke(
                    darkgray,
                    StrokeOptions {
                        width: 1.0,
                        ..Default::default()
                    },
                );
                path.fill(
                    Gradient::Radial {
                        center: origin,
                        inner_radius: 0.0,
                        outer_radius: boss_rad,
                        start_color: silver,
                        end_color: darksilver,
                    },
                    Default::default(),
                );
            },
            PathOptions {
                composite_operation: CompositeOperation::Basic(BasicCompositeOperation::SourceOver),
                alpha: 1.0,
                transform: Some(Transform::new().translate(dial_center.0, dial_center.1)),
                ..Default::default()
            },
        );
    }
}
