use chrono::prelude::*;
use nanovg::{
    Alignment, BasicCompositeOperation, Color, CompositeOperation, Context, Font, Frame, Gradient,
    PathOptions, StrokeOptions, TextOptions, Transform,
};

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

impl<'a> Frontend<'a> {
    pub fn new(ctx: &'a Context) -> Frontend<'a> {
        Frontend {
            prev_second: -1.0,
            board: Default::default(),
            font: nanovg::Font::from_memory(&ctx, "Roboto", FONT)
                .expect("Failed to load font 'Roboto-Regular.ttf'"),
        }
    }
    pub fn render(&mut self, dc: &DrawContext<'a>) {}
}
