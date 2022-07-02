use nanovg as nv;
use nanovg::{Context, Font, Frame, PathOptions, Transform};
use std::f32::consts::PI;
use std::ops::Mul;
use vecmath::{vec2_add, vec2_scale, vec2_sub, Vector2};

use three_player_chess::board::*;

pub struct DrawContext<'a> {
    pub frame: Frame<'a>,
    pub width: u32,
    pub height: u32,
}

pub const FONT: &'static [u8] = include_bytes!("../res/Roboto-Regular.ttf");

pub const PIECES: [[&'static [u8]; PIECE_COUNT]; HB_COUNT] = [
    [
        include_bytes!("../res/pwp.png"),
        include_bytes!("../res/pwn.png"),
        include_bytes!("../res/pwb.png"),
        include_bytes!("../res/pwr.png"),
        include_bytes!("../res/pwq.png"),
        include_bytes!("../res/pwk.png"),
    ],
    [
        include_bytes!("../res/pbp.png"),
        include_bytes!("../res/pbn.png"),
        include_bytes!("../res/pbb.png"),
        include_bytes!("../res/pbr.png"),
        include_bytes!("../res/pbq.png"),
        include_bytes!("../res/pbk.png"),
    ],
    [
        include_bytes!("../res/pcp.png"),
        include_bytes!("../res/pcn.png"),
        include_bytes!("../res/pcb.png"),
        include_bytes!("../res/pcr.png"),
        include_bytes!("../res/pcq.png"),
        include_bytes!("../res/pck.png"),
    ],
];

pub struct Frontend<'a> {
    font: Font<'a>,
    piece_images: [[nv::Image<'a>; PIECE_COUNT]; HB_COUNT],
    board: ThreePlayerChess,
    black: nv::Color,
    white: nv::Color,
    background: nv::Color,
    border: nv::Color,
    prev_second: f32,
    hex_board_coordinates: [[[f32; 2]; HB_ROW_COUNT + 1]; HB_ROW_COUNT + 1],
}
const HBRC: f32 = HB_ROW_COUNT as f32;

fn point(vec2: Vector2<f32>) -> (f32, f32) {
    (vec2[0], vec2[1])
}

impl<'a> Frontend<'a> {
    pub fn new(ctx: &'a Context) -> Frontend<'a> {
        let images = PIECES.map(|pieces_for_color| {
            pieces_for_color
                .map(|piece_data| nv::Image::new(ctx).build_from_memory(piece_data).unwrap())
        });
        let mut fe = Frontend {
            prev_second: -1.0,
            board: ThreePlayerChess::from_str("BCEFGH2A5D4D5H4/BG1/CF1/AH1/D1/E1/AH/:LKIDCBA7J6/KB8/JC8/LA8/L5/D8/LA/:HGFEILbJaK9/GKc/FJc/HLc/Ec/Ic/HL/Ka:7:7").unwrap(),//Default::default(),
            font: nanovg::Font::from_memory(&ctx, "Roboto", FONT)
                .expect("Failed to load font 'Roboto-Regular.ttf'"),
            black: nv::Color::from_rgb(230, 230, 230),
            white: nv::Color::from_rgb(130, 130, 130),
            background: nv::Color::from_rgb(142, 83, 46),
            border: nv::Color::from_rgb(0, 0, 0),
            piece_images: images,
            hex_board_coordinates: Default::default(),
        };
        let center_angle_half = 2. * PI / 12.;
        let side_len = center_angle_half.sin();
        let hex_height = center_angle_half.cos();
        let top_right = [side_len + (1. - side_len) / 2., hex_height / 2.];
        let bottom_right = [side_len, hex_height];
        let right_flank = vec2_sub(top_right, bottom_right);
        for r in 0..HB_ROW_COUNT + 1 {
            let rank_frac = r as f32 / 4.;
            // rank along the center axis
            fe.hex_board_coordinates[0][r] = [0., hex_height * (1. - rank_frac)];
            // rank along the right side
            fe.hex_board_coordinates[HB_ROW_COUNT][r] =
                vec2_add(bottom_right, vec2_scale(right_flank, rank_frac));
        }
        for r in 0..HB_ROW_COUNT + 1 {
            for f in 1..HB_ROW_COUNT {
                let rank_left = fe.hex_board_coordinates[0][r];
                let rank_right = fe.hex_board_coordinates[HB_ROW_COUNT][r];
                fe.hex_board_coordinates[f][r] = vec2_add(
                    rank_left,
                    vec2_scale(vec2_sub(rank_right, rank_left), f as f32 / 4.),
                );
            }
        }
        fe
    }
    pub fn render(&mut self, dc: &DrawContext<'a>) {
        let radius_board = std::cmp::min(dc.width, dc.height) as f32 * 0.45;
        let radius_border = radius_board * 1.05;
        dc.frame.path(
            |path| {
                path.rect((0., 0.), (dc.width as f32, dc.height as f32));
                path.fill(self.background, Default::default());
            },
            Default::default(),
        );
        dc.frame.path(
            |path| {
                let center_angle = 2. * PI / 6.;
                for i in 0..6 {
                    let point = (
                        (i as f32 * center_angle).cos(),
                        (i as f32 * center_angle).sin(),
                    );
                    if i == 0 {
                        path.move_to(point);
                    } else {
                        path.line_to(point);
                    }
                }
                path.close();
                path.fill(self.border, Default::default());
            },
            PathOptions {
                transform: Some(
                    nv::Transform::new()
                        .translate(dc.width as f32 / 2., dc.height as f32 / 2.)
                        .scale(radius_border, radius_border),
                ),
                ..Default::default()
            },
        );
        self.render_hex_board(dc, radius_board, Color::C0, true);
        for c in Color::iter() {
            for right in [true, false] {
                self.render_hex_board(dc, radius_board, *c, right);
            }
        }
    }
    pub fn render_hex_board(&mut self, dc: &DrawContext, radius: f32, hb: Color, right: bool) {
        let rot = (1. / 3.) * 2.0 * PI;
        let transform = Transform::new()
            .translate(dc.width as f32 / 2., dc.height as f32 / 2.)
            .rotate(usize::from(hb) as f32 * rot)
            .scale(if !right { -radius } else { radius }, radius);
        let wtf = transform.transform_point((0.125, 0.83));
        for file in 0..HB_ROW_COUNT {
            for rank in 0..HB_ROW_COUNT {
                dc.frame.path(
                    |path| {
                        path.move_to(point(self.hex_board_coordinates[file][rank]));
                        path.line_to(point(self.hex_board_coordinates[file + 1][rank]));
                        path.line_to(point(self.hex_board_coordinates[file + 1][rank + 1]));
                        path.line_to(point(self.hex_board_coordinates[file][rank + 1]));
                        path.close();
                        path.fill(
                            if ((file % 2) + (rank % 2) + (right as usize)) % 2 == 0 {
                                self.black
                            } else {
                                self.white
                            },
                            Default::default(),
                        );
                    },
                    PathOptions {
                        transform: Some(transform),
                        ..Default::default()
                    },
                );
                let mut f = file as i8;
                let mut r = rank as i8;
                if right {
                    (f, r) = (HB_ROW_COUNT as i8 + f + 1, r + 1);
                } else {
                    (f, r) = (HB_ROW_COUNT as i8 - f, r + 1);
                }
                let loc = AnnotatedFieldLocation::from_file_and_rank(hb, hb, f, r);
                if let Some((color, piece_value)) =
                    *FieldValue::from(self.board.board[usize::from(loc.loc)])
                {
                    let img = &self.piece_images[usize::from(color)][usize::from(piece_value)];

                    let bl = self.hex_board_coordinates[file][rank];
                    let br = self.hex_board_coordinates[file + 1][rank];
                    let tr = self.hex_board_coordinates[file + 1][rank + 1];
                    let tl = self.hex_board_coordinates[file][rank + 1];

                    let mut field_tf = Transform::new();

                    field_tf = field_tf.translate(tl[0], tl[1]);
                    let width = ((br[0] - bl[0]) + (tr[0] - tl[0])) / 2.;
                    let height = ((bl[1] - tl[1]) + (br[1] - tr[1])) / 1.95;
                    let skew_x = ((br[0] - tr[0]) + bl[0] - tl[0]) / 2.;
                    let skew_y = ((br[1] - bl[1]) + tr[1] - tl[1]) / 2.;
                    field_tf = field_tf.skew_y(skew_y / height);
                    field_tf = field_tf.skew_x(skew_x / width);
                    field_tf = field_tf.scale(width, height);
                    if !right || color != hb {
                        field_tf = field_tf.translate(0.5, 0.5);
                        let (mut sx, mut sy) = (1., 1.);
                        if !right {
                            sx = -1.;
                        };
                        if color != hb {
                            sy = -1.;
                        }
                        field_tf = field_tf.scale(sx, sy);
                        field_tf = field_tf.translate(-0.5, -0.5);
                    }
                    // this is mat = transform * field_df. nanovg is weird
                    let mat = field_tf.mul(transform);
                    dc.frame.path(
                        |path| {
                            let origin = (0., 0.);
                            let size = (1., 1.);
                            path.rect(origin, size);
                            path.fill(
                                //nv::Color::from_rgb(255, 0, 0),
                                nv::ImagePattern {
                                    image: img,
                                    origin,
                                    size,
                                    angle: 0.,
                                    alpha: 1.,
                                },
                                Default::default(),
                            );
                        },
                        PathOptions {
                            transform: Some(mat),
                            ..Default::default()
                        },
                    );
                }
            }
        }
    }
}
