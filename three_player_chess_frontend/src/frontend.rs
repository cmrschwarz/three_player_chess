use nalgebra::{ArrayStorage, OMatrix, OVector, Point2, Transform2};
use skia_safe::{
    gpu::{gl::FramebufferInfo, BackendRenderTarget, SurfaceOrigin},
    radians_to_degrees, Canvas, Color, Color4f, ColorType, Data, Document, Font, Image, Matrix,
    Paint, PaintStyle, Path, Point, Rect, Surface, Typeface,
};
use std::f32::consts::PI;
use std::ops::Mul;
use vecmath::{vec2_add, vec2_len, vec2_normalized, vec2_scale, vec2_sub, Vector2};

use three_player_chess::board;
use three_player_chess::board::*;
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

pub struct Frontend {
    font: Font,
    piece_images: [[Image; PIECE_COUNT]; HB_COUNT],
    board: ThreePlayerChess,
    black: Color,
    white: Color,
    background: Color,
    border: Color,
    prev_second: f32,
}
fn point(vec2: Vector2<f32>) -> (f32, f32) {
    (vec2[0], vec2[1])
}

lazy_static! {
    static ref HEX_BOARD_COORDS: [[[f32; 2]; HB_ROW_COUNT + 1]; HB_ROW_COUNT + 1] = {
        let mut hbc: [[[f32; 2]; HB_ROW_COUNT + 1]; HB_ROW_COUNT + 1] = Default::default();
        let center_angle_half = 2. * PI / 12.;
        let side_len = center_angle_half.sin();
        let hex_height = center_angle_half.cos();
        let top_right = [side_len + (1. - side_len) / 2., hex_height / 2.];
        let bottom_right = [side_len, hex_height];
        let right_flank = vec2_sub(top_right, bottom_right);
        for r in 0..HB_ROW_COUNT + 1 {
            let rank_frac = r as f32 / 4.;
            // rank along the center axis
            hbc[0][r] = [0., hex_height * (1. - rank_frac)];
            // rank along the right side
            hbc[HB_ROW_COUNT][r] =
                vec2_add(bottom_right, vec2_scale(right_flank, rank_frac));
        }
        for r in 0..HB_ROW_COUNT + 1 {
            for f in 1..HB_ROW_COUNT {
                let rank_left = hbc[0][r];
                let rank_right = hbc[HB_ROW_COUNT][r];
                hbc[f][r] = vec2_add(
                    rank_left,
                    vec2_scale(vec2_sub(rank_right, rank_left), f as f32 / 4.),
                );
            }
        }
        hbc
    };
    static ref PIECE_TRANSFORMS: [[Matrix; HB_ROW_COUNT]; HB_ROW_COUNT] = {
        let mut ptf: [[Matrix; HB_ROW_COUNT]; HB_ROW_COUNT] = Default::default();
        let hbc = &HEX_BOARD_COORDS;
        for file in 0..HB_ROW_COUNT {
            for rank in 0..HB_ROW_COUNT {
                let bl = hbc[file][rank];
                let br = hbc[file + 1][rank];
                let tr = hbc[file + 1][rank + 1];
                let tl = hbc[file][rank + 1];
                let t = getTransform([[0., 0.], [1., 0.], [1., 1.], [0., 1.]], [tl, tr, br, bl]);
                let mat = Matrix::new_all(
                    t[0][0], t[0][1], t[0][2], t[1][0], t[1][1], t[1][2], t[2][0], t[2][1], t[2][2],
                );
                ptf[file][rank] = mat;
            }
        }
        ptf
    };
}

fn getTransform(from: [[f32; 2]; 4], to: [[f32; 2]; 4]) -> [[f32; 3]; 3] {
    let mut a = [[0f32; 8]; 8]; // 8x8
    let mut ai = 0;
    for i in 0..4 {
        a[ai] = [
            from[i][0],
            from[i][1],
            1.,
            0.,
            0.,
            0.,
            -from[i][0] * to[i][0],
            -from[i][1] * to[i][0],
        ];
        a[ai + 1] = [
            0.,
            0.,
            0.,
            from[i][0],
            from[i][1],
            1.,
            -from[i][0] * to[i][1],
            -from[i][1] * to[i][1],
        ];
        ai += 2;
    }
    let mut b = [0f32; 8]; // 8x1
    let mut bi = 0;
    for i in 0..4 {
        b[bi] = to[i][0];
        b[bi + 1] = to[i][1];
        bi += 2;
    }
    let a_mat = OMatrix::from_array_storage(ArrayStorage(a)).transpose();
    let b_mat = OVector::from_array_storage(ArrayStorage([b]));

    let h = a_mat.lu().solve(&b_mat).unwrap();

    let h_res = [[h[0], h[1], h[2]], [h[3], h[4], h[5]], [h[6], h[7], 1.]];

    // Sanity check that H actually maps `from` to `to`
    let h_mat = Transform2::from_matrix_unchecked(
        OMatrix::from_array_storage(ArrayStorage(h_res)).transpose(),
    );
    for i in 0..4 {
        let res = h_mat.transform_point(&Point2::new(from[i][0], from[i][1]));
        assert!(
            vec2_len(vec2_sub(res.into(), to[i])) < 1e-5,
            "transform creation failed"
        )
    }

    h_res
}
impl Frontend {
    pub fn new() -> Frontend {
        let images = PIECES.map(|pieces_for_color| {
            pieces_for_color
                .map(|piece_data| Image::from_encoded(Data::new_copy(&piece_data)).unwrap())
        });
        let mut fe = Frontend {
            prev_second: -1.0,
            board: ThreePlayerChess::from_str("BCEFGH2A5E4D5H4C5/BG1/CF1/AH1/D1/E1/AH/:LKIDCBA7J6/KB8/JC8/LA8/L5/D8/LA/:HGFEILbJaK9/GKc/FJc/HLc/Ec/Ic/HL/Ka:7:7").unwrap(),//Default::default(),
            font:  Font::from_typeface(Typeface::from_data(Data::new_copy(&FONT), None).expect("Failed to load font 'Roboto-Regular.ttf'"), None),
            black: Color::from_rgb(230, 230, 230),
            white: Color::from_rgb(130, 130, 130),
            background: Color::from_rgb(142, 83, 46),
            border: Color::from_rgb(0, 0, 0),
            piece_images: images
        };

        fe
    }
    pub fn render(&mut self, canvas: &mut Canvas) {
        let dim = canvas.image_info().dimensions();
        let radius_board = std::cmp::min(dim.width, dim.height) as f32 * 0.45;
        let radius_border = radius_board * 1.05;
        canvas.clear(self.background);
        canvas.translate(Point {
            x: dim.width as f32 / 2.,
            y: dim.height as f32 / 2.,
        });
        canvas.save();
        canvas.scale((radius_border, radius_border));
        let mut path = Path::new();

        let center_angle = 2. * PI / 6.;
        for i in 0..6 {
            let point = Point::new(
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
        let mut paint = Paint::new(&Color4f::from(self.border), None);
        paint.set_style(PaintStyle::Fill);
        canvas.draw_path(&path, &paint);
        canvas.restore();
        for c in board::Color::iter() {
            for right in [true, false] {
                self.render_hex_board(canvas, radius_board, *c, right);
            }
        }
    }
    pub fn render_hex_board(
        &mut self,
        canvas: &mut Canvas,
        radius: f32,
        hb: board::Color,
        right: bool,
    ) {
        canvas.save();
        let rot = (1. / 3.) * 2.0 * PI;
        canvas.rotate(radians_to_degrees(rot * usize::from(hb) as f32), None);
        if !right {
            canvas.scale((-radius, radius));
        } else {
            canvas.scale((radius, radius));
        }
        for file in 0..HB_ROW_COUNT {
            for rank in 0..HB_ROW_COUNT {
                canvas.save();
                canvas.concat(&PIECE_TRANSFORMS[file][rank]);
                let color = if ((file % 2) + (rank % 2) + (right as usize)) % 2 == 0 {
                    self.black
                } else {
                    self.white
                };
                let mut paint = Paint::new(&Color4f::from(color), None);
                canvas.draw_rect(Rect::from_xywh(0., 0., 1., 1.), &paint);
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

                    let (mut sx, mut sy) = (1., 1.);
                    if !right {
                        sx = -1.;
                    }
                    if hb != color {
                        sy = -1.;
                    }
                    if sx < 0. || sy < 0. {
                        canvas.translate(Point { x: 0.5, y: 0.5 });
                        canvas.scale((sx, sy));
                        canvas.translate(Point { x: -0.5, y: -0.5 });
                    }

                    canvas.draw_image_rect(
                        img,
                        Some((
                            &Rect::from_xywh(0., 0., img.width() as f32, img.height() as f32),
                            skia_safe::canvas::SrcRectConstraint::Fast,
                        )),
                        Rect::from_xywh(0., 0., 1., 1.),
                        &Paint::default(),
                    );
                }
                canvas.restore();
            }
        }
        canvas.restore();
    }
}
