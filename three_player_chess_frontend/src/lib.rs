#[macro_use]
extern crate lazy_static;

use arrayvec::ArrayString;
use nalgebra::geometry::*;
use nalgebra::{ArrayStorage, Matrix3, OMatrix, OVector, Transform2, Vector2};
use skia_safe::{
    radians_to_degrees, Bitmap, Canvas, Color, ColorType, Data, Font, IPoint, Image, ImageInfo,
    Matrix, Paint, PaintStyle, Path, Point, Rect, Typeface,
};
use std::cmp::min;
use std::f32::consts::PI;
use std::ops::{Add, Sub};
use three_player_chess::board;
use three_player_chess::board::PieceType::*;
use three_player_chess::board::*;
use three_player_chess::movegen::*;
pub const FONT: &'static [u8] = include_bytes!("../res/Roboto-Regular.ttf");
const BORDER_WIDTH: f32 = 0.02;

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
    pub font: Font,

    pub black: Color,
    pub white: Color,
    pub show_notation: bool,
    pub selection_color: Color,
    pub move_hint_color: Color,
    pub danger: Color,
    pub danger_light: Color,
    pub background: Color,
    pub last_move_color: Color,
    pub move_before_last_color: Color,
    pub player_colors: [Color; HB_COUNT],
    pub prev_second: f32,
    pub transformed_pieces: bool,
    // we need to store the color of the player's turn before the move
    // to support illegal moves from other players
    pub history: Vec<(ReversableMove, three_player_chess::board::Color)>,
    pub board: ThreePlayerChess,
    pub selected_square: Option<FieldLocation>,
    pub hovered_square: Option<FieldLocation>,
    pub dragged_square: Option<FieldLocation>,
    pub move_info_square: Option<FieldLocation>,
    pub promotion_preview: Option<FieldLocation>,
    pub possible_moves: bitvec::BitArr!(for (32 * HB_COUNT), in u32), //  BitArray<bitvec::order::LocalBits, [u32; HB_COUNT]>,
    pub king_in_check: [bool; HB_COUNT],
    pub cursor_pos: Vector2<i32>,
    pub board_radius: f32,
    pub board_origin: Vector2<i32>,
    pub origin: board::Color,
    pub transform_dragged_pieces: bool,
    pub pieces: [[Image; PIECE_COUNT]; HB_COUNT],
    pub piece_scale_non_transformed: f32,
    pub highlight_attacked: bool,
    pub highlight_capturable: bool,
    pub allow_illegal_moves: bool,
    pub show_non_legal_move_hints: bool,
}
const PROMOTION_QUADRANTS: [(PieceType, f32, f32); 4] = [
    (Queen, 0., 0.),
    (Bishop, 0., 0.5),
    (Knight, 0.5, 0.),
    (Rook, 0.5, 0.5),
];
const HEX_CENTER_ANGLE: f32 = 2. * PI / 6.;
// assumes origin being top left, forms a U
const UNIT_SQUARE: [Vector2<f32>; 4] = [
    Vector2::new(0., 0.),
    Vector2::new(0., 1.),
    Vector2::new(1., 1.),
    Vector2::new(1., 0.),
];

fn recolor_images<const COUNT: usize>(
    images: &[Image; COUNT],
    source_color: Color,
    tgt_col: Color,
    epsilon: u32,
) -> [Image; COUNT] {
    let sr = source_color.r() as u32;
    let sg = source_color.g() as u32;
    let sb = source_color.b() as u32;
    let sa = source_color.a() as u32;
    let origin = IPoint::new(0, 0);
    let img_info = &images[0].image_info();
    let info = ImageInfo::new(
        img_info.dimensions(),
        ColorType::RGBA8888,
        img_info.alpha_type(),
        img_info.color_space(),
    );
    let mut ok;
    let mut bitmap = Bitmap::new();
    ok = bitmap.set_info(&info, None);
    assert!(ok);
    bitmap.alloc_pixels();

    let row_size = bitmap.row_bytes();
    let img_size = row_size * bitmap.height() as usize;
    let mut row_vec = Vec::with_capacity(img_size);
    row_vec.resize(img_size, 0u8);
    let mut pix_data = row_vec.into_boxed_slice();
    let mut out_images = arrayvec::ArrayVec::<Image, COUNT>::new();
    for img in images.iter() {
        ok = img.read_pixels(
            &info,
            &mut pix_data,
            row_size,
            origin,
            skia_safe::image::CachingHint::Disallow,
        );
        assert!(ok);

        let mut i = 0;
        let p = &mut pix_data;
        for _ in 0..p.len() / 4 {
            let diff = sr.abs_diff(p[i] as u32)
                + sg.abs_diff(p[i + 1] as u32)
                + sb.abs_diff(p[i + 2] as u32)
                + sa.abs_diff(p[i + 3] as u32);
            if diff <= epsilon {
                p[i + 0] = tgt_col.r();
                p[i + 1] = tgt_col.g();
                p[i + 2] = tgt_col.b();
                p[i + 3] = tgt_col.a();
            }
            i += 4;
        }
        out_images
            .push(Image::from_raster_data(&info, Data::new_copy(&pix_data), row_size).unwrap());
    }
    out_images.into_inner().unwrap()
}

fn sk_paint_img() -> Paint {
    let mut paint = Paint::default();
    paint.set_anti_alias(true);
    paint
}

fn sk_sampling_opts() -> skia_safe::SamplingOptions {
    let mut so = skia_safe::SamplingOptions::new(
        skia_safe::FilterMode::Linear,
        skia_safe::MipmapMode::Linear,
    );
    so.use_cubic = false;
    so
}

fn sk_paint(color: Color, style: PaintStyle) -> Paint {
    let mut paint = Paint::default();
    paint.set_anti_alias(true);
    paint.set_color(color);
    paint.set_style(style);
    paint
}

lazy_static! {
    static ref UNIT_RECT: Rect = Rect::from_xywh(0., 0., 1., 1.);
    static ref HEX_SIDE_LEN: f32 = (HEX_CENTER_ANGLE / 2.).sin();
    static ref HEX_HEIGHT: f32 =  (HEX_CENTER_ANGLE / 2.).cos();
    static ref PIECE_IMAGES: [[Image; PIECE_COUNT]; HB_COUNT] = {
        PIECES.map(|pieces_for_color| {
            pieces_for_color
                .map(|piece_data| Image::from_encoded(Data::new_copy(&piece_data)).unwrap())
        })
    };
    static ref ORIGINAL_PIECE_COLORS: [Color; HB_COUNT] = [Color::from_rgb(255, 255, 255), Color::from_rgb(0, 0, 0), Color::from_rgb(7, 16, 132)];
    static ref HEX_BOARD_COORDS: [[Vector2<f32>; HB_ROW_COUNT + 1]; HB_ROW_COUNT + 1] = {
        let hex_side_len: f32 = *HEX_SIDE_LEN;
        let hex_height: f32 = *HEX_HEIGHT;
        let mut hbc: [[Vector2<f32>; HB_ROW_COUNT + 1]; HB_ROW_COUNT + 1] = Default::default();
        let top_right = Vector2::new(hex_side_len + (1. - hex_side_len) / 2., hex_height / 2.);
        let bottom_right = Vector2::new(hex_side_len, hex_height);
        let right_flank = top_right.sub(bottom_right);
        for r in 0..HB_ROW_COUNT + 1 {
            let rank_frac = r as f32 / 4.;
            // rank along the center axis
            hbc[0][r] = Vector2::new(0., hex_height * (1. - rank_frac));
            // rank along the right side
            hbc[HB_ROW_COUNT][r] = bottom_right.add(right_flank.scale( rank_frac));
        }
        for r in 0..HB_ROW_COUNT + 1 {
            for f in 1..HB_ROW_COUNT {
                let rank_left = hbc[0][r];
                let rank_right = hbc[HB_ROW_COUNT][r];
                hbc[f][r] = rank_left.add(
                    rank_right.sub(rank_left).scale(f as f32 / 4.)
                );
            }
        }
        hbc
    };
    static ref CELL_TRANSFORMS: [[Matrix; HB_ROW_COUNT]; HB_ROW_COUNT] = {
        let mut ptf: [[Matrix; HB_ROW_COUNT]; HB_ROW_COUNT] = Default::default();
        for file in 0..HB_ROW_COUNT {
            for rank in 0..HB_ROW_COUNT {
                let t = create_rect_transform(UNIT_SQUARE, get_cell_quad(file, rank)).unwrap();
                let mat = Matrix::new_all(
                    t.m11, t.m12, t.m13, t.m21, t.m22, t.m23, t.m31, t.m32, t.m33
                );
                ptf[file][rank] = mat;
            }
        }
        ptf
    };
}

fn get_cell_quad(file: usize, rank: usize) -> [Vector2<f32>; 4] {
    let hbc = &*HEX_BOARD_COORDS;
    let tl = hbc[file][rank + 1];
    let bl = hbc[file][rank];
    let br = hbc[file + 1][rank];
    let tr = hbc[file + 1][rank + 1];
    [tl, bl, br, tr]
}

fn create_rect_transform(from: [Vector2<f32>; 4], to: [Vector2<f32>; 4]) -> Option<Matrix3<f32>> {
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

    // check if h actually works (maps the points in 'from' to 'to')
    // this might not be the case for concave rects
    let h_mat = OMatrix::from_array_storage(ArrayStorage(h_res)).transpose();

    let h_trans = Transform2::from_matrix_unchecked(h_mat);
    for i in 0..4 {
        let res = h_trans.transform_point(&from[i].into());
        let err = res.coords.sub(to[i]).norm();
        // times 10 because the errors can be quite large here
        if err > f32::EPSILON * 10. {
            return None;
        }
    }
    Some(h_mat)
}

fn sign(p1: Vector2<f32>, p2: Vector2<f32>, p3: Vector2<f32>) -> f32 {
    return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y);
}

fn point_in_triangle(point: Vector2<f32>, triangle: &[Vector2<f32>; 3]) -> bool {
    let d1 = sign(point, triangle[0], triangle[1]);
    let d2 = sign(point, triangle[1], triangle[2]);
    let d3 = sign(point, triangle[2], triangle[0]);

    let has_neg = (d1 < 0.) || (d2 < 0.) || (d3 < 0.);
    let has_pos = (d1 > 0.) || (d2 > 0.) || (d3 > 0.);

    return !(has_neg && has_pos);
}

fn point_in_quad(point: Vector2<f32>, quad: &[Vector2<f32>; 4]) -> bool {
    let tri1 = &quad[0..3].try_into().unwrap();
    let tri2 = &[quad[2], quad[3], quad[0]].try_into().unwrap();
    point_in_triangle(point, tri1) || point_in_triangle(point, tri2)
}

fn center_of_quad(quad: &[Vector2<f32>; 4]) -> Vector2<f32> {
    let diag_half_1 = quad[0].add(quad[2].sub(quad[0]).scale(0.5));
    let diag_half_2 = quad[1].add(quad[3].sub(quad[1]).scale(0.5));
    diag_half_1.add(diag_half_2.sub(diag_half_1).scale(0.5))
}

// returns the center and half the width
fn square_in_quad(quad: &[Vector2<f32>; 4]) -> (Vector2<f32>, f32) {
    let center = center_of_quad(quad);
    let diags = [Vector2::new(1 as f32, 1.), Vector2::new(1 as f32, -1.)];
    let mut t_min = f32::MAX;
    for d in 0..diags.len() {
        let diag = diags[d];
        let m_diag = diag.y / diag.x;
        for i in 0..quad.len() {
            let p1 = quad[i];
            let p2 = quad[(i + 1) % 4];
            let flank = p2.sub(p1);
            let t = if flank.x.abs() < f32::EPSILON {
                p1.x - center.x // flank is parallel to y axis
            } else {
                let m_flank = flank.y / flank.x;

                if (m_diag - m_flank).abs() < f32::EPSILON {
                    continue; // flank and are parallel
                }
                ((p1.y + ((center.x - p1.x) * m_flank)) - center.y) / (m_diag - m_flank)
            };
            t_min = t_min.min(t.abs());
        }
    }
    (center, t_min)
}

pub fn get_quadrant_from_unit_box_pos(pos: Vector2<f32>, flipped: bool) -> usize {
    if (pos.x < 0.5) != flipped {
        if (pos.y < 0.5) != flipped {
            0
        } else {
            1
        }
    } else {
        if (pos.y < 0.5) != flipped {
            2
        } else {
            3
        }
    }
}

impl Frontend {
    pub fn new() -> Frontend {
        Frontend {
            prev_second: -1.0,
            board: ThreePlayerChess::default(),
            font: Font::from_typeface(
                Typeface::from_data(Data::new_copy(&FONT), None)
                    .expect("Failed to load font 'Roboto-Regular.ttf'"),
                None,
            ),
            black: Color::from_rgb(161, 119, 67),
            white: Color::from_rgb(240, 217, 181),
            selection_color: Color::from_argb(128, 56, 173, 105),
            move_hint_color: Color::from_argb(128, 0, 163, 225),
            last_move_color: Color::from_argb(180, 75, 104, 198),
            move_before_last_color: Color::from_argb(150, 153, 186, 241),
            background: Color::from_rgb(201, 144, 73),
            danger: Color::from_rgb(232, 15, 13),
            danger_light: Color::from_rgb(255, 114, 118),
            transformed_pieces: false,
            selected_square: None,
            history: Default::default(),
            cursor_pos: Vector2::new(-1, -1),
            board_radius: 1.,
            board_origin: Vector2::new(1, 1),
            origin: board::Color::default(),
            possible_moves: Default::default(),
            dragged_square: None,
            promotion_preview: None,
            hovered_square: None,
            move_info_square: None,
            player_colors: [
                Color::from_rgb(236, 236, 236),
                Color::from_rgb(41, 41, 41),
                Color::from_rgb(36, 36, 128),
            ],
            king_in_check: [false; HB_COUNT],
            pieces: PIECE_IMAGES.clone(),
            transform_dragged_pieces: false,
            piece_scale_non_transformed: 1.2,
            highlight_attacked: false,
            highlight_capturable: false,
            show_notation: true,
            allow_illegal_moves: false,
            show_non_legal_move_hints: false,
        }
    }
    fn get_hb_id(&self, color: board::Color) -> usize {
        (3 + usize::from(color) - usize::from(self.origin)) % 3
    }
    fn get_cell_quad_rotated(
        &self,
        color: board::Color,
        right: bool,
        file: usize,
        rank: usize,
    ) -> [Vector2<f32>; 4] {
        let hb_id = self.get_hb_id(color);
        let mut quad = get_cell_quad(file, rank);
        let rot = Rotation2::new(hb_id as f32 * 2. * HEX_CENTER_ANGLE);
        for p in quad.iter_mut() {
            if !right {
                p.x = -p.x;
            }
            *p = rot.transform_vector(p);
        }
        // make sure that the points are still in a U shape with
        // top left coming first
        quad.rotate_left(hb_id);
        if !right {
            quad.swap(0, 3);
            quad.swap(1, 2);
        }
        quad
    }
    pub fn render_background(&mut self, canvas: &mut Canvas) {
        canvas.clear(self.background);
        //TODO: config option for this
        let mut path = Path::new();
        for i in 0..HB_COUNT * 2 {
            let point = Point::new(
                (i as f32 * HEX_CENTER_ANGLE).cos(),
                (i as f32 * HEX_CENTER_ANGLE).sin(),
            );
            if i == 0 {
                path.move_to(point);
            } else {
                path.line_to(point);
            }
        }
        path.close();

        let border_scale = 1. + BORDER_WIDTH / 2.;
        let mut paint = sk_paint(
            self.player_colors[usize::from(self.board.turn)],
            PaintStyle::Stroke,
        );
        paint.set_stroke_width(BORDER_WIDTH);
        canvas.save();
        canvas.scale((border_scale, border_scale));

        if self.board.game_status == GameStatus::Ongoing {
            canvas.draw_path(&path, &paint);
        } else if let GameStatus::Win(winner, _) = self.board.game_status {
            paint.set_color(self.player_colors[usize::from(winner)]);
            canvas.draw_path(&path, &paint);
            let border_2_scale = border_scale + BORDER_WIDTH;
            canvas.scale((border_2_scale, border_2_scale));
            paint.set_stroke_width(BORDER_WIDTH / border_2_scale);
            canvas.draw_path(&path, &paint);
        }
        canvas.restore();
    }
    pub fn render_notation(&mut self, canvas: &mut Canvas) {
        if !self.show_notation {
            return;
        }
        let hex_height = *HEX_HEIGHT;
        let hex_side_len = *HEX_SIDE_LEN;
        let notation_paint = {
            let color = if let GameStatus::Win(winner, _) = self.board.game_status {
                self.player_colors[usize::from(winner)]
            } else {
                self.player_colors[usize::from(self.board.turn)]
            };
            &sk_paint(color, PaintStyle::Fill)
        };
        let tgt_font_size = 0.050;
        let mut notation_offset = 0.025;
        if self.board.game_status != GameStatus::Ongoing {
            notation_offset += BORDER_WIDTH;
        }
        for rank in [false, true] {
            for c in 0..HB_COUNT {
                let c_logical = (c + usize::from(self.origin)) % HB_COUNT;
                let angle = radians_to_degrees(
                    HEX_CENTER_ANGLE * 2. * c as f32 + usize::from(rank) as f32 * -HEX_CENTER_ANGLE,
                );
                let xshift = hex_side_len / 4.;
                let y = hex_height + notation_offset;
                let mut x = -hex_side_len + hex_side_len / (HBRC as f32 * 2.);
                for i in 1..ROW_SIZE + 1 {
                    let field_loc = if rank {
                        if i <= HB_ROW_COUNT {
                            FieldLocation::new(board::Color::from_u8(c_logical as u8), RS, i as i8)
                        } else {
                            FieldLocation::new(
                                board::Color::from_u8(((c_logical + 2) % HB_COUNT) as u8),
                                1,
                                (ROW_SIZE - i + 1) as i8,
                            )
                        }
                    } else {
                        FieldLocation::new(board::Color::from_u8(c_logical as u8), i as i8, 1)
                    };
                    let notation = if !rank {
                        let mut res = ArrayString::new();
                        res.push(field_loc.file_char_fancy() as char);
                        res
                    } else {
                        field_loc.rank_char_fancy()
                    };
                    let text = skia_safe::TextBlob::new(notation.as_str(), &self.font).unwrap();

                    let font_scale = tgt_font_size / self.font.size();
                    canvas.save();
                    canvas.rotate(angle, None);
                    canvas.translate(Point::new(x, y + text.bounds().height() * font_scale / 2.));
                    canvas.rotate(-angle, None);
                    canvas.scale((font_scale, font_scale));
                    canvas.draw_text_blob(
                        &text,
                        Point::new(-text.bounds().center_x(), -text.bounds().center_y()),
                        notation_paint,
                    );
                    canvas.restore();
                    x += xshift;
                }
            }
        }
    }
    pub fn render_dragged_piece(&mut self, canvas: &mut Canvas) {
        if let Some(field) = self.dragged_square {
            canvas.save();
            if let Some((color, piece_type)) = *self.board.get_field_value(field) {
                let afl = AnnotatedFieldLocation::from(field);
                let right = afl.file > 4;
                let file_phys = if !right {
                    HB_ROW_COUNT - afl.file as usize
                } else {
                    afl.file as usize - HB_ROW_COUNT - 1
                };
                let rank_phys = afl.rank as usize - 1;
                let pos = self
                    .cursor_pos
                    .sub(self.board_origin)
                    .cast::<f32>()
                    .scale(1. / self.board_radius);
                if self.transform_dragged_pieces {
                    canvas.translate(Point::new(pos.x, pos.y));
                    let scale = (*HEX_SIDE_LEN + *HEX_HEIGHT) / 2. / HB_ROW_COUNT as f32;
                    canvas.scale((scale, scale));
                    canvas.translate(Point::new(-0.5, -0.5));
                } else if self.transformed_pieces {
                    let rotation = self.get_hb_id(afl.hb) as f32 * HEX_CENTER_ANGLE * 2.;
                    let mut mat = Matrix::new_identity();
                    mat.pre_rotate(radians_to_degrees(rotation), None);
                    if !right {
                        mat.pre_scale((-1., 1.), None);
                    }
                    mat.pre_concat(&CELL_TRANSFORMS[file_phys][rank_phys]);
                    mat.pre_translate(Point::new(0.5, 0.5));
                    if !right {
                        mat.pre_scale((-1., 1.), None);
                    }
                    if color != afl.hb {
                        mat.pre_scale((1., -1.), None);
                    }
                    mat.pre_translate(Point::new(-0.5, -0.5));

                    let center_trans = mat.map_point(Point::new(0.5, 0.5));
                    canvas.translate(Point::new(pos.x - center_trans.x, pos.y - center_trans.y));
                    canvas.concat(&mat);
                } else {
                    canvas.translate(Point::new(pos.x, pos.y));
                    let (_, mut size_h) = square_in_quad(
                        &self.get_cell_quad_rotated(afl.hb, right, file_phys, rank_phys),
                    );
                    size_h *= self.piece_scale_non_transformed;
                    canvas.scale((size_h * 2., size_h * 2.));
                    canvas.translate(Point::new(-0.5, -0.5));
                };
                let img = &self.pieces[usize::from(color)][usize::from(piece_type)];
                canvas.draw_image_rect_with_sampling_options(
                    img,
                    Some((
                        &Rect::from_xywh(0., 0., img.width() as f32, img.height() as f32),
                        skia_safe::canvas::SrcRectConstraint::Fast,
                    )),
                    &*UNIT_RECT,
                    sk_sampling_opts(),
                    &sk_paint_img(),
                );
            } else {
                // can happen if the board was reloaded etc.
                self.dragged_square = None;
            }
            canvas.restore();
        }
    }
    pub fn render_board(&mut self, canvas: &mut Canvas) {
        for c in board::Color::iter() {
            for right in [true, false] {
                self.render_hexboard(canvas, *c, right);
            }
        }
    }
    pub fn update_dimensions(&mut self, x: i32, y: i32, width: i32, height: i32) {
        self.board_origin = Vector2::new(x + width / 2, y + height / 2);
        self.board_radius = std::cmp::min(width, height) as f32 * 0.46;
    }
    pub fn render(&mut self, canvas: &mut Canvas) {
        let translation = self.board_origin.cast::<f32>();

        canvas.save();
        canvas.translate(Point {
            x: translation.x,
            y: translation.y,
        });
        canvas.scale((self.board_radius, self.board_radius));

        self.render_background(canvas);
        self.render_notation(canvas);
        self.render_board(canvas);
        self.render_dragged_piece(canvas);

        canvas.restore();
    }
    pub fn transform_to_cell(
        &self,
        canvas: &mut Canvas,
        hb: board::Color,
        right: bool,
        file: usize,
        rank: usize,
    ) {
        let (center, size_h) = square_in_quad(&self.get_cell_quad_rotated(hb, right, file, rank));
        canvas.translate(Point::new(center.x, center.y));
        canvas.scale((
            2. * size_h * self.piece_scale_non_transformed,
            2. * size_h * self.piece_scale_non_transformed,
        ));
        canvas.translate((-0.5, -0.5));
    }
    pub fn transform_to_cell_nonaffine(
        &self,
        canvas: &mut Canvas,
        hb: board::Color,
        right: bool,
        file: usize,
        rank: usize,
        flip_x: bool,
        flip_y: bool,
    ) {
        canvas.rotate(
            radians_to_degrees(self.get_hb_id(hb) as f32 * 2. * HEX_CENTER_ANGLE),
            None,
        );
        if !right {
            canvas.scale((-1., 1.));
        }
        canvas.concat(&CELL_TRANSFORMS[file][rank]);
        let (mut sx, mut sy) = (1., 1.);
        if !right != flip_x {
            sx = -1.;
        }
        if flip_y {
            sy = -1.;
        }
        if sx < 0. || sy < 0. {
            canvas.translate(Point::new(0.5, 0.5));
            canvas.scale((sx, sy));
            canvas.translate(Point::new(-0.5, -0.5));
        }
    }
    pub fn draw_cell(
        &mut self,
        canvas: &mut Canvas,
        hb: board::Color,
        right: bool,
        file: usize,
        rank: usize,
    ) {
        let mut file_rot = file as i8;
        let mut rank_rot = rank as i8;

        if right {
            (file_rot, rank_rot) = (HB_ROW_COUNT as i8 + file_rot + 1, rank_rot + 1);
        } else {
            (file_rot, rank_rot) = (HB_ROW_COUNT as i8 - file_rot, rank_rot + 1);
        }
        let field = FieldLocation::new(hb, file_rot, rank_rot);

        let field_color = if ((file % 2) + (rank % 2) + (right as usize)) % 2 == 0 {
            self.white // bottom right must be white
        } else {
            self.black
        };

        let possible_move = self.possible_moves[usize::from(field)];
        let selection_paint = sk_paint(self.selection_color, PaintStyle::Fill);
        let move_hint_paint = sk_paint(self.move_hint_color, PaintStyle::Fill);
        let field_paint = sk_paint(field_color, PaintStyle::Fill);
        let field_val = self.board.get_field_value(field);
        let selected = Some(field) == self.hovered_square
            || Some(field) == self.selected_square
            || Some(field) == self.dragged_square;
        let selected_for_move_hint = Some(field) == self.move_info_square;

        let mut prev_action_col = None;
        for i in 0..min(2, self.history.len()) {
            let m = self.history[self.history.len() - i - 1].0.mov;
            if m.source == field || m.target == field {
                if i == 0 {
                    prev_action_col = Some(self.last_move_color);
                } else {
                    prev_action_col = Some(self.move_before_last_color);
                }
                break;
            }
        }

        let promotion = Some(field) == self.promotion_preview;
        let flip_pieces = (field_val.is_some() && field_val.color() != Some(hb)) || promotion;

        canvas.save();
        self.transform_to_cell_nonaffine(
            canvas,
            hb,
            right,
            file,
            rank,
            promotion || flip_pieces,
            flip_pieces,
        );
        canvas.draw_rect(&*UNIT_RECT, &field_paint);

        if promotion {
            let cursor_quadrant = self
                .get_screen_pos_in_field(self.cursor_pos, field)
                .map(|pos| get_quadrant_from_unit_box_pos(pos, self.transformed_pieces));
            if !self.transformed_pieces {
                canvas.restore();
                canvas.save();
                self.transform_to_cell(canvas, hb, right, file, rank);
            }
            for (quadrant, &(piece, x, y)) in PROMOTION_QUADRANTS.iter().enumerate() {
                let color =
                    FieldValue::from(self.board.board[usize::from(self.selected_square.unwrap())])
                        .color()
                        .unwrap();
                let img = &self.pieces[usize::from(color)][usize::from(piece)];

                let rect = Rect::from_xywh(x, y, 0.5, 0.5);
                if Some(quadrant) == cursor_quadrant {
                    canvas.draw_rect(rect, &selection_paint);
                }

                canvas.draw_image_rect_with_sampling_options(
                    img,
                    Some((
                        &Rect::from_xywh(0., 0., img.width() as f32, img.height() as f32),
                        skia_safe::canvas::SrcRectConstraint::Fast,
                    )),
                    rect,
                    sk_sampling_opts(),
                    &sk_paint_img(),
                );
            }
        } else if let Some((color, piece_value)) = *self.board.get_field_value(field) {
            let king_check = self.king_in_check[usize::from(color)] && piece_value == King;
            let capturable = self.highlight_capturable
                && field_val.color().is_some()
                && field_val.color() != Some(self.board.turn)
                && self
                    .board
                    .is_piece_capturable_at(
                        field,
                        Some(self.board.turn),
                        Some(
                            &self.board.moves_for_board[usize::from(
                                self.board.king_positions[usize::from(self.board.turn)],
                            )],
                        ),
                    )
                    .is_some();
            let img = &self.pieces[usize::from(color)][usize::from(piece_value)];

            if selected {
                canvas.draw_rect(&*UNIT_RECT, &selection_paint);
            } else if selected_for_move_hint {
                canvas.draw_rect(&*UNIT_RECT, &move_hint_paint);
            } else if self.highlight_attacked
                && field_val.color() == Some(self.board.turn)
                && self
                    .board
                    .is_piece_capturable_at(field, None, None)
                    .is_some()
            {
                canvas.draw_rect(&*UNIT_RECT, &sk_paint(self.danger_light, PaintStyle::Fill));
            } else if let Some(col) = prev_action_col {
                canvas.draw_rect(&*UNIT_RECT, &sk_paint(col, PaintStyle::Fill));
            }

            if (!selected && (possible_move || capturable)) || king_check {
                let size = 0.35;
                let mut path = Path::new();
                for p in UNIT_SQUARE {
                    path.move_to(Point::new(p.x, p.y));
                    path.line_to(Point::new(p.x, if p.y > 0. { p.y - size } else { size }));
                    path.line_to(Point::new(if p.x > 0. { p.x - size } else { size }, p.y));
                    path.line_to(Point::new(p.x, p.y));
                }
                let color = if king_check {
                    if self.board.turn == color {
                        self.danger
                    } else {
                        self.danger_light
                    }
                } else if possible_move {
                    if self.move_info_square.is_some() {
                        self.move_hint_color
                    } else {
                        self.selection_color
                    }
                } else {
                    // -> capturable
                    self.danger_light
                };
                canvas.draw_path(&path, &sk_paint(color, PaintStyle::Fill));
            }

            if !self.transformed_pieces {
                canvas.restore();
                canvas.save();
                self.transform_to_cell(canvas, hb, right, file, rank);
            }

            canvas.draw_image_rect_with_sampling_options(
                img,
                Some((
                    &Rect::from_xywh(0., 0., img.width() as f32, img.height() as f32),
                    skia_safe::canvas::SrcRectConstraint::Fast,
                )),
                &*UNIT_RECT,
                sk_sampling_opts(),
                &sk_paint_img(),
            );

            if self.transformed_pieces {}
        } else {
            if selected {
                canvas.draw_rect(&*UNIT_RECT, &selection_paint);
            }
            if let Some(col) = prev_action_col {
                canvas.draw_rect(&*UNIT_RECT, &sk_paint(col, PaintStyle::Fill));
            }
            if possible_move {
                let point_radius = 0.2;
                if self.transformed_pieces {
                    canvas.draw_circle(Point::new(0.5, 0.5), point_radius, &selection_paint);
                } else {
                    canvas.restore();
                    canvas.save();
                    self.transform_to_cell(canvas, hb, right, file, rank);
                    let paint = if self.move_info_square.is_some() {
                        &move_hint_paint
                    } else {
                        &selection_paint
                    };
                    canvas.draw_circle(Point::new(0.5, 0.5), point_radius, paint);
                }
            }
        }
        canvas.restore();
    }
    pub fn render_hexboard(&mut self, canvas: &mut Canvas, hb: board::Color, right: bool) {
        for file in 0..HB_ROW_COUNT {
            for rank in 0..HB_ROW_COUNT {
                self.draw_cell(canvas, hb, right, file, rank);
            }
        }
    }
    pub fn get_hexboard_from_screen_point(&self, screen_pos: Vector2<i32>) -> u8 {
        let pos_rel = screen_pos.sub(self.board_origin).cast::<f32>();
        let rot = ((-pos_rel.y).atan2(pos_rel.x) + 2.5 * PI) % (2. * PI);
        ((HB_COUNT * 2 - (rot / HEX_CENTER_ANGLE) as usize) % 6) as u8
    }
    pub fn is_screen_pos_in_bounds(&self, screen_pos: Vector2<i32>) -> bool {
        let pos_rel = screen_pos.sub(self.board_origin).cast::<f32>();
        pos_rel.norm_squared() < self.board_radius * self.board_radius
    }
    pub fn get_hexboard_logical(&self, phys_hexboard: u8) -> u8 {
        (phys_hexboard + u8::from(self.origin) * 2) % 6
    }
    // returns (halfboard, right, position transformed and scaled to the lower right halfboard)
    pub fn transform_screen_point_to_hb0(
        &self,
        screen_pos: Vector2<i32>,
    ) -> Option<(board::Color, bool, Vector2<f32>)> {
        if !self.is_screen_pos_in_bounds(screen_pos) {
            return None;
        }
        let hexboard = self.get_hexboard_from_screen_point(screen_pos);
        let hb = board::Color::from_u8(self.get_hexboard_logical(hexboard) / 2);
        let right = hexboard % 2 == 0;
        let pos_rot = nalgebra::geometry::Rotation2::new(hexboard as f32 * -HEX_CENTER_ANGLE)
            .transform_vector(&screen_pos.sub(self.board_origin).cast());
        let pos_scaled = pos_rot.scale(1. / self.board_radius);
        Some((hb, right, pos_scaled))
    }
    pub fn get_board_pos_from_screen_pos(&self, screen_pos: Vector2<i32>) -> Option<FieldLocation> {
        let (hb, right, pos_scaled) = self.transform_screen_point_to_hb0(screen_pos)?;
        for f in 0..HB_ROW_COUNT {
            for r in 0..HB_ROW_COUNT {
                if point_in_quad(pos_scaled.into(), &get_cell_quad(f, r)) {
                    let mut file = f as i8 + 1;
                    let mut rank = r as i8 + 1;
                    if right {
                        file += HBRC;
                    } else {
                        (file, rank) = (rank, HBRC - file + 1)
                    }
                    return Some(FieldLocation::new(hb, file, rank));
                }
            }
        }
        None
    }
    pub fn get_screen_pos_in_field(
        &self,
        screen_pos: Vector2<i32>,
        field: FieldLocation,
    ) -> Option<Vector2<f32>> {
        let afl = AnnotatedFieldLocation::from(field);
        if !self.is_screen_pos_in_bounds(screen_pos) {
            return None;
        }
        let (mut f, mut r) = (afl.file as usize - 1, afl.rank as usize - 1);
        let right = afl.file > HBRC;
        if right {
            f -= HB_ROW_COUNT;
        } else {
            (f, r) = (HB_ROW_COUNT - 1 - r, f)
        }
        let point = if !self.transformed_pieces {
            let pos_rel = screen_pos
                .sub(self.board_origin)
                .cast::<f32>()
                .scale(1. / self.board_radius);
            let hexboard = self.get_hexboard_from_screen_point(screen_pos);
            let hexboard_logical = self.get_hexboard_logical(hexboard);
            if board::Color::from_u8(hexboard_logical / 2) != field.hb()
                || (hexboard % 2 == 0) != field.is_right_side()
            {
                return None;
            }
            let (center, mut scale_h) = square_in_quad(&get_cell_quad(f, r));
            scale_h *= self.piece_scale_non_transformed;
            let center_trans =
                Rotation2::new(hexboard as f32 * HEX_CENTER_ANGLE).transform_vector(&center);
            let origin_trans = center_trans.sub(Vector2::new(scale_h, scale_h));
            pos_rel.sub(&origin_trans).scale(1. / (2. * scale_h))
        } else {
            let (hb, point_right, pos_scaled) = self.transform_screen_point_to_hb0(screen_pos)?;
            if hb != afl.hb || right != point_right {
                return None;
            }
            let pt = CELL_TRANSFORMS[f][r]
                .invert()
                .unwrap()
                .map_point(Point::new(pos_scaled.x, pos_scaled.y));
            if right {
                Vector2::new(pt.x, pt.y)
            } else {
                Vector2::new(1. - pt.y, pt.x)
            }
        };
        Some(point)
    }
    pub fn set_possible_moves_for_move_info(&mut self, square: FieldLocation) {
        let fv = self.board.board[usize::from(square)];
        let (color, piece_type) = FieldValue::from(fv).unwrap();
        if !self.show_non_legal_move_hints {
            let mut board = self.board.clone();
            board.turn = color;
            let mut moves = Default::default();
            let mp = MovegenParams::new(&mut self.board, MovegenOptions::default());
            board.gen_moves_for_field(square, &mut moves, &mp);
            for m in moves {
                self.possible_moves.set(usize::from(m.target), true);
                if let MoveType::Castle(short) = m.move_type {
                    self.possible_moves.set(
                        usize::from(
                            self.board.possible_rooks_for_castling[usize::from(self.board.turn)]
                                [usize::from(short)]
                            .unwrap(),
                        ),
                        true,
                    );
                }
            }
            return;
        }

        let oriented_afl = AnnotatedFieldLocation::from(square);
        let mff = &self.board.moves_for_board[usize::from(square)];
        if let Queen | Rook = piece_type {
            for loc in mff.orthogonal_lines {
                self.possible_moves.set(usize::from(loc), true);
            }
        }
        if let Queen | Bishop = piece_type {
            for loc in
                mff.diagonal_lines[0..*mff.diagonal_line_ends.last().unwrap() as usize].iter()
            {
                self.possible_moves.set(usize::from(*loc), true);
            }
        }
        if Knight == piece_type {
            for loc in mff.knight_moves.iter() {
                self.possible_moves.set(usize::from(*loc), true);
            }
        }
        if Pawn == piece_type {
            for dir in [false, true] {
                if let Some((take_1, take_2)) = move_diagonal(oriented_afl, true, dir) {
                    self.possible_moves.set(usize::from(take_1.loc), true);
                    take_2.map(|take_2| self.possible_moves.set(usize::from(take_2.loc), true));
                }
            }
            if let Some(afl) = move_rank(oriented_afl, true) {
                self.possible_moves.set(usize::from(afl.loc), true);
                if oriented_afl.rank == 2 {
                    self.possible_moves
                        .set(usize::from(move_rank(afl, true).unwrap().loc), true);
                }
            }
        }
        if King == piece_type {
            for up_down in [false, true] {
                for left_right in [false, true] {
                    if let Some((take_1, take_2)) = move_diagonal(oriented_afl, up_down, left_right)
                    {
                        self.possible_moves.set(usize::from(take_1.loc), true);
                        take_2.map(|take_2| self.possible_moves.set(usize::from(take_2.loc), true));
                    }
                }
                if let Some(afl) = move_rank(oriented_afl, up_down) {
                    self.possible_moves.set(usize::from(afl.loc), true);
                }
                if let Some(afl) = move_file(oriented_afl, up_down) {
                    self.possible_moves.set(usize::from(afl.loc), true);
                }
            }
            for loc in self.board.possible_rooks_for_castling[usize::from(color)] {
                loc.map(|loc| self.possible_moves.set(usize::from(loc), true));
            }
        }
    }
    pub fn set_possible_moves(&mut self, square: FieldLocation) {
        let mut moves = Default::default();
        if self.allow_illegal_moves {
            for i in 0..BOARD_SIZE {
                if FieldValue::from(self.board.board[i]).piece_type() != Some(King)
                    && i != usize::from(square)
                {
                    self.possible_moves.set(i, true);
                }
            }
            return;
        }
        let mp = MovegenParams::new(&mut self.board, MovegenOptions::default());
        self.board.gen_moves_for_field(square, &mut moves, &mp);
        for m in moves {
            if FieldValue::from(self.board.board[usize::from(m.target)]).piece_type() == Some(King)
            {
                // prevent moves that would capture a king since they cause our board to freak out
                continue;
            }
            self.possible_moves.set(usize::from(m.target), true);
            if let MoveType::Castle(short) = m.move_type {
                self.possible_moves.set(
                    usize::from(
                        self.board.possible_rooks_for_castling[usize::from(self.board.turn)]
                            [usize::from(short)]
                        .unwrap(),
                    ),
                    true,
                );
            }
        }
    }
    pub fn mouse_clicked(&mut self, right_click: bool) {
        let square = self.get_board_pos_from_screen_pos(self.cursor_pos);
        if right_click {
            self.reset_effects();
            self.move_info_square = square;
            if let Some(square) = square {
                if FieldValue::from(self.board.board[usize::from(square)]).is_some() {
                    self.set_possible_moves_for_move_info(square);
                }
            }
            return;
        }
        let prev = self.selected_square;
        self.selected_square = None;
        if let Some(square) = square {
            if let Some(src) = prev {
                if self.promotion_preview == Some(square) {
                    let cursor_quadrant = self
                        .get_screen_pos_in_field(self.cursor_pos, square)
                        .map(|pos| get_quadrant_from_unit_box_pos(pos, self.transformed_pieces));
                    if let Some(quadrant) = cursor_quadrant {
                        let (piece_type, _, _) = PROMOTION_QUADRANTS[quadrant as usize];
                        if self.apply_move(src, square, Some(piece_type)) {
                            return;
                        }
                    }
                }
                if self.apply_move(src, square, None) {
                    self.reset_effects();
                    return;
                }
            }
            let field_value = self.board.get_field_value(square);
            self.reset_effects();
            if let Some((color, _)) = *field_value {
                if !self.allow_illegal_moves && color != self.board.turn {
                    return;
                }
                self.dragged_square = Some(square);
                if Some(square) != prev {
                    self.selected_square = self.dragged_square;
                }
                self.set_possible_moves(square);
            }
        } else {
            self.reset_effects();
        }
    }
    pub fn mouse_released(&mut self, right_click: bool) {
        if right_click {
            self.reset_effects();
            return;
        }
        let square = self.get_board_pos_from_screen_pos(self.cursor_pos);
        if let Some(src) = self.dragged_square {
            if let Some(tgt) = square {
                if self.apply_move(src, tgt, None) {
                    return;
                }
            }
        }
        self.dragged_square = None;
        if square != self.selected_square {
            self.selected_square = None;
            self.possible_moves.fill(false);
        }
    }
    pub fn mouse_moved(&mut self, pos: Vector2<i32>) {
        self.cursor_pos = pos;
        self.hovered_square = None;
        if self.dragged_square.is_some() {
            if let Some(sq) = self.get_board_pos_from_screen_pos(self.cursor_pos) {
                if self.possible_moves[usize::from(sq)] {
                    self.hovered_square = Some(sq);
                }
            }
        }
    }

    pub fn ctrl_pressed(&mut self) {
        self.allow_illegal_moves = true;
        self.show_non_legal_move_hints = true;
        if let Some(square) = self.move_info_square {
            self.possible_moves.fill(false);
            self.set_possible_moves_for_move_info(square);
        } else if let Some(square) = self.selected_square {
            self.possible_moves.fill(false);
            self.set_possible_moves(square);
        }
    }
    pub fn ctrl_released(&mut self) {
        self.allow_illegal_moves = false;
        self.show_non_legal_move_hints = false;
        if let Some(square) = self.move_info_square {
            self.possible_moves.fill(false);
            self.set_possible_moves_for_move_info(square);
        } else if let Some(square) = self.selected_square {
            let dragged_square = self.dragged_square;
            self.reset_effects();
            if FieldValue::from(self.board.board[usize::from(square)]).color()
                == Some(self.board.turn)
            {
                self.selected_square = Some(square);
                self.dragged_square = dragged_square;
                self.possible_moves.fill(false);
                self.set_possible_moves(square);
            }
        }
    }
    pub fn apply_move_with_history(&mut self, mov: Move, color: three_player_chess::board::Color) {
        println!("making move: {}", mov.to_string(&mut self.board));
        let rm = ReversableMove::new(&self.board, mov);
        self.history.push((rm.clone(), color));
        self.board.perform_reversable_move(&rm, true);
        println!("state: {}", self.board.state_string());
        self.reset_effects();
    }
    pub fn apply_move(
        &mut self,
        src: FieldLocation,
        tgt: FieldLocation,
        promotion: Option<PieceType>,
    ) -> bool {
        if src == tgt || !self.possible_moves[usize::from(tgt)] {
            return false;
        }
        let mut mov = Move {
            source: src,
            target: tgt,
            move_type: MoveType::Slide,
        };
        let src_val = self.board.get_field_value(src);
        let tgt_val = self.board.get_field_value(tgt);
        let src_afl = AnnotatedFieldLocation::from(src);
        let tgt_afl = AnnotatedFieldLocation::from_with_origin(src_afl.hb, tgt);
        if tgt_val.is_some() {
            mov.move_type = MoveType::Capture(tgt_val.into());
        }
        if let Some((color, King)) = *src_val {
            let mut castle = false;
            if src_afl.file.abs_diff(tgt_afl.file) != 1 && src_afl.rank == tgt_afl.rank {
                castle = true;
            } else if src_val.piece_type() == Some(Rook) && Some(color) == tgt_val.color() {
                castle = true;
            }
            if castle {
                //TODO: illegal moves might break this
                let short = tgt_afl.file > src_afl.file;
                mov.move_type = MoveType::Castle(short);
                mov.target = get_castling_target(src_afl.hb, true, short);
            }
        }
        if let Some((color, Pawn)) = *src_val {
            if let Some(promotion) = promotion {
                if tgt_val.is_some() {
                    mov.move_type = MoveType::CapturePromotion(tgt_val.into(), promotion);
                } else {
                    mov.move_type = MoveType::Promotion(promotion);
                }
            } else {
                let tgt_rot = AnnotatedFieldLocation::from_with_origin(color, tgt);
                if tgt_rot.rank == RS {
                    self.reset_effects();
                    self.selected_square = Some(src_afl.loc);
                    self.promotion_preview = Some(tgt_rot.loc);
                    self.possible_moves.set(usize::from(tgt), true);
                    return true;
                } else if self.board.possible_en_passant[usize::from(tgt.hb())] == Some(tgt) {
                    let ep_square = AnnotatedFieldLocation::from_file_and_rank(
                        tgt_rot.origin,
                        tgt_rot.hb,
                        tgt_rot.file,
                        tgt_rot.rank - 1,
                    )
                    .loc;
                    mov.move_type = MoveType::EnPassant(
                        self.board.get_packed_field_value(ep_square),
                        ep_square,
                    );
                }
            }
        }

        if self.board.is_valid_move(mov) {
            self.apply_move_with_history(mov, self.board.turn);
            return true;
        } else if self.allow_illegal_moves {
            if src_val.color() != Some(self.board.turn) {
                let original_turn = self.board.turn;
                self.board.turn = src_val.color().unwrap();
                self.apply_move_with_history(mov, original_turn);
            } else {
                self.apply_move_with_history(mov, self.board.turn);
            }
            return true;
        }
        false
    }
    pub fn recolor(&mut self) {
        let player = 2; //rand::random::<usize>() % 3;
        println!("recoloring player {} ...", player + 1);
        let pc = self.player_colors[player];
        let color = Color::from_rgb(
            (rand::random::<u8>() / 2 + 50).wrapping_add(pc.b()),
            (rand::random::<u8>() / 2 + 50).wrapping_add(pc.g()),
            (rand::random::<u8>() / 2 + 50).wrapping_add(pc.r()),
        );
        let base_player = rand::random::<usize>() % 3;
        self.pieces[player] = recolor_images(
            &PIECE_IMAGES[base_player],
            ORIGINAL_PIECE_COLORS[base_player],
            color,
            20,
        );
        println!("recoloring player {} is done", player + 1);
        self.player_colors[player] = color;
    }
    pub fn reset_effects(&mut self) {
        self.hovered_square = None;
        self.selected_square = None;
        self.move_info_square = None;
        self.promotion_preview = None;
        self.dragged_square = None;
        self.possible_moves.fill(false);
        for c in board::Color::iter() {
            let kp = self.board.king_positions[usize::from(*c)];
            self.king_in_check[usize::from(*c)] =
                self.board.is_piece_capturable_at(kp, None, None).is_some();
        }
    }
    pub fn reset(&mut self) {
        self.board = ThreePlayerChess::default();
        self.reset_effects();
        self.history.clear();
    }
    pub fn rotate(&mut self) {
        self.origin = board::Color::from_u8((HB_COUNT + usize::from(self.origin) - 1) as u8 % 3);
    }
    pub fn undo_move(&mut self) {
        let rm = self.history.pop();
        if let Some((rm, turn)) = rm {
            self.board.revert_move(&rm);
            self.reset_effects();
            println!("undid move: {}", rm.mov.to_string(&mut self.board));
            self.board.turn = turn;
            println!("state: {}", self.board.state_string());
        }
    }
}
