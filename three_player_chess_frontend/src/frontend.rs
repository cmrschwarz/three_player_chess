use nalgebra::{ArrayStorage, Matrix3, OMatrix, OVector, Transform2, Vector2};
use skia_safe::{
    radians_to_degrees, Bitmap, Canvas, Color, ColorType, Data, Font, IPoint, Image, ImageInfo,
    Matrix, Paint, PaintStyle, Path, Point, Rect, Typeface,
};
use std::f32::consts::PI;
use std::ops::{Add, Sub};
use three_player_chess::board;
use three_player_chess::board::*;
use three_player_chess::movegen::HBRC;
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
    pub font: Font,

    pub black: Color,
    pub white: Color,
    pub selection_color: Color,
    pub danger: Color,
    pub background: Color,
    pub player_colors: [Color; HB_COUNT],
    pub prev_second: f32,
    pub transformed_pieces: bool,
    pub board: ThreePlayerChess,
    pub selected_square: Option<AnnotatedFieldLocation>,
    pub hovered_square: Option<AnnotatedFieldLocation>,
    pub dragged_square: Option<AnnotatedFieldLocation>,
    pub possible_moves: bitvec::BitArr!(for (32 * HB_COUNT), in u32), //  BitArray<bitvec::order::LocalBits, [u32; HB_COUNT]>,
    pub king_in_check: bool,
    pub cursor_pos: Vector2<i32>,
    pub board_radius: f32,
    pub board_origin: Vector2<i32>,
    pub origin: board::Color,
    pub pieces: [[Image; PIECE_COUNT]; HB_COUNT],
}

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

fn drop_perpendicular(
    point: Vector2<f32>,
    line_p1: Vector2<f32>,
    line_p2: Vector2<f32>,
) -> Vector2<f32> {
    let line_direction = line_p2.sub(line_p1).normalize();
    line_p1.add(line_direction.scale(point.sub(line_p1).dot(&line_direction)))
}

fn square_in_quad(quad: &[Vector2<f32>; 4]) -> (Vector2<f32>, f32) {
    let center = center_of_quad(quad);
    let mut flank_centers: [Vector2<f32>; 4] = Default::default();
    for i in 0..4 {
        flank_centers[i] = drop_perpendicular(center, quad[i], quad[(i + 1) % 4]);
    }
    let mut bot = flank_centers[0].y;
    let mut top = flank_centers[0].y;
    let mut left = flank_centers[0].x;
    let mut right = flank_centers[0].x;
    for i in 1..4 {
        bot = bot.min(flank_centers[i].y);
        top = top.max(flank_centers[i].y);
        left = left.max(flank_centers[i].x);
        right = right.min(flank_centers[i].x);
    }
    let height = (center.y - bot).abs().min(top - center.y).abs() * 2.;
    let width = (center.x - left).abs().min(right - center.x).abs() * 2.;
    let size = height.min(width);
    (center.sub(Vector2::new(size / 2., size / 2.)), size)
}

impl Frontend {
    pub fn new() -> Frontend {
        Frontend {
            prev_second: -1.0,
            board: ThreePlayerChess::from_str("BCEFGH2A5E4D5H4C5/BG1/CF1/AH1/D4/E1/AH/:LKIDCBA7J6/KB8/JC8/LA8/L5/D8/LA/:HGFEILbJaK9/GKc/FJc/HLc/Ec/Ic/HL/Ka:0:0").unwrap(),//Default::default(),
            font:  Font::from_typeface(Typeface::from_data(Data::new_copy(&FONT), None).expect("Failed to load font 'Roboto-Regular.ttf'"), None),
            black: Color::from_rgb(161, 119, 67),
            white: Color::from_rgb(240, 217, 181) ,
            selection_color: Color::from_argb(128, 56, 173, 105),
            background: Color::from_rgb(201, 144, 73),
            danger: Color::from_rgb(232, 15, 13),
            transformed_pieces: true,
            selected_square: None,
            cursor_pos: Vector2::new(-1, -1),
            board_radius: 1.,
            board_origin: Vector2::new(1, 1),
            origin: board::Color::C0,
            possible_moves: Default::default(),
            dragged_square: None,
            hovered_square: None,
            player_colors: [Color::from_rgb(236, 236, 236), Color::from_rgb(41, 41, 41), Color::from_rgb(36, 36, 128)],
            king_in_check: false,
            pieces: PIECE_IMAGES.clone()
        }
    }
    pub fn render_background(&mut self, canvas: &mut Canvas) {
        canvas.clear(self.background);
        //TODO: config option for this
        let mut path = Path::new();
        for i in 0..6 {
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
        let mut paint = sk_paint(
            self.player_colors[usize::from(self.board.turn)],
            PaintStyle::Stroke,
        );
        let border_width = 0.02;
        paint.set_stroke_width(border_width);
        canvas.scale((
            self.board_radius * (1. + border_width / 2.),
            self.board_radius * (1. + border_width / 2.),
        ));
        if self.board.game_status == GameStatus::Ongoing {
            canvas.draw_path(&path, &paint);
        } else if let GameStatus::Win(winner, _) = self.board.game_status {
            paint.set_color(self.player_colors[usize::from(winner)]);
            canvas.draw_path(&path, &paint);
            let border_2 = 1. + border_width * 1.5;
            canvas.scale((border_2, border_2));
            paint.set_stroke_width(border_width / border_2);
            canvas.draw_path(&path, &paint);
        }
    }
    pub fn render_dragged_piece(&mut self, canvas: &mut Canvas) {
        if let Some(field) = self.dragged_square {
            if let Some((color, piece_type)) =
                *FieldValue::from(self.board.board[usize::from(field.loc)])
            {
                let size = self.board_radius * 0.1;
                let img = &self.pieces[usize::from(color)][usize::from(piece_type)];
                canvas.draw_image_rect(
                    img,
                    Some((
                        &Rect::from_xywh(0., 0., img.width() as f32, img.height() as f32),
                        skia_safe::canvas::SrcRectConstraint::Fast,
                    )),
                    Rect::from_xywh(
                        self.cursor_pos.x as f32 - size,
                        self.cursor_pos.y as f32 - size,
                        2. * size,
                        2. * size,
                    ),
                    &sk_paint_img(),
                );
            } else {
                // can happen if the board was reloaded etc.
                self.dragged_square = None;
            }
        }
    }
    pub fn render(&mut self, canvas: &mut Canvas) {
        let dim = canvas.image_info().dimensions();

        self.hovered_square = None;
        if self.dragged_square.is_some() {
            if let Some(sq) = self.get_board_pos_from_screen_pos(self.cursor_pos) {
                if self.possible_moves[usize::from(sq.loc)] {
                    self.hovered_square = Some(sq);
                }
            }
        }
        self.king_in_check = self.board.is_king_capturable(None);

        self.board_origin = Vector2::new(dim.width / 2, dim.height / 2);
        self.board_radius = std::cmp::min(dim.width, dim.height) as f32 * 0.46;
        let translation = self.board_origin.cast::<f32>();

        canvas.save();
        canvas.translate(Point {
            x: translation.x,
            y: translation.y,
        });

        canvas.save();
        self.render_background(canvas);
        canvas.restore();

        for c in board::Color::iter() {
            for right in [true, false] {
                canvas.save();
                self.render_hex_board(canvas, *c, right);
                canvas.restore();
            }
        }
        canvas.restore();

        canvas.save();
        self.render_dragged_piece(canvas);
        canvas.restore();
    }
    pub fn draw_cell(
        &mut self,
        canvas: &mut Canvas,
        hb: board::Color,
        right: bool,
        file: usize,
        rank: usize,
        rotation: f32,
    ) {
        let mut file_rot = file as i8;
        let mut rank_rot = rank as i8;

        if right {
            (file_rot, rank_rot) = (HB_ROW_COUNT as i8 + file_rot + 1, rank_rot + 1);
        } else {
            (file_rot, rank_rot) = (HB_ROW_COUNT as i8 - file_rot, rank_rot + 1);
        }
        let field = AnnotatedFieldLocation::from_file_and_rank(hb, hb, file_rot, rank_rot);

        let field_color = if ((file % 2) + (rank % 2) + (right as usize)) % 2 == 0 {
            self.black
        } else {
            self.white
        };

        let possible_move = self.possible_moves[usize::from(field.loc)];
        let selection_paint = sk_paint(self.selection_color, PaintStyle::Fill);
        let field_paint = sk_paint(field_color, PaintStyle::Fill);
        canvas.save();
        canvas.concat(&CELL_TRANSFORMS[file][rank]);
        canvas.draw_rect(&*UNIT_RECT, &field_paint);
        let selected = Some(field) == self.hovered_square
            || Some(field) == self.selected_square
            || Some(field) == self.dragged_square;
        if let Some((color, piece_value)) =
            *FieldValue::from(self.board.board[usize::from(field.loc)])
        {
            let king_check =
                self.king_in_check && piece_value == PieceType::King && color == self.board.turn;
            let img = &self.pieces[usize::from(color)][usize::from(piece_value)];

            let (mut sx, mut sy) = (1., 1.);
            if !right {
                sx = -1.;
            }
            if hb != color {
                sy = -1.;
            }
            if sx < 0. || sy < 0. {
                canvas.translate(Point::new(0.5, 0.5));
                canvas.scale((sx, sy));
                canvas.translate(Point::new(-0.5, -0.5));
            }

            if selected {
                canvas.draw_rect(&*UNIT_RECT, &selection_paint);
            } else if possible_move || king_check {
                let size = 0.35;
                let mut path = Path::new();
                for p in UNIT_SQUARE {
                    path.move_to(Point::new(p.x, p.y));
                    path.line_to(Point::new(p.x, if p.y > 0. { p.y - size } else { size }));
                    path.line_to(Point::new(if p.x > 0. { p.x - size } else { size }, p.y));
                    path.line_to(Point::new(p.x, p.y));
                }
                if king_check {
                    let danger_paint = sk_paint(self.danger, PaintStyle::Fill);
                    canvas.draw_path(&path, &danger_paint);
                } else {
                    canvas.draw_path(&path, &selection_paint);
                }
            }

            if self.transformed_pieces {
                canvas.draw_image_rect(
                    img,
                    Some((
                        &Rect::from_xywh(0., 0., img.width() as f32, img.height() as f32),
                        skia_safe::canvas::SrcRectConstraint::Fast,
                    )),
                    &*UNIT_RECT,
                    &sk_paint_img(),
                );
                canvas.restore();
            } else {
                canvas.restore();
                let (img_origin, size) = square_in_quad(&get_cell_quad(file, rank));
                let size_h = size / 2.;
                let t = img_origin.add(Vector2::new(size_h, size_h));
                canvas.translate(Point::new(t.x, t.y));
                canvas.scale((sx, 1.));
                canvas.rotate(-rotation, None);
                canvas.draw_image_rect(
                    img,
                    Some((
                        &Rect::from_xywh(0., 0., img.width() as f32, img.height() as f32),
                        skia_safe::canvas::SrcRectConstraint::Fast,
                    )),
                    &Rect::from_xywh(-size_h, -size_h, size, size),
                    &sk_paint_img(),
                );
            }
        } else {
            if selected {
                canvas.draw_rect(&*UNIT_RECT, &selection_paint);
            } else if possible_move {
                canvas.draw_circle(Point::new(0.5, 0.5), 0.2, &selection_paint);
            }
            canvas.restore();
        }
    }
    pub fn render_hex_board(&mut self, canvas: &mut Canvas, hb: board::Color, right: bool) {
        let rotation = radians_to_degrees(HEX_CENTER_ANGLE * 2. * usize::from(hb) as f32);
        canvas.rotate(rotation, None);
        if !right {
            canvas.scale((-self.board_radius, self.board_radius));
        } else {
            canvas.scale((self.board_radius, self.board_radius));
        }
        for file in 0..HB_ROW_COUNT {
            for rank in 0..HB_ROW_COUNT {
                canvas.save();
                self.draw_cell(canvas, hb, right, file, rank, rotation);
                canvas.restore();
            }
        }
    }
    pub fn get_board_pos_from_screen_pos(
        &mut self,
        screen_pos: Vector2<i32>,
    ) -> Option<AnnotatedFieldLocation> {
        let pos_rel: Vector2<f32> = screen_pos.sub(self.board_origin).cast::<f32>();
        if pos_rel.norm_squared() > self.board_radius * self.board_radius {
            return None;
        }
        let rot = ((-pos_rel.y).atan2(pos_rel.x) + 2.5 * PI) % (2. * PI);
        let hex_board = (HB_COUNT * 2 - (rot / HEX_CENTER_ANGLE) as usize) % 6;
        let hb = board::Color::from(((hex_board / 2) % 3) as u8);
        let right = hex_board % 2 == 0;
        let pos_rot = nalgebra::geometry::Rotation2::new(hex_board as f32 * -HEX_CENTER_ANGLE)
            .transform_vector(&pos_rel);
        let pos_scaled = pos_rot.scale(1. / self.board_radius);

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
                    return Some(AnnotatedFieldLocation::from_file_and_rank(
                        hb, hb, file, rank,
                    ));
                }
            }
        }
        None
    }

    pub fn clicked(&mut self) {
        let square = self.get_board_pos_from_screen_pos(self.cursor_pos);
        let prev = self.selected_square;
        self.selected_square = None;
        if let Some(square) = square {
            if let Some(src) = prev {
                if self.make_move(src.loc, square.loc).is_some() {
                    self.possible_moves.fill(false);
                    return;
                }
            }
            let field_value = FieldValue::from(self.board.board[usize::from(square.loc)]);
            if let Some((color, _)) = *field_value {
                if color != self.board.turn {
                    return;
                }
                self.dragged_square = Some(square);
                if Some(square) != prev {
                    self.selected_square = self.dragged_square;
                }
                let mut moves = Default::default();
                self.board.gen_moves_for_field(square.loc, &mut moves);
                self.possible_moves.fill(false);
                for m in moves {
                    self.possible_moves.set(usize::from(m.target), true);
                }
            }
        } else {
            self.possible_moves.fill(false);
        }
    }
    pub fn released(&mut self) {
        let square = self.get_board_pos_from_screen_pos(self.cursor_pos);
        if let Some(src) = self.dragged_square {
            if let Some(tgt) = square {
                self.make_move(src.loc, tgt.loc);
            }
        }
        self.dragged_square = None;
        if square != self.selected_square {
            self.selected_square = None;
            self.possible_moves.fill(false);
        }
    }
    pub fn make_move(&mut self, src: FieldLocation, tgt: FieldLocation) -> Option<Move> {
        if src != tgt && self.possible_moves[usize::from(tgt)] {
            let mut mov = Move {
                source: src,
                target: tgt,
                move_type: MoveType::Slide,
            };
            let field_val = self.board.board[usize::from(tgt)];
            if FieldValue::from(field_val).is_some() {
                mov.move_type = MoveType::Capture(field_val);
            }
            if self.board.is_valid_move(mov) {
                self.board.make_move(mov);
                self.board.apply_move_sideeffects(mov);
                return Some(mov);
            }
        }
        None
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
    pub fn reset(&mut self) {
        self.board = ThreePlayerChess::default();
        self.hovered_square = None;
        self.selected_square = None;
        self.dragged_square = None;
        self.possible_moves.fill(false);
    }
}
