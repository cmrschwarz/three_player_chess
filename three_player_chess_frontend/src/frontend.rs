use nalgebra::{ArrayStorage, Matrix3, OMatrix, OVector, Transform2, Vector2};
use skia_safe::{
    radians_to_degrees, Canvas, Color, Color4f, Data, Font, Image, Matrix, Paint, PaintStyle, Path,
    Point, Rect, Typeface,
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
    pub background: Color,
    pub border: Color,
    pub prev_second: f32,
    pub transformed_pieces: bool,
    pub board: ThreePlayerChess,
    pub selected_square: Option<AnnotatedFieldLocation>,
    pub hovered_square: Option<AnnotatedFieldLocation>,
    pub dragged_square: Option<AnnotatedFieldLocation>,
    pub possible_moves: bitvec::array::BitArray<bitvec::order::LocalBits, [u32; 3]>,
    pub cursor_pos: Vector2<i32>,
    pub board_radius: f32,
    pub board_origin: Vector2<i32>,
    pub origin: board::Color,
}

const HEX_CENTER_ANGLE: f32 = 2. * PI / 6.;
// assumes origin being top left, forms a U
const UNIT_SQUARE: [Vector2<f32>; 4] = [
    Vector2::new(0., 0.),
    Vector2::new(0., 1.),
    Vector2::new(1., 1.),
    Vector2::new(1., 0.),
];
lazy_static! {
    static ref HEX_SIDE_LEN: f32 = (HEX_CENTER_ANGLE / 2.).sin();
    static ref HEX_HEIGHT: f32 =  (HEX_CENTER_ANGLE / 2.).cos();

    static ref PIECE_IMAGES: [[Image; PIECE_COUNT]; HB_COUNT] = {
        PIECES.map(|pieces_for_color| {
            pieces_for_color
                .map(|piece_data| Image::from_encoded(Data::new_copy(&piece_data)).unwrap())
        })
    };

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

impl Frontend {
    pub fn new() -> Frontend {
        Frontend {
            prev_second: -1.0,
            board: ThreePlayerChess::from_str("BCEFGH2A5E4D5H4C5/BG1/CF1/AH1/D1/E1/AH/:LKIDCBA7J6/KB8/JC8/LA8/L5/D8/LA/:HGFEILbJaK9/GKc/FJc/HLc/Ec/Ic/HL/Ka:7:7").unwrap(),//Default::default(),
            font:  Font::from_typeface(Typeface::from_data(Data::new_copy(&FONT), None).expect("Failed to load font 'Roboto-Regular.ttf'"), None),
            black: Color::from_rgb(230, 230, 230),
            white: Color::from_rgb(130, 130, 130),
            selection_color: Color::from_argb(128, 148, 195, 143),
            background: Color::from_rgb(142, 83, 46),
            border: Color::from_rgb(0, 0, 0),
            transformed_pieces: true,
            selected_square: None,
            cursor_pos: Vector2::new(-1, -1),
            board_radius: 1.,
            board_origin: Vector2::new(1, 1),
            origin: board::Color::C0,
            possible_moves: Default::default(),
            dragged_square: None,
            hovered_square: None
        }
    }
    pub fn render_background(&mut self, canvas: &mut Canvas) {
        canvas.clear(self.background);
        //TODO: config option for this
        canvas.scale((self.board_radius * 1.05, self.board_radius * 1.05));
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
        let mut paint = Paint::new(&Color4f::from(self.border), None);
        paint.set_style(PaintStyle::Fill);
        canvas.draw_path(&path, &paint);
    }
    pub fn render_dragged_piece(&mut self, canvas: &mut Canvas) {
        if let Some(field) = self.dragged_square {
            if let Some((color, piece_type)) =
                *FieldValue::from(self.board.board[usize::from(field.loc)])
            {
                let size = self.board_radius * 0.1;
                let img = &PIECE_IMAGES[usize::from(color)][usize::from(piece_type)];
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
                    &Paint::default(),
                );
            } else {
                // can happen if the board was reloaded etc.
                self.dragged_square = None;
            }
        }
    }
    pub fn render(&mut self, canvas: &mut Canvas) {
        let dim = canvas.image_info().dimensions();

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
    ) {
        let mut file_rot = file as i8;
        let mut rank_rot = rank as i8;

        if right {
            (file_rot, rank_rot) = (HB_ROW_COUNT as i8 + file_rot + 1, rank_rot + 1);
        } else {
            (file_rot, rank_rot) = (HB_ROW_COUNT as i8 - file_rot, rank_rot + 1);
        }
        let field = AnnotatedFieldLocation::from_file_and_rank(hb, hb, file_rot, rank_rot);

        let field_color = if Some(field) == self.selected_square {
            self.selection_color
        } else if ((file % 2) + (rank % 2) + (right as usize)) % 2 == 0 {
            self.black
        } else {
            self.white
        };

        let possible_move = self.possible_moves[usize::from(field.loc)];

        canvas.concat(&CELL_TRANSFORMS[file][rank]);
        let paint = Paint::new(&Color4f::from(field_color), None);
        canvas.draw_rect(Rect::from_xywh(0., 0., 1., 1.), &paint);

        if let Some((color, piece_value)) =
            *FieldValue::from(self.board.board[usize::from(field.loc)])
        {
            let img = &PIECE_IMAGES[usize::from(color)][usize::from(piece_value)];

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
        } else if possible_move {
            let selection_paint = Paint::new(&Color4f::from(self.selection_color), None);
            canvas.draw_circle(Point::new(0.5, 0.5), 0.2, &selection_paint);
        }
    }
    pub fn render_hex_board(&mut self, canvas: &mut Canvas, hb: board::Color, right: bool) {
        canvas.rotate(
            radians_to_degrees(HEX_CENTER_ANGLE * 2. * usize::from(hb) as f32),
            None,
        );
        if !right {
            canvas.scale((-self.board_radius, self.board_radius));
        } else {
            canvas.scale((self.board_radius, self.board_radius));
        }
        for file in 0..HB_ROW_COUNT {
            for rank in 0..HB_ROW_COUNT {
                canvas.save();
                self.draw_cell(canvas, hb, right, file, rank);
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
        self.possible_moves.set_all(false);
        if let Some(square) = square {
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
                for m in moves {
                    self.possible_moves.set(usize::from(m.target), true);
                }
            }
        }
    }
    pub fn released(&mut self) {
        let square = self.get_board_pos_from_screen_pos(self.cursor_pos);
        if let Some(ds) = self.dragged_square {
            if let Some(sq) = square {
                if ds != sq && self.possible_moves[usize::from(sq.loc)] {
                    let mut mov = Move {
                        source: ds.loc,
                        target: sq.loc,
                        move_type: MoveType::Slide,
                    };
                    let field_val = self.board.board[usize::from(sq.loc)];
                    if FieldValue::from(field_val).is_some() {
                        mov.move_type = MoveType::Capture(field_val);
                    }
                    if self.board.is_valid_move(mov) {
                        self.board.make_move(mov);
                        self.board.apply_move_sideeffects(mov);
                    }
                }
            }
        }
        self.dragged_square = None;
        if square != self.selected_square {
            self.selected_square = None;
            self.possible_moves.set_all(false);
        }
    }
}
