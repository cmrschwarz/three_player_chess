use num_traits::FromPrimitive;

use crate::board::*;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MoveType {
    Slide,
    Capture,
    EnPassant,
    CastleShort,
    CastleLong,
    Promotion(PieceType),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Move {
    pub move_type: MoveType,
    pub source: FieldLocation,
    pub target: FieldLocation,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct AnnotatedFieldLocation {
    pub origin: Color,
    pub loc: FieldLocation,
    pub hb: Color,
    pub file: i8,
    pub rank: i8,
}
impl AnnotatedFieldLocation {
    pub fn new(origin: Color, loc: u8, hb: Color, file: i8, rank: i8) -> Self {
        AnnotatedFieldLocation {
            origin,
            loc: FieldLocation::from(loc),
            hb,
            file,
            rank,
        }
    }
    pub fn from_field_with_origin_and_hb(origin: Color, hb: Color, field: FieldLocation) -> Self {
        AnnotatedFieldLocation {
            origin,
            loc: field,
            hb,
            file: get_file(field, origin),
            rank: get_rank(field, origin),
        }
    }
    pub fn from_field_with_origin(origin: Color, field: FieldLocation) -> Self {
        Self::from_field_with_origin_and_hb(origin, get_hb(field), field)
    }
    pub fn from_field(field: FieldLocation) -> Self {
        let hb = get_hb(field);
        Self::from_field_with_origin_and_hb(hb, hb, field)
    }
    pub fn reorient(&mut self, origin: Color) {
        if origin != self.origin {
            self.file = invert_coord(self.file);
            self.rank = invert_coord(self.rank);
        }
    }
}

fn get_hb(field: FieldLocation) -> Color {
    Color::from_usize(usize::from(field) / HB_SIZE + 1).unwrap()
}

fn get_next_hb(color: Color, clockwise: bool) -> Color {
    let dir: i8 = if clockwise { 1 } else { -1 };
    const CC: i8 = COLOR_COUNT as i8;
    Color::from(((u8::from(color) as i8 - 1 + CC + dir) % CC) as u8 + 1)
}

fn invert_coord(coord: i8) -> i8 {
    (ROW_SIZE + 1) as i8 - coord
}
fn coord_dir(positive: bool) -> i8 {
    if positive {
        1i8
    } else {
        -1i8
    }
}

fn coord_in_bounds(coord: i8) -> bool {
    coord >= 1 && coord <= 8
}

// convenience wrapper for ternary usecases
fn adjust_coord(coord: i8, invert: bool) -> i8 {
    if invert {
        invert_coord(coord)
    } else {
        coord
    }
}
fn get_raw_rank(field: FieldLocation) -> i8 {
    ((u8::from(field) % HB_SIZE as u8) / ROW_SIZE as u8 + 1) as i8
}
fn get_rank(field: FieldLocation, color: Color) -> i8 {
    adjust_coord(get_raw_rank(field), get_hb(field) != color)
}

fn get_raw_file(field: FieldLocation) -> i8 {
    (u8::from(field) % ROW_SIZE as u8 + 1) as i8
}
fn get_file(field: FieldLocation, color: Color) -> i8 {
    adjust_coord(get_raw_file(field), get_hb(field) != color)
}

fn get_hb_offset(color: Color) -> u8 {
    (u8::from(color) - 1) * 32
}

fn move_rank(field: AnnotatedFieldLocation, up: bool) -> Option<AnnotatedFieldLocation> {
    let tgt_rank = field.rank + if up { 1i8 } else { -1i8 };
    if !coord_in_bounds(tgt_rank) {
        return None;
    }
    if (tgt_rank <= 3 || (tgt_rank == 4 && up)) || (tgt_rank > 5 || (tgt_rank == 5 && !up)) {
        return Some(AnnotatedFieldLocation::new(
            field.origin,
            (u8::from(field.loc) as i8 + coord_dir(up == (tgt_rank <= 4)) * ROW_SIZE as i8) as u8,
            field.hb,
            field.file,
            tgt_rank,
        ));
    }
    let hb = get_next_hb(field.hb, (field.file <= 4) == up);
    assert!(hb != field.origin || field.hb != field.origin);
    let file_idx = adjust_coord(field.file, field.origin != hb) - 1;
    Some(AnnotatedFieldLocation::new(
        field.origin,
        get_hb_offset(hb) + 3 * ROW_SIZE as u8 + file_idx as u8,
        hb,
        field.file,
        tgt_rank,
    ))
}

fn move_file(field: AnnotatedFieldLocation, right: bool) -> Option<AnnotatedFieldLocation> {
    let inverted = field.hb != field.origin;
    let tgt_file = field.file + coord_dir(right);
    let dir_raw = coord_dir(inverted != right);
    coord_in_bounds(tgt_file).then(|| {
        AnnotatedFieldLocation::new(
            field.origin,
            (u8::from(field.loc) as i8 + dir_raw) as u8,
            field.hb,
            tgt_file,
            field.rank,
        )
    })
}

fn get_field_on_next_hb(loc: u8) -> u8 {
    (loc + HB_SIZE as u8) % (HB_COUNT * HB_SIZE) as u8
}

fn move_diagonal(
    field: AnnotatedFieldLocation,
    up: bool,
    right: bool,
) -> Option<(AnnotatedFieldLocation, Option<AnnotatedFieldLocation>)> {
    let tgt_rank = field.rank + coord_dir(up);
    let tgt_file = field.file + coord_dir(right);
    if !coord_in_bounds(tgt_file) || !coord_in_bounds(tgt_rank) {
        return None;
    }
    if (field.rank == 4 && up) || (field.rank == 5 && !up) {
        if (field.file == 4 && right) || (field.file == 5 && !right) {
            let hb1 = get_next_hb(field.hb, true);
            let hb2 = get_next_hb(hb1, true);
            let loc1 = get_field_on_next_hb(u8::from(field.loc));
            let loc2 = get_field_on_next_hb(loc1);
            return Some((
                AnnotatedFieldLocation::new(field.origin, loc1, hb1, tgt_file, tgt_rank),
                Some(AnnotatedFieldLocation::new(
                    field.origin,
                    loc2,
                    hb2,
                    tgt_file,
                    tgt_rank,
                )),
            ));
        }
        let tgt_hb = get_next_hb(get_hb(field.loc), (field.file <= 4) == up);
        let loc = get_hb_offset(tgt_hb) as i8 + 3 * ROW_SIZE as i8 + invert_coord(tgt_file) - 1;
        return Some((
            AnnotatedFieldLocation::new(field.origin, loc as u8, tgt_hb, tgt_file, tgt_rank),
            None,
        ));
    }
    let rank_dir = coord_dir(up == (field.hb == field.origin));
    let file_dir = coord_dir(right == (field.hb == field.origin));
    Some((
        AnnotatedFieldLocation::new(
            field.origin,
            (u8::from(field.loc) as i8 + rank_dir * ROW_SIZE as i8 + file_dir) as u8,
            field.hb,
            tgt_file,
            tgt_rank,
        ),
        None,
    ))
}

impl ThreePlayerChess {
    fn gen_move(
        &self,
        src: AnnotatedFieldLocation,
        tgt: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
    ) -> bool {
        match self.get_field_value(tgt.loc) {
            FieldValue(None) => {
                moves.push(Move {
                    move_type: MoveType::Slide,
                    source: src.loc,
                    target: tgt.loc,
                });
            }
            FieldValue(Some((color, _))) => {
                if color != self.turn {
                    moves.push(Move {
                        move_type: MoveType::Capture,
                        source: src.loc,
                        target: tgt.loc,
                    });
                }
                return false;
            }
        }
        return true;
    }
    fn gen_move_opt(
        &self,
        src: AnnotatedFieldLocation,
        tgt: Option<AnnotatedFieldLocation>,
        moves: &mut Vec<Move>,
    ) -> bool {
        match tgt {
            Some(tgt) => self.gen_move(src, tgt, moves),
            None => false,
        }
    }
    fn gen_moves_rook(&self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        let rs = ROW_SIZE as i8;
        for (length, rank, increase) in [
            (rs - field.rank, true, true),  // up
            (field.rank - 1, true, false),  // down
            (field.file - 1, false, false), // left
            (rs - field.file, false, true), // right
        ] {
            let mut pos = field;
            for _ in 0..length {
                let res = if rank {
                    move_rank(pos, increase)
                } else {
                    move_file(pos, increase)
                };
                match res {
                    Some(tgt) => {
                        if !self.gen_move(pos, tgt, moves) {
                            break;
                        }
                        pos = tgt;
                    }
                    _ => break,
                }
            }
        }
    }
    fn gen_moves_bishop(&self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        let rs = ROW_SIZE as i8;
        use std::cmp::min;
        for (length, up, right) in [
            (min(rs - field.file, rs - field.rank), true, true), // up right
            (min(field.file, rs - field.rank), true, false),     // up left
            (min(field.file, field.rank), false, false),         // down left
            (min(rs - field.file, field.rank), false, true),     // down right
        ] {
            let mut pos = field;
            for i in 0..length {
                match move_diagonal(pos, up, right) {
                    None => break,
                    Some((one, None)) => {
                        if !self.gen_move(field, one, moves) {
                            break;
                        };
                        pos = one;
                    }
                    Some((mut one, Some(mut two))) => {
                        let swap_dir = pos.hb != field.origin;
                        if swap_dir && one.hb != field.origin {
                            std::mem::swap(&mut one, &mut two);
                        }
                        if self.gen_move(field, two, moves) {
                            let mut pos2 = two;
                            for _ in i..length {
                                match move_diagonal(pos2, up != swap_dir, right != swap_dir) {
                                    None => break,
                                    Some((one, None)) => {
                                        if !self.gen_move(pos2, one, moves) {
                                            break;
                                        }
                                        pos2 = one;
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                        if !self.gen_move(field, one, moves) {
                            break;
                        }
                        pos = one;
                    }
                }
            }
        }
    }
    fn gen_moves_knight(&self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        for right in [true, false] {
            if let Some(r1) = move_file(field, right) {
                for up in [true, false] {
                    if let Some(r1u1) = move_rank(r1, up) {
                        self.gen_move_opt(field, move_rank(r1u1, up), moves);
                    }
                }
                if let Some(r2) = move_file(r1, right) {
                    for up in [true, false] {
                        self.gen_move_opt(field, move_rank(r2, up), moves);
                    }
                }
            }
        }
        for up in [true, false] {
            let rank_adjusted = adjust_coord(field.rank, !up);
            if rank_adjusted != 3 && rank_adjusted != 4 {
                continue;
            }
            let u1 = move_rank(field, up).unwrap();
            let u2 = move_rank(u1, up).unwrap();
            for right in [true, false] {
                self.gen_move_opt(field, move_file(u2, right), moves);
            }
            if rank_adjusted == 4 && field.file > 2 && field.file < 7 {
                let right = field.file < 5;
                let u1r1 = move_file(u1, right).unwrap();
                let u1r2 = move_file(u1r1, right).unwrap();
                self.gen_move(field, u1r2, moves);
            }
        }
    }
    fn gen_moves_king(&self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        for tgt in [
            move_file(field, false),
            move_file(field, true),
            move_rank(field, false),
            move_rank(field, false),
        ] {
            self.gen_move_opt(field, tgt, moves);
        }
        for right in [true, false] {
            for up in [true, false] {
                match move_diagonal(field, up, right) {
                    Some((one, two)) => {
                        self.gen_move(field, one, moves);
                        self.gen_move_opt(field, two, moves);
                    }
                    None => {}
                }
            }
        }
    }
    fn gen_moves_pawn(&self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        todo!()
    }
    fn gen_moves_queen(&self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        self.gen_moves_rook(field, moves);
        self.gen_moves_bishop(field, moves);
    }
    pub fn gen_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();
        for (loc, val) in self.board.iter().enumerate() {
            match FieldValue::from(*val) {
                FieldValue(Some((color, piece_type))) if color == self.turn => {
                    let field = AnnotatedFieldLocation::from_field(FieldLocation::from(loc));
                    match piece_type {
                        PieceType::Pawn => self.gen_moves_pawn(field, &mut moves),
                        PieceType::Knight => self.gen_moves_knight(field, &mut moves),
                        PieceType::Bishop => self.gen_moves_bishop(field, &mut moves),
                        PieceType::Rook => self.gen_moves_rook(field, &mut moves),
                        PieceType::Queen => self.gen_moves_queen(field, &mut moves),
                        PieceType::King => self.gen_moves_king(field, &mut moves),
                    }
                }
                _ => {}
            }
        }
        moves
    }
    pub fn get_field_value(&self, field: FieldLocation) -> FieldValue {
        FieldValue::from(self.board[usize::from(field)])
    }
}
