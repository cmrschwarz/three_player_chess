use num_traits::FromPrimitive;

use crate::board::*;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MoveType {
    Slide,
    PawnMove,
    Capture(PackedFieldValue),
    EnPassant,
    Castle,
    Promotion(PieceType),
    ThreefoldRepetitionClaim,
    FiftyMoveRuleClaim,
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

const HBRC: i8 = HB_ROW_COUNT as i8;
const RS: i8 = ROW_SIZE as i8;

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
    RS + 1 - coord
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
    ((u8::from(field) % HB_SIZE as u8) / RS as u8 + 1) as i8
}
fn get_rank(field: FieldLocation, color: Color) -> i8 {
    adjust_coord(get_raw_rank(field), get_hb(field) != color)
}

fn get_raw_file(field: FieldLocation) -> i8 {
    (u8::from(field) % RS as u8 + 1) as i8
}
fn get_file(field: FieldLocation, color: Color) -> i8 {
    adjust_coord(get_raw_file(field), get_hb(field) != color)
}

fn get_hb_offset(color: Color) -> u8 {
    (u8::from(color) - 1) * HB_SIZE as u8
}

fn move_rank(field: AnnotatedFieldLocation, up: bool) -> Option<AnnotatedFieldLocation> {
    let tgt_rank = field.rank + coord_dir(up);
    if !coord_in_bounds(tgt_rank) {
        return None;
    }
    if (tgt_rank < HBRC || (tgt_rank == HBRC && up))
        || (tgt_rank > HBRC + 1 || (tgt_rank == HBRC + 1 && !up))
    {
        return Some(AnnotatedFieldLocation::new(
            field.origin,
            (u8::from(field.loc) as i8 + coord_dir(up == (tgt_rank <= HBRC)) * RS) as u8,
            field.hb,
            field.file,
            tgt_rank,
        ));
    }
    let hb = get_next_hb(field.hb, (field.file <= HBRC) == up);
    assert!(hb != field.origin || field.hb != field.origin);
    let file_idx = adjust_coord(field.file, field.origin != hb) - 1;
    Some(AnnotatedFieldLocation::new(
        field.origin,
        (get_hb_offset(hb) as i8 + (HBRC - 1) * RS + file_idx) as u8,
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
    if (field.rank == HBRC && up) || (field.rank == HBRC + 1 && !up) {
        if (field.file == HBRC && right) || (field.file == HBRC + 1 && !right) {
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
        let tgt_hb = get_next_hb(get_hb(field.loc), (field.file <= HBRC) == up);
        let loc = get_hb_offset(tgt_hb) as i8 + (HBRC - 1) * RS + invert_coord(tgt_file) - 1;
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
            (u8::from(field.loc) as i8 + rank_dir * RS + file_dir) as u8,
            field.hb,
            tgt_file,
            tgt_rank,
        ),
        None,
    ))
}

impl ThreePlayerChess {
    fn gen_move_unless_check(
        &self,
        src: AnnotatedFieldLocation,
        tgt: AnnotatedFieldLocation,
        move_type: MoveType,
        moves: &mut Vec<Move>,
    ) -> bool {
        // TODO: don't allow the move if it would cause a check
        moves.push(Move {
            move_type,
            source: src.loc,
            target: tgt.loc,
        });
        true
    }
    fn gen_move(
        &self,
        src: AnnotatedFieldLocation,
        tgt: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
    ) -> bool {
        let piece_value = self.get_packed_field_value(tgt.loc);
        match FieldValue::from(piece_value) {
            FieldValue(None) => self.gen_move_unless_check(src, tgt, MoveType::Slide, moves),
            FieldValue(Some((color, _))) if color != self.turn => {
                self.gen_move_unless_check(src, tgt, MoveType::Capture(piece_value), moves)
            }
            _ => false,
        }
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
        for (length, rank, increase) in [
            (RS - field.rank, true, true),  // up
            (field.rank - 1, true, false),  // down
            (field.file - 1, false, false), // left
            (RS - field.file, false, true), // right
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
        use std::cmp::min;
        for (length, up, right) in [
            (min(RS - field.file, RS - field.rank), true, true), // up right
            (min(field.file, RS - field.rank), true, false),     // up left
            (min(field.file, field.rank), false, false),         // down left
            (min(RS - field.file, field.rank), false, true),     // down right
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
            if rank_adjusted != HBRC - 1 && rank_adjusted != HBRC {
                continue;
            }
            let u1 = move_rank(field, up).unwrap();
            let u2 = move_rank(u1, up).unwrap();
            for right in [true, false] {
                self.gen_move_opt(field, move_file(u2, right), moves);
            }
            if rank_adjusted == HBRC && field.file > HBRC - 2 && field.file <= HBRC + 2 {
                let right = field.file < HBRC + 1;
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
    fn try_gen_pawn_capture(
        &self,
        src: AnnotatedFieldLocation,
        tgt: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
    ) -> bool {
        let piece_value = self.get_packed_field_value(tgt.loc);
        match FieldValue::from(piece_value) {
            FieldValue(Some((color, _))) if color != self.turn => {
                self.gen_move_unless_check(src, tgt, MoveType::Capture(piece_value), moves)
            }
            FieldValue(None) => {
                if tgt.rank == ROW_SIZE as i8 - 2
                    && self.possible_en_passant[u8::from(tgt.hb) as usize - 1] == Some(tgt.loc)
                {
                    self.gen_move_unless_check(src, tgt, MoveType::EnPassant, moves)
                } else {
                    false
                }
            }
            _ => false,
        }
    }
    fn gen_moves_pawn(&self, field: FieldLocation, moves: &mut Vec<Move>) {
        let src = AnnotatedFieldLocation::from_field_with_origin(self.turn, field);
        if let Some(up) = move_rank(src, true) {
            if let FieldValue(None) = self.get_field_value(up.loc) {
                self.gen_move_unless_check(src, up, MoveType::PawnMove, moves);
                if src.rank == 2 {
                    let up2 = move_rank(up, true).unwrap();
                    if let FieldValue(None) = self.get_field_value(up2.loc) {
                        self.gen_move_unless_check(src, up2, MoveType::PawnMove, moves);
                    }
                }
            }
        }
        for right in [true, false] {
            match move_diagonal(src, true, right) {
                Some((one, two)) => {
                    self.try_gen_pawn_capture(src, one, moves);
                    if let Some(two) = two {
                        self.try_gen_pawn_capture(src, two, moves);
                    }
                }
                None => {}
            }
        }
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
                    let field = FieldLocation::from(loc);
                    let field_a = AnnotatedFieldLocation::from_field(field);
                    match piece_type {
                        PieceType::Pawn => self.gen_moves_pawn(field, &mut moves),
                        PieceType::Knight => self.gen_moves_knight(field_a, &mut moves),
                        PieceType::Bishop => self.gen_moves_bishop(field_a, &mut moves),
                        PieceType::Rook => self.gen_moves_rook(field_a, &mut moves),
                        PieceType::Queen => self.gen_moves_queen(field_a, &mut moves),
                        PieceType::King => self.gen_moves_king(field_a, &mut moves),
                    }
                }
                _ => {}
            }
        }
        moves
    }

    pub fn get_packed_field_value(&self, field: FieldLocation) -> PackedFieldValue {
        self.board[usize::from(field)]
    }
    pub fn get_field_value(&self, field: FieldLocation) -> FieldValue {
        FieldValue::from(self.get_packed_field_value(field))
    }
}
