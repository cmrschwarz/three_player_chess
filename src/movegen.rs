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
    Promotion(PieceType)
}


#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Move{
    pub move_type: MoveType,
    pub source: FieldLocation,
    pub target: FieldLocation,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct AnnotatedFieldLocation {
    pub loc: FieldLocation,
    pub hb: Color,
    pub file: i8,
    pub rank: i8
}
impl AnnotatedFieldLocation {
    pub fn new(loc: u8, hb: Color, file: i8, rank: i8) -> AnnotatedFieldLocation{
        AnnotatedFieldLocation{
            loc: FieldLocation::from(loc),
            hb,
            file,
            rank
        }
    }
    pub fn from_field(color: Color, field: FieldLocation) -> AnnotatedFieldLocation{
        let hb = get_hb(field);
        AnnotatedFieldLocation{
            loc: field,
            hb: hb,
            file: get_file(field, color),
            rank: get_rank(field, color)
        }
    }
}

fn get_hb(field: FieldLocation) -> Color {
    Color::from_usize(usize::from(field) / HB_SIZE + 1).unwrap()
}

fn get_next_hb(color: Color, clockwise: bool) -> Color {
    let dir: i8 = if clockwise {1} else {-1};
    const CC: i8 = COLOR_COUNT as i8;
    Color::from(((u8::from(color) as i8 - 1 + CC + dir) % CC) as u8 + 1)
}

fn invert_coord(coord: i8) -> i8{
    (ROW_SIZE + 1) as i8 - coord
}
fn coord_dir(positive: bool) -> i8 {
    if positive {1i8} else {-1i8}
}

fn coord_in_bounds(coord: i8) -> bool{
    coord >= 1 && coord <= 8
}

// convenience wrapper for ternary usecases
fn adjust_coord(coord: i8, invert: bool) -> i8{
    if invert {
        invert_coord(coord)
    }
    else{
        coord
    }
}
fn get_raw_rank(field: FieldLocation) -> i8{
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

fn move_rank(
    field: AnnotatedFieldLocation, color: Color, up: bool
) -> Option<AnnotatedFieldLocation>{
    let tgt_rank = field.rank + if up {1i8} else {-1i8};
    if !coord_in_bounds(tgt_rank) { return None; }
    if (tgt_rank <= 3 || (tgt_rank == 4 && up)) || (tgt_rank > 5 || (tgt_rank == 5 && !up)) {
        return Some(AnnotatedFieldLocation::new(
            (u8::from(field.loc) as i8 + coord_dir(up == (tgt_rank <= 4)) * ROW_SIZE as i8) as u8,
            field.hb, field.file, tgt_rank
        ));
    }
    let hb = get_next_hb(color, (field.file <= 4) == up);
    let file_idx = invert_coord(field.file) - 1;
    Some(AnnotatedFieldLocation::new(
        get_hb_offset(hb) + 3 * ROW_SIZE as u8 + file_idx as u8,
        hb, field.file, tgt_rank
    ))
}

fn move_file(
    field: AnnotatedFieldLocation, color: Color, right: bool
) -> Option<AnnotatedFieldLocation> {
    let inverted = field.hb != color;
    let file = adjust_coord(field.file, inverted);
    let dir = coord_dir((field.hb == color) == right);
    let tgt_file = file + dir;
    coord_in_bounds(tgt_file).then(||
        AnnotatedFieldLocation::new(
            (u8::from(field.loc) as i8 + dir) as u8,
            field.hb, tgt_file, field.rank
        )
    )
}

fn get_field_on_next_hb(loc: u8) -> u8{
    (loc + HB_SIZE as u8) % (HB_COUNT * HB_SIZE) as u8
}

fn move_diagonal(
    field: AnnotatedFieldLocation, color: Color, up: bool, right: bool,
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
                AnnotatedFieldLocation::new(loc1, hb1, tgt_file, tgt_rank),
                Some(AnnotatedFieldLocation::new(loc2, hb2, tgt_file, tgt_rank))
            ));
        }
        let tgt_hb = get_next_hb(get_hb(field.loc), (field.file <= 4) == up);
        return Some((
            AnnotatedFieldLocation::new(
                (get_hb_offset(tgt_hb) as i8 + 3 * ROW_SIZE as i8 + invert_coord(tgt_file) - 1) as u8,
                tgt_hb, tgt_file, tgt_rank
            ),
            None
        ));
    }
    let rank_dir = coord_dir(up == (field.hb == color));
    let file_dir = coord_dir(right == (field.hb == color));
    Some((
        AnnotatedFieldLocation::new(
            (u8::from(field.loc) as i8 + rank_dir * ROW_SIZE as i8 + file_dir) as u8,
            field.hb, tgt_file, tgt_rank
        ),
        None
    ))
}

impl ThreePlayerChess {
    fn gen_slide_move(
        &self,
        src: AnnotatedFieldLocation,
        tgt: AnnotatedFieldLocation,
        moves: &mut Vec<Move>
    ) -> bool {
        match self.get_field_value(tgt.loc) {
            FieldValue(None) => {
                moves.push(Move {
                    move_type: MoveType::Slide,
                    source: src.loc,
                    target: tgt.loc
                });
            },
            FieldValue(Some((color, _))) => {
                if color != self.turn {
                    moves.push(
                        Move {move_type: MoveType::Capture, source: src.loc, target: tgt.loc }
                    );
                }
                return false;
            }
        }
        return true;
    }
    fn gen_moves_rook(&self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        let rs = ROW_SIZE as i8;
        for (length, rank, increase) in [
            (rs - field.rank, true, true), // up
            (field.rank - 1, true, false), // down
            (field.file - 1, false, false), // left
            (rs - field.file, false, true) // right
        ] {
            let mut pos = field;
            for _ in 0 .. length {
                let res = if rank {
                    move_rank(pos, self.turn, increase)
                }
                else{
                    move_file(pos, self.turn, increase)
                };
                match res {
                    Some(tgt) => {
                        if !self.gen_slide_move(pos, tgt, moves) {
                            break;
                        }
                        pos = tgt;
                    },
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
            (min(field.file, rs - field.rank), true, false), // up left
            (min(field.file, field.rank), false, false), // down left
            (min(rs - field.file, field.rank), false, true), // down right
        ] {
            let mut pos = field;
            for i in 0 .. length {
                match move_diagonal(pos, self.turn, up, right) {
                    None => break,
                    Some((one, None)) => {
                        if !self.gen_slide_move(field, one, moves) {
                            break;
                        };
                        pos = one;
                    },
                    Some((mut one, Some(mut two))) => {
                        let swap_dir = pos.hb != self.turn;
                        if swap_dir && one.hb != self.turn {
                            std::mem::swap(&mut one, &mut two);
                        }
                        if self.gen_slide_move(field, two, moves) {
                            let mut pos2 = two;
                            for _ in i .. length {
                                match move_diagonal(pos2, self.turn, up != swap_dir, right != swap_dir) {
                                    None => break,
                                    Some((one, None)) => {
                                        if !self.gen_slide_move(pos2, one, moves) {
                                            break;
                                        }
                                        pos2 = one;
                                    },
                                    _ => unreachable!(),
                                }
                            }
                        }
                        if !self.gen_slide_move(field, one, moves) {
                            break;
                        }
                        pos = one;
                    },
                }
            }
        }
    }
    fn gen_moves_knight(&self, _field: AnnotatedFieldLocation, _moves: &mut Vec<Move>) {
        unimplemented!()
    }
    fn gen_moves_king(&self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        for tgt in [
            move_file(field, self.turn, false),
            move_file(field, self.turn, true),
            move_rank(field, self.turn, false),
            move_rank(field, self.turn, false)
        ] {
            match tgt {
                Some(tgt) => { self.gen_slide_move(field, tgt, moves); },
                None => {},
            }
        }
        for right in [false, true] {
            for up in [false, true] {
                match move_diagonal(field, self.turn, up, right) {
                    Some((one, two)) => {
                        self.gen_slide_move(field, one, moves);
                        if let Some(two) = two {
                            self.gen_slide_move(field, two, moves);
                        }
                    }
                    None => {},
                }
            }
        }
    }
    fn gen_moves_pawn(&self, _field: AnnotatedFieldLocation,_moves: &mut Vec<Move>) {
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
                    let field = AnnotatedFieldLocation::from_field(self.turn, FieldLocation::from(loc));
                    match piece_type {
                            PieceType::Pawn => self.gen_moves_pawn(field, &mut moves),
                            PieceType::Knight => self.gen_moves_knight(field, &mut moves),
                            PieceType::Bishop => self.gen_moves_bishop(field, &mut moves),
                            PieceType::Rook => self.gen_moves_rook(field, &mut moves),
                            PieceType::Queen => self.gen_moves_queen(field, &mut moves),
                            PieceType::King => self.gen_moves_king(field, &mut moves),
                    }
                },
                _ => {},
            }
        }
        moves
    }
    pub fn get_field_value(&self, field: FieldLocation) -> FieldValue {
        FieldValue::from(self.board[usize::from(field)])
    }
}
