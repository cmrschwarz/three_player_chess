use crate::board::MoveType::*;
use crate::board::PieceType::*;
use crate::board::*;
use arrayvec::ArrayVec;
use std::cmp::{max, min};
use std::option::Option::*;
use MovegenResult::*;
pub const HBRC: i8 = HB_ROW_COUNT as i8;
pub const RS: i8 = ROW_SIZE as i8;

const CHECK_LINES_DIAGONALS_MAX_SQUARES: usize = 17;
const CHECK_LINES_DIAGONALS_COUNT: usize = 5;
const MAX_KNIGHT_MOVES_PER_SQUARE: usize = 10;
#[repr(packed)]
#[derive(Copy, Clone)]
pub struct MovegenOptions {
    pub captures_only: bool,
    pub only_one: bool,
}

enum MovegenResult {
    SuccessCapture,
    SuccessSlide,
    SlideButCapturesOnly,
    SameColorCollision,
    CaptureButCheck,
    SlideButCheck,
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct CheckPossibilities {
    pub knight_moves: ArrayVec<FieldLocation, MAX_KNIGHT_MOVES_PER_SQUARE>,

    pub diagonal_lines: [FieldLocation; CHECK_LINES_DIAGONALS_MAX_SQUARES],
    pub diagonal_line_ends: [usize; CHECK_LINES_DIAGONALS_COUNT],

    pub file: [FieldLocation; ROW_SIZE],
    pub rank: [FieldLocation; ROW_SIZE],
}

lazy_static! {
    static ref CHECK_POSSIBILITIES: [CheckPossibilities; BOARD_SIZE] = {
        let mut cps = ArrayVec::new();
        for i in 0..BOARD_SIZE {
            cps.push(CheckPossibilities::from_king_pos(FieldLocation::from(i)));
        }
        cps.into_inner().unwrap()
    };
}

impl Default for MovegenOptions {
    fn default() -> Self {
        MovegenOptions {
            captures_only: false,
            only_one: false,
        }
    }
}

impl MovegenResult {
    fn success(self) -> bool {
        match self {
            SuccessCapture | SuccessSlide => true,
            _ => false,
        }
    }
}

impl CheckPossibilities {
    pub fn add_cardinal_directions(&mut self, field: AnnotatedFieldLocation) {
        let mut rank_it = FieldLocation::new(field.hb, 1, field.rank);
        for i in 0..ROW_SIZE {
            self.rank[i] = rank_it;
            rank_it = FieldLocation::from(u8::from(rank_it) + 1);
        }
        self.file[0] = FieldLocation::new(field.origin, field.file, 1);
        self.file[HB_ROW_COUNT] = FieldLocation::new(
            get_next_hb(field.origin, field.file <= HBRC),
            invert_coord(field.file),
            HBRC,
        );
        for i in 1..HB_ROW_COUNT {
            self.file[i] = FieldLocation::from(u8::from(self.file[i - 1]) + ROW_SIZE as u8);
            self.file[i + HB_ROW_COUNT] =
                FieldLocation::from(u8::from(self.file[i + HB_ROW_COUNT - 1]) - ROW_SIZE as u8);
        }
    }
    pub fn add_diagonal_directions(&mut self, field: AnnotatedFieldLocation) {
        let mut lines_idx = 0;
        let mut lines_count = 0;
        let mut line_begin = 0;
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
                        self.diagonal_lines[lines_idx] = one.loc;
                        lines_idx += 1;
                        pos = one;
                    }
                    Some((mut one, Some(mut two))) => {
                        let swap_dir = pos.hb != field.origin;
                        if swap_dir && one.hb != field.origin {
                            std::mem::swap(&mut one, &mut two);
                        }
                        let line_split_point = lines_idx;
                        let mut pos2 = two;
                        self.diagonal_lines[lines_idx] = two.loc;
                        lines_idx += 1;
                        for _ in i..length {
                            match move_diagonal(pos2, up != swap_dir, right != swap_dir) {
                                None => break,
                                Some((one, None)) => {
                                    pos2 = one;
                                    self.diagonal_lines[lines_idx] = one.loc;
                                    lines_idx += 1;
                                }
                                _ => unreachable!(),
                            }
                        }
                        let line_end = lines_idx;
                        self.diagonal_line_ends[lines_count] = line_end;
                        lines_count += 1;
                        for i in line_begin..line_split_point {
                            self.diagonal_lines[lines_idx] = self.diagonal_lines[i];
                            lines_idx += 1;
                        }
                        line_begin = line_end;
                        self.diagonal_lines[lines_idx] = one.loc;
                        lines_idx += 1;
                        pos = one;
                    }
                }
            }
            self.diagonal_line_ends[lines_count] = lines_idx;
            line_begin = lines_idx;
            lines_count += 1;
        }
        if lines_count != CHECK_LINES_DIAGONALS_COUNT {
            debug_assert!(lines_count + 1 == CHECK_LINES_DIAGONALS_COUNT);
            self.diagonal_line_ends[lines_count] = self.diagonal_line_ends[lines_count - 1];
        }
    }
    pub fn add_knight_moves(&mut self, field: AnnotatedFieldLocation) {
        get_knight_moves_for_field(field, &mut self.knight_moves);
    }
    pub fn new() -> Self {
        Self {
            knight_moves: Default::default(),
            diagonal_lines: Default::default(),
            diagonal_line_ends: Default::default(),
            file: Default::default(),
            rank: Default::default(),
        }
    }
    pub fn from_king_pos(king_pos: FieldLocation) -> CheckPossibilities {
        let mut cp = CheckPossibilities::default();
        let afl = AnnotatedFieldLocation::from(king_pos);
        cp.add_knight_moves(afl);
        cp.add_cardinal_directions(afl);
        cp.add_diagonal_directions(afl);
        cp
    }
}
pub fn get_castling_target(color: Color, king: bool, short: bool) -> FieldLocation {
    let file = if king {
        [3, 7][short as usize]
    } else {
        [4, 6][short as usize]
    };
    FieldLocation::new(color, file, 1)
}

impl AnnotatedFieldLocation {
    pub fn new(origin: Color, loc: FieldLocation, hb: Color, file: i8, rank: i8) -> Self {
        AnnotatedFieldLocation {
            origin,
            loc,
            hb,
            file,
            rank,
        }
    }
    pub fn from_field_with_origin_and_hb(origin: Color, hb: Color, field: FieldLocation) -> Self {
        debug_assert!(hb == field.hb());
        AnnotatedFieldLocation {
            origin,
            loc: field,
            hb,
            file: get_file(field, origin),
            rank: get_rank(field, origin),
        }
    }
    pub fn from_file_and_rank(origin: Color, hb: Color, file: i8, rank: i8) -> Self {
        AnnotatedFieldLocation {
            origin,
            loc: FieldLocation::new(
                hb,
                adjust_coord(file, hb != origin),
                adjust_coord(rank, hb != origin),
            ),
            hb,
            file,
            rank,
        }
    }
    pub fn from_with_origin(origin: Color, field: FieldLocation) -> Self {
        Self::from_field_with_origin_and_hb(origin, field.hb(), field)
    }
}

impl From<FieldLocation> for AnnotatedFieldLocation {
    fn from(field: FieldLocation) -> Self {
        let hb = field.hb();
        Self::from_field_with_origin_and_hb(hb, hb, field)
    }
}

pub fn get_next_hb(color: Color, clockwise: bool) -> Color {
    let dir: i8 = if clockwise { 1 } else { -1 };
    const CC: i8 = COLOR_COUNT as i8;
    Color::from(((u8::from(color) as i8 + CC + dir) % CC) as u8)
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
pub fn adjust_coord(coord: i8, invert: bool) -> i8 {
    if invert {
        invert_coord(coord)
    } else {
        coord
    }
}
pub fn get_raw_rank(field: FieldLocation) -> i8 {
    ((u8::from(field) % HB_SIZE as u8) / RS as u8 + 1) as i8
}
pub fn get_rank(field: FieldLocation, color: Color) -> i8 {
    adjust_coord(get_raw_rank(field), field.hb() != color)
}

pub fn get_raw_file(field: FieldLocation) -> i8 {
    (u8::from(field) % RS as u8 + 1) as i8
}
pub fn get_file(field: FieldLocation, color: Color) -> i8 {
    adjust_coord(get_raw_file(field), field.hb() != color)
}

pub fn move_rank(field: AnnotatedFieldLocation, up: bool) -> Option<AnnotatedFieldLocation> {
    let tgt_rank = field.rank + coord_dir(up);
    if !coord_in_bounds(tgt_rank) {
        return None;
    }
    if (tgt_rank < HBRC || (tgt_rank == HBRC && up))
        || (tgt_rank > HBRC + 1 || (tgt_rank == HBRC + 1 && !up))
    {
        return Some(AnnotatedFieldLocation::new(
            field.origin,
            FieldLocation::from(
                (u8::from(field.loc) as i8 + coord_dir(up == (tgt_rank <= HBRC)) * RS) as u8,
            ),
            field.hb,
            field.file,
            tgt_rank,
        ));
    }
    let hb = get_next_hb(field.hb, (field.file <= HBRC) == up);
    // we don't have to support changing rank across the top center line,
    // as AFLs for rook and queen moves get the origin of the field they
    // are on. this way they don't have to change direction
    debug_assert!(hb == field.origin || field.hb == field.origin);
    let file = adjust_coord(field.file, field.origin != hb);
    Some(AnnotatedFieldLocation::new(
        field.origin,
        // the *physical* target rank will always be the top on an hb change
        FieldLocation::new(hb, file, HBRC),
        hb,
        field.file,
        tgt_rank,
    ))
}

pub fn move_file(field: AnnotatedFieldLocation, right: bool) -> Option<AnnotatedFieldLocation> {
    let inverted = field.hb != field.origin;
    let tgt_file = field.file + coord_dir(right);
    let dir_raw = coord_dir(inverted != right);
    coord_in_bounds(tgt_file).then(|| {
        AnnotatedFieldLocation::new(
            field.origin,
            FieldLocation::from((u8::from(field.loc) as i8 + dir_raw) as u8),
            field.hb,
            tgt_file,
            field.rank,
        )
    })
}

fn get_knight_moves_for_field(
    field: AnnotatedFieldLocation,
    moves: &mut arrayvec::ArrayVec<FieldLocation, MAX_KNIGHT_MOVES_PER_SQUARE>,
) {
    // handles the (up to 8) moves from
    // moving left / right first (by one or two squares)
    for right in [true, false] {
        if let Some(r1) = move_file(field, right) {
            for up in [true, false] {
                if let Some(r1u1) = move_rank(r1, up) {
                    move_rank(r1u1, up).map(|m| moves.push(m.loc));
                }
            }
            if let Some(r2) = move_file(r1, right) {
                for up in [true, false] {
                    move_rank(r2, up).map(|m| moves.push(m.loc));
                }
            }
        }
    }
    // handles the additional (up to three, but only in the center square)
    // moves originating from moving up
    // excluding the ones already covered
    // because knight moves are calculated with the origin being set
    // to the starting board, only moving up can change the board
    debug_assert!(field.hb == field.origin);
    // if we are on the left hb, the new moves are up right
    let move_right = field.file <= HBRC;
    let hb_file = if move_right {
        field.file
    } else {
        RS - field.file + 1
    };
    if (field.rank == HBRC && hb_file >= HBRC - 1) || (field.rank == HBRC - 1 && hb_file == HBRC) {
        //these must exist because of the rank check above
        let u1 = move_rank(field, true).unwrap();
        let u2 = move_rank(u1, true).unwrap();

        if field.rank == HBRC {
            if coord_in_bounds(field.file + 2 * coord_dir(move_right)) {
                let u1r1 = move_file(u1, move_right).unwrap();
                let u1r2 = move_file(u1r1, move_right).unwrap();
                moves.push(u1r2.loc);
            }
        }
        moves.push(move_file(u2, move_right).unwrap().loc);
    }
}

pub fn get_field_on_next_hb(loc: FieldLocation) -> FieldLocation {
    FieldLocation::from((u8::from(loc) + HB_SIZE as u8) % (HB_COUNT * HB_SIZE) as u8)
}

fn move_diagonal(
    field: AnnotatedFieldLocation,
    up: bool,
    right: bool,
) -> Option<(AnnotatedFieldLocation, Option<AnnotatedFieldLocation>)> {
    let rank_dir = coord_dir(up);
    let file_dir = coord_dir(right);
    let tgt_rank = field.rank + rank_dir;
    let tgt_file = field.file + file_dir;
    if !coord_in_bounds(tgt_file) || !coord_in_bounds(tgt_rank) {
        return None;
    }
    // moving from rank 4 to 5 or vice versa is the only way to change boards
    if (tgt_rank == HBRC + 1 && up) || (tgt_rank == HBRC && !up) {
        if (tgt_file == HBRC + 1 && right) || (tgt_file == HBRC && !right) {
            let hb1 = get_next_hb(field.hb, true);
            let hb2 = get_next_hb(hb1, true);
            let loc1 = get_field_on_next_hb(field.loc);
            let loc2 = get_field_on_next_hb(loc1);
            let tgt_rank_1 = adjust_coord(HBRC, hb1 != field.origin);
            let tgt_rank_2 = adjust_coord(HBRC, hb2 != field.origin);
            return Some((
                AnnotatedFieldLocation::new(field.origin, loc1, hb1, tgt_file, tgt_rank_1),
                Some(AnnotatedFieldLocation::new(
                    field.origin,
                    loc2,
                    hb2,
                    tgt_file,
                    tgt_rank_2,
                )),
            ));
        }
        let tgt_hb = get_next_hb(field.hb, (field.file <= HBRC) == up);
        return Some((
            AnnotatedFieldLocation::new(
                field.origin,
                FieldLocation::new(tgt_hb, invert_coord(tgt_file), HBRC),
                tgt_hb,
                tgt_file,
                tgt_rank,
            ),
            None,
        ));
    }
    let inverted = field.hb != field.origin;
    Some((
        AnnotatedFieldLocation::new(
            field.origin,
            FieldLocation::new(
                field.hb,
                adjust_coord(field.file, inverted) + coord_dir(right != inverted),
                adjust_coord(field.rank, inverted) + coord_dir(up != inverted),
            ),
            field.hb,
            tgt_file,
            tgt_rank,
        ),
        None,
    ))
}

impl ThreePlayerChess {
    #[cfg(not(feature = "debug_movegen"))]
    fn movegen_sanity_check(&self, _rm: &ReversableMove, _before: bool, _revert: bool) {}

    #[cfg(feature = "debug_movegen")]
    fn movegen_sanity_check(&self, rm: &ReversableMove, before: bool, revert: bool) {
        let state_str = self.state_string();
        let expected_state = if before != revert {
            &rm.state_before
        } else {
            &rm.state_after
        };
        let constex_str_before = ["after", "before"][usize::from(before)];
        let constex_str_revert = ["move", "move reversal"][usize::from(revert)];
        if state_str != *expected_state {
            println!(
                "state missmatch {} {}!:\nexpected: {}\ngot     : {}",
                constex_str_before, constex_str_revert, expected_state, state_str
            );
            println!("move: {:?}", rm.mov);
            assert!(false);
        }
        if self.zobrist_hash.value != crate::zobrist::ZobristHash::new(self).value {
            println!(
                "zobrist missmatch {} {}!: expected: {}\ngot     : {}",
                constex_str_before,
                constex_str_revert,
                crate::zobrist::ZobristHash::new(self).value,
                self.zobrist_hash.value
            );
            println!("state bef: {}", rm.state_before);
            println!("state aft: {}", rm.state_after);
            println!("move: {:?}", rm.mov);
            assert!(false);
        }
    }
    pub fn perform_reversable_move(&mut self, rm: &ReversableMove) {
        self.movegen_sanity_check(rm, true, false);
        self.apply_move(rm.mov);
        self.apply_move_sideeffects(rm.mov);
        self.movegen_sanity_check(rm, false, false);
    }
    pub fn perform_move(&mut self, m: Move) {
        self.apply_move(m);
        self.apply_move_sideeffects(m);
    }
    pub fn revert_move(&mut self, rm: &ReversableMove) {
        self.movegen_sanity_check(rm, true, true);
        self.unapply_move_sideffects(rm);
        self.unapply_move(rm.mov);
        self.movegen_sanity_check(rm, false, true);
    }
    pub fn apply_move(&mut self, m: Move) {
        let src = usize::from(m.source);
        let tgt = usize::from(m.target);
        match m.move_type {
            Slide | SlideClaimDraw(_) | Capture(_) => {
                self.board[tgt] = self.board[src];
                self.board[src] = Default::default();
            }
            EnPassant(_, captured_pawn_loc) => {
                self.board[tgt] = self.board[src];
                self.board[usize::from(captured_pawn_loc)] = Default::default();
                self.board[src] = Default::default();
            }
            Castle(short) => {
                let rook_source = self.possible_rooks_for_castling[usize::from(self.turn)]
                    [usize::from(short)]
                .unwrap();
                let rook_target = get_castling_target(self.turn, false, short);
                let rook = self.get_packed_field_value(rook_source);
                let king = self.board[src];
                self.board[src] = Default::default();
                self.board[usize::from(rook_source)] = Default::default();
                self.board[usize::from(rook_target)] = rook;
                self.board[tgt] = king;
            }
            Promotion(piece_type) | CapturePromotion(_, piece_type) => {
                self.board[tgt] = FieldValue(Some((self.turn, piece_type))).into();
                self.board[src] = Default::default();
            }
            ClaimDraw(_) => return,
        }
    }
    pub fn apply_king_move_sideeffects(&mut self, m: Move) {
        self.king_positions[usize::from(self.turn)] = m.target;
        for r in self.possible_rooks_for_castling[usize::from(self.turn)].iter_mut() {
            if let Some(loc) = *r {
                *r = None;
                self.zobrist_hash.toggle_possible_castling_rook(loc);
            }
        }
    }
    pub fn remove_castling_rights_from_rook(&mut self, loc: FieldLocation, color: Color) {
        for r in self.possible_rooks_for_castling[usize::from(color)].iter_mut() {
            if *r == Some(loc) {
                *r = None;
                self.zobrist_hash.toggle_possible_castling_rook(loc);
            }
        }
    }
    pub fn remove_en_passent_target(&mut self, loc: FieldLocation, color: Color) {
        let ci = usize::from(color);
        if self.possible_en_passant[ci] == Some(loc) {
            debug_assert!(loc.hb() == color);
            self.possible_en_passant[ci] = None;
            self.zobrist_hash.toggle_en_passent_square(loc);
        }
    }
    fn apply_slide_sideeffects(&mut self, m: Move) {
        let field_value = self.get_field_value(m.target);
        self.zobrist_hash.toggle_square(m.source, field_value);
        self.zobrist_hash.toggle_square(m.target, field_value);
        let (_, piece) = field_value.unwrap();
        match piece {
            PieceType::King => {
                self.apply_king_move_sideeffects(m);
                self.zobrist_hash.fifty_move_rule_move_inc(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
            }
            PieceType::Pawn => {
                self.zobrist_hash.fifty_move_rule_move_reset(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
                self.last_capture_or_pawn_move_index = self.move_index;
                let source = AnnotatedFieldLocation::from_with_origin(self.turn, m.source);
                let target = AnnotatedFieldLocation::from_with_origin(self.turn, m.target);
                if source.rank == 2 && target.rank == 4 {
                    let loc = move_rank(source, true).unwrap().loc;
                    self.possible_en_passant[usize::from(self.turn)] = Some(loc);
                    self.zobrist_hash.toggle_en_passent_square(loc);
                }
            }
            PieceType::Rook => {
                self.zobrist_hash.fifty_move_rule_move_inc(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
                self.remove_castling_rights_from_rook(m.source, self.turn);
            }
            _ => {
                self.zobrist_hash.fifty_move_rule_move_inc(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
            }
        }
    }
    pub fn apply_move_sideeffects(&mut self, m: Move) {
        self.move_index += 1;
        let ep = &mut self.possible_en_passant[usize::from(self.turn)];
        if let Some(loc) = *ep {
            self.zobrist_hash.toggle_en_passent_square(loc);
            *ep = None;
        }
        match m.move_type {
            Slide => self.apply_slide_sideeffects(m),
            SlideClaimDraw(draw_claim_basis) => {
                self.apply_slide_sideeffects(m);
                self.game_status = GameStatus::Draw(DrawReason::DrawClaimed(draw_claim_basis));
            }
            Castle(short) => {
                let king = FieldValue(Some((self.turn, King)));
                self.zobrist_hash.toggle_square(m.source, king);
                self.zobrist_hash.toggle_square(m.target, king);

                let rook = FieldValue(Some((self.turn, Rook)));
                let rook_source = self.possible_rooks_for_castling[usize::from(self.turn)]
                    [usize::from(short)]
                .unwrap();
                let rook_target = get_castling_target(self.turn, false, short);
                self.zobrist_hash.toggle_square(rook_source, rook);
                self.zobrist_hash.toggle_square(rook_target, rook);

                self.zobrist_hash.fifty_move_rule_move_inc(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
                self.apply_king_move_sideeffects(m);
            }
            EnPassant(captured_pawn, capture_loc) => {
                let pawn = FieldValue::from(captured_pawn);
                let capturer = self.get_field_value(m.target);
                self.zobrist_hash.toggle_square(m.source, capturer);
                self.zobrist_hash.toggle_square(m.target, capturer);
                self.zobrist_hash.toggle_square(capture_loc, pawn);
                self.zobrist_hash.fifty_move_rule_move_reset(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
                self.last_capture_or_pawn_move_index = self.move_index;
                let ep_square = self.possible_en_passant[usize::from(pawn.color().unwrap())]
                    .take()
                    .unwrap();
                self.zobrist_hash.toggle_en_passent_square(ep_square);
            }
            ClaimDraw(draw_claim_basis) => {
                self.game_status = GameStatus::Draw(DrawReason::DrawClaimed(draw_claim_basis));
            }
            Capture(field_val) | CapturePromotion(field_val, _) => {
                let (_, piece) = self.get_field_value(m.target).unwrap();
                let capturer = self.get_field_value(m.target);
                let capturer_origininal = if let CapturePromotion(_, _) = m.move_type {
                    FieldValue(Some((self.turn, Pawn)))
                } else {
                    capturer
                };
                self.zobrist_hash
                    .toggle_square(m.source, capturer_origininal);
                self.zobrist_hash.toggle_square(m.target, capturer);
                self.zobrist_hash.toggle_square(m.target, field_val.into());
                self.zobrist_hash.fifty_move_rule_move_reset(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
                self.last_capture_or_pawn_move_index = self.move_index;
                if piece == PieceType::King {
                    self.apply_king_move_sideeffects(m);
                } else if piece == PieceType::Rook {
                    self.remove_castling_rights_from_rook(m.source, self.turn);
                }
                let (color, captured_piece) = FieldValue::from(field_val).unwrap();
                if captured_piece == PieceType::Rook {
                    self.remove_castling_rights_from_rook(m.target, color);
                } else if captured_piece == PieceType::Pawn {
                    let ci = usize::from(color);
                    let tgt_loc = AnnotatedFieldLocation::from_with_origin(color, m.target);
                    if tgt_loc.rank == 4 {
                        let ep_target = move_rank(tgt_loc, false).unwrap().loc;
                        if self.possible_en_passant[ci] == Some(ep_target) {
                            self.possible_en_passant[ci] = None;
                            self.zobrist_hash.toggle_en_passent_square(ep_target);
                        }
                    }
                }
            }
            Promotion(piece_type) => {
                self.zobrist_hash
                    .toggle_square(m.source, FieldValue(Some((self.turn, Pawn))));
                self.zobrist_hash
                    .toggle_square(m.target, FieldValue(Some((self.turn, piece_type))));
                self.zobrist_hash.fifty_move_rule_move_reset(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
                self.last_capture_or_pawn_move_index = self.move_index;
            }
        }
        self.turn = get_next_hb(self.turn, true);
        self.zobrist_hash.next_turn(self.turn);
        self.game_status = self.game_status();
    }
    pub fn unapply_move(&mut self, m: Move) {
        let src = usize::from(m.source);
        let tgt = usize::from(m.target);
        match m.move_type {
            Slide | SlideClaimDraw(_) => {
                self.board[src] = self.board[tgt];
                self.board[tgt] = Default::default();
            }
            Capture(captured_piece) => {
                self.board[src] = self.board[tgt];
                self.board[tgt] = captured_piece;
            }
            CapturePromotion(captured_piece, _) => {
                self.board[src] = FieldValue(Some((self.turn, PieceType::Pawn))).into();
                self.board[tgt] = captured_piece;
            }
            EnPassant(captured_pawn, captured_pawn_loc) => {
                assert!(FieldValue::from(captured_pawn).is_some());
                self.board[usize::from(captured_pawn_loc)] = captured_pawn;
                self.board[src] = self.board[tgt];
                self.board[tgt] = Default::default();
            }
            Castle(short) => {
                let rook_source = self.possible_rooks_for_castling[usize::from(self.turn)]
                    [usize::from(short)]
                .unwrap();
                let rook_target = get_castling_target(self.turn, false, short);
                let rook = self.get_packed_field_value(rook_target);
                let king = self.board[tgt];
                self.board[tgt] = Default::default();
                self.board[usize::from(rook_target)] = Default::default();
                self.board[src] = king;
                self.board[usize::from(rook_source)] = rook;
            }
            Promotion(_) => {
                self.board[tgt] = Default::default();
                self.board[src] = FieldValue(Some((self.turn, PieceType::Pawn))).into();
            }
            ClaimDraw(_) => return,
        }
    }
    pub fn unapply_move_sideffects(&mut self, rm: &ReversableMove) {
        self.turn = get_next_hb(self.turn, false);
        self.game_status = GameStatus::Ongoing;
        self.move_index -= 1;
        self.last_capture_or_pawn_move_index = rm.last_capture_or_pawn_move_index;
        debug_assert!(self.move_index >= rm.last_capture_or_pawn_move_index);

        self.possible_rooks_for_castling = rm.possible_rooks_for_castling;
        self.possible_en_passant = rm.possible_en_passant;
        self.zobrist_hash.value = rm.zobrist_hash_value;

        if let ClaimDraw(_) = rm.mov.move_type {
            return;
        }
        if FieldValue::from(self.board[usize::from(rm.mov.target)])
            .piece_type()
            .unwrap()
            == King
        {
            self.king_positions[usize::from(self.turn)] = rm.mov.source;
        }
    }
    fn would_king_move_bypass_check(&mut self, mv: Move) -> bool {
        self.apply_move(mv);
        let would_be_check = self
            .is_piece_capturable_at(mv.target, None, false)
            .is_some();
        self.unapply_move(mv);
        would_be_check
    }
    fn would_non_king_move_bypass_check(&mut self, mv: Move) -> bool {
        self.apply_move(mv);
        let would_be_check = self.is_king_capturable(None);
        self.unapply_move(mv);
        would_be_check
    }
    fn append_non_king_move_unless_check(&mut self, mv: Move, moves: &mut Vec<Move>) -> bool {
        if !self.would_non_king_move_bypass_check(mv) {
            moves.push(mv);
            true
        } else {
            false
        }
    }
    fn gen_non_king_move_unless_check(
        &mut self,
        src: FieldLocation,
        tgt: FieldLocation,
        move_type: MoveType,
        moves: &mut Vec<Move>,
    ) -> bool {
        let m = Move {
            move_type,
            source: src,
            target: tgt,
        };
        self.append_non_king_move_unless_check(m, moves)
    }
    pub fn is_piece_capturable_at(
        &mut self,
        loc: FieldLocation,
        capturing_color: Option<Color>,
        check_for_checks: bool,
    ) -> Option<Move> {
        //checking for checks only makes sense for the current player
        debug_assert!(!check_for_checks || capturing_color == Some(self.turn));
        let field_value = self.board[usize::from(loc)];
        let (piece_color, piece_type) = FieldValue::from(field_value).unwrap();
        fn color_may_capture(
            col: Color,
            piece_color: Color,
            capturing_color: Option<Color>,
        ) -> bool {
            col != piece_color && capturing_color.map(|cc| cc == col).unwrap_or(true)
        }
        let kp = AnnotatedFieldLocation::from(loc);
        let cp = &CHECK_POSSIBILITIES[usize::from(loc)];
        for (axis, start, end, dir) in [
            (&cp.file, kp.rank - 1, 0, -1i8),
            (&cp.file, kp.rank + 1, RS + 1, 1),
            (&cp.rank, kp.file - 1, 0, -1),
            (&cp.rank, kp.file + 1, RS + 1, 1),
        ] {
            let mut i = start;
            while i != end {
                let f = axis[i as usize - 1];
                match *self.get_field_value(f) {
                    Some((color, piece_type)) => match piece_type {
                        PieceType::Rook | PieceType::Queen
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            let m = Move::new(f, loc, Capture(field_value));
                            if !check_for_checks || !self.would_non_king_move_bypass_check(m) {
                                return Some(m);
                            }
                            break;
                        }
                        PieceType::King
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            if i == start {
                                let m = Move::new(f, loc, Capture(field_value));
                                if !check_for_checks || !self.would_king_move_bypass_check(m) {
                                    return Some(m);
                                }
                            }
                            break;
                        }
                        _ => break,
                    },
                    None => (),
                }
                i += dir;
            }
        }
        let mut line_start = 0;
        for line_end in cp.diagonal_line_ends {
            for (i, f) in cp.diagonal_lines[line_start..line_end].iter().enumerate() {
                let board_val = self.board[usize::from(*f)];
                match *FieldValue::from(board_val) {
                    None => continue,
                    Some((color, piece_type)) => match piece_type {
                        PieceType::Bishop | PieceType::Queen
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            let m = Move::new(*f, loc, Capture(field_value));
                            if !check_for_checks || !self.would_non_king_move_bypass_check(m) {
                                return Some(m);
                            }
                            break;
                        }
                        PieceType::King
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            if i == 0 {
                                let m = Move::new(*f, loc, Capture(field_value));
                                if !check_for_checks || !self.would_king_move_bypass_check(m) {
                                    return Some(m);
                                }
                            }
                            break;
                        }
                        PieceType::Pawn
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            let field_pos = AnnotatedFieldLocation::from_with_origin(color, kp.loc);
                            let pawn_pos = AnnotatedFieldLocation::from_with_origin(color, *f);
                            if i == 0 && pawn_pos.rank + 1 == field_pos.rank {
                                let m = Move::new(*f, loc, Capture(field_value));
                                if !check_for_checks || !self.would_non_king_move_bypass_check(m) {
                                    return Some(m);
                                }
                            }
                            break;
                        }
                        _ => break,
                    },
                }
            }
            line_start = line_end;
        }
        for f in cp.knight_moves.iter() {
            let board_val = self.board[usize::from(*f)];
            match *FieldValue::from(board_val) {
                None => continue,
                Some((color, piece_type)) => match piece_type {
                    PieceType::Knight if color_may_capture(color, piece_color, capturing_color) => {
                        let m = Move::new(*f, loc, Capture(field_value));
                        if !check_for_checks || !self.would_non_king_move_bypass_check(m) {
                            return Some(m);
                        }
                    }
                    _ => continue,
                },
            }
        }
        if piece_type == PieceType::Pawn {
            if let Some(ep_square) = self.possible_en_passant[usize::from(piece_color)] {
                if move_rank(AnnotatedFieldLocation::from(ep_square), true)
                    .unwrap()
                    .loc
                    == kp.loc
                {
                    for f in [-1, 1] {
                        if coord_in_bounds(kp.file + f) {
                            let src_loc = FieldLocation::new(kp.hb, kp.file + f, kp.rank);
                            if let Some((color, piece_type)) =
                                *FieldValue::from(self.board[usize::from(src_loc)])
                            {
                                if piece_type == PieceType::Pawn
                                    && color_may_capture(color, piece_color, capturing_color)
                                {
                                    let tgt_loc = FieldLocation::new(kp.hb, kp.file, kp.rank - 1);
                                    let m =
                                        Move::new(src_loc, tgt_loc, EnPassant(field_value, loc));
                                    if !check_for_checks
                                        || !self.would_non_king_move_bypass_check(m)
                                    {
                                        return Some(m);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }
    pub fn is_king_capturable(&mut self, capturing_color: Option<Color>) -> bool {
        let kp = self.king_positions[usize::from(self.turn)];
        self.is_piece_capturable_at(kp, capturing_color, false)
            .is_some()
    }
    pub fn game_status(&mut self) -> GameStatus {
        if self.game_status != GameStatus::Ongoing {
            return self.game_status;
        }
        // check whether the current player has a legal move (doesn't disregard check)
        // that would capture the third player's king
        // (piece of previous player moved out of the way uncovering the king). in that case the current player wins
        let next_player = get_next_hb(self.turn, true);
        if self
            .is_piece_capturable_at(
                self.king_positions[usize::from(next_player)],
                Some(self.turn),
                true,
            )
            .is_some()
        {
            return GameStatus::Win(self.turn, WinReason::CapturableKing(next_player));
        }
        // check whether the current player has no move to get out of check
        // which would mean somebody won
        let opts = MovegenOptions {
            captures_only: false,
            only_one: true,
        };
        let mut dv = self.dummy_vec.take().unwrap();
        self.gen_moves_with_options(&mut dv, opts);
        let no_legal_moves = dv.is_empty();
        dv.clear();
        self.dummy_vec = Some(dv);

        if no_legal_moves {
            if self.is_king_capturable(None) {
                let winner = if self.is_king_capturable(Some(next_player)) {
                    next_player
                } else {
                    get_next_hb(next_player, true)
                };
                GameStatus::Win(winner, WinReason::Checkmate(self.turn))
            } else {
                GameStatus::Draw(DrawReason::Stalemate(self.turn))
            }
        } else {
            GameStatus::Ongoing
        }
    }

    fn gen_non_king_slide_move(
        &mut self,
        src: FieldLocation,
        tgt: FieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) -> MovegenResult {
        let piece_value = self.get_packed_field_value(tgt);
        match FieldValue::from(piece_value) {
            FieldValue(None) => {
                if opts.captures_only {
                    SlideButCapturesOnly
                } else if self.gen_non_king_move_unless_check(src, tgt, Slide, moves) {
                    SuccessSlide
                } else {
                    SlideButCheck
                }
            }
            FieldValue(Some((color, _))) if color != self.turn => {
                let cap = Capture(piece_value);
                if self.gen_non_king_move_unless_check(src, tgt, cap, moves) {
                    SuccessCapture
                } else {
                    CaptureButCheck
                }
            }
            _ => MovegenResult::SameColorCollision,
        }
    }

    fn gen_moves_rook(
        &mut self,
        field: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) {
        // we want to avoid having to change directions when attempting
        // to move up/down(!) across the upper center line
        // therefore we use the native origin of the starting field
        // so this can't happen
        debug_assert!(field.origin == field.hb);
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
                        match self.gen_non_king_slide_move(field.loc, tgt.loc, moves, opts) {
                            SuccessCapture | SuccessSlide if opts.only_one => return,
                            SuccessCapture | CaptureButCheck | SameColorCollision => break,
                            SlideButCheck | SuccessSlide | SlideButCapturesOnly => {}
                        }
                        pos = tgt;
                    }
                    _ => break,
                }
            }
        }
    }
    fn gen_moves_bishop(
        &mut self,
        field: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) {
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
                        match self.gen_non_king_slide_move(field.loc, one.loc, moves, opts) {
                            SuccessCapture | SuccessSlide if opts.only_one => return,
                            SuccessCapture | CaptureButCheck | SameColorCollision => break,
                            SlideButCheck | SuccessSlide | SlideButCapturesOnly => {}
                        };
                        pos = one;
                    }
                    Some((mut one, Some(mut two))) => {
                        let swap_dir = pos.hb != field.origin;
                        if swap_dir && one.hb != field.origin {
                            // to make sure that one doesn't have to swap directions
                            // we come here because 'pos' and 'one' are both not the origin
                            // on such a transition we need to change the order, which we want
                            // to avoid for the outer loop
                            std::mem::swap(&mut one, &mut two);
                        }
                        match self.gen_non_king_slide_move(field.loc, two.loc, moves, opts) {
                            SuccessCapture | SuccessSlide if opts.only_one => return,
                            SuccessCapture | CaptureButCheck | SameColorCollision => break,
                            SlideButCheck | SuccessSlide | SlideButCapturesOnly => {
                                let mut pos2 = two;
                                for _ in i..length {
                                    match move_diagonal(pos2, up != swap_dir, right != swap_dir) {
                                        None => break,
                                        Some((one, None)) => {
                                            match self.gen_non_king_slide_move(
                                                field.loc, one.loc, moves, opts,
                                            ) {
                                                SuccessCapture | SuccessSlide if opts.only_one => {
                                                    return
                                                }
                                                SuccessCapture | CaptureButCheck
                                                | SameColorCollision => break,
                                                SlideButCheck | SuccessSlide
                                                | SlideButCapturesOnly => {}
                                            };
                                            pos2 = one;
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                            }
                        }
                        match self.gen_non_king_slide_move(field.loc, one.loc, moves, opts) {
                            SuccessCapture | SuccessSlide if opts.only_one => return,
                            SuccessCapture | CaptureButCheck | SameColorCollision => break,
                            SlideButCheck | SuccessSlide | SlideButCapturesOnly => {}
                        };
                        pos = one;
                    }
                }
            }
        }
    }
    fn gen_moves_knight(
        &mut self,
        field: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) {
        let mut knight_moves = arrayvec::ArrayVec::new();
        get_knight_moves_for_field(field, &mut knight_moves);
        for m in knight_moves {
            let mt = self.gen_non_king_slide_move(field.loc, m, moves, opts);
            if opts.only_one && mt.success() {
                return;
            };
        }
    }
    pub fn gen_move_castling(&mut self, short: bool) -> Option<Move> {
        let hb = self.turn;
        let king_src = AnnotatedFieldLocation::from(self.king_positions[usize::from(hb)]);
        let rook_src = AnnotatedFieldLocation::from(
            self.possible_rooks_for_castling[usize::from(hb)][short as usize]?,
        );
        let king_tgt = AnnotatedFieldLocation::from(get_castling_target(hb, true, short));
        let rook_tgt = AnnotatedFieldLocation::from(get_castling_target(hb, false, short));

        let king_val = self.get_packed_field_value(king_src.loc);
        let rook_val = self.get_packed_field_value(rook_src.loc);

        if self.is_king_capturable(None) {
            return None;
        }

        self.board[usize::from(king_src.loc)] = FieldValue(None).into();
        self.board[usize::from(rook_src.loc)] = FieldValue(None).into();
        for i in min(king_src.file, rook_src.file)..max(king_src.file, rook_src.file) + 1 {
            if self.get_field_value(FieldLocation::new(hb, i, 1)).is_some() {
                self.board[usize::from(king_src.loc)] = king_val;
                self.board[usize::from(rook_src.loc)] = rook_val;
                return None;
            }
        }
        let mut conflict = false;
        self.board[usize::from(rook_tgt.loc)] = rook_val;
        let (fbegin, fend) = [
            (king_tgt.file, king_src.file),
            (king_src.file + 1, king_tgt.file + 1),
        ][short as usize];
        for f in fbegin..fend {
            if conflict {
                break;
            }
            let kp = AnnotatedFieldLocation::from_file_and_rank(hb, hb, f, 1);
            self.board[usize::from(kp.loc)] = king_val;
            conflict = self.is_piece_capturable_at(kp.loc, None, false).is_some();
            self.board[usize::from(kp.loc)] = FieldValue(None).into();
        }
        self.board[usize::from(king_src.loc)] = king_val;
        self.board[usize::from(rook_src.loc)] = rook_val;
        self.board[usize::from(rook_tgt.loc)] = FieldValue(None).into();
        if conflict {
            return None;
        }
        Some(Move {
            move_type: Castle(short),
            source: king_src.loc,
            target: king_tgt.loc,
        })
    }
    fn gen_king_slide_unless_check(
        &mut self,
        src: FieldLocation,
        tgt: FieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) -> bool {
        let move_type;
        let piece_val = self.board[usize::from(tgt)];
        if let Some((color, _)) = *FieldValue::from(piece_val) {
            if color == self.turn {
                return false;
            }
            move_type = Capture(piece_val);
        } else {
            if opts.captures_only {
                return false;
            }
            move_type = Slide;
        }
        let mov = Move {
            move_type,
            source: src,
            target: tgt,
        };
        //cant use would_move_bypass_check because of the different king pos
        self.apply_move(mov);
        let would_be_check = self.is_piece_capturable_at(tgt, None, false).is_some();
        self.unapply_move(mov);
        if would_be_check {
            false
        } else {
            moves.push(mov);
            true
        }
    }
    fn gen_moves_king(
        &mut self,
        field: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) {
        for tgt in [
            move_rank(field, true),
            move_rank(field, false),
            move_file(field, true),
            move_file(field, false),
        ] {
            if let Some(tgt) = tgt {
                if self.gen_king_slide_unless_check(field.loc, tgt.loc, moves, opts)
                    && opts.only_one
                {
                    return;
                }
            }
        }
        for right in [true, false] {
            for up in [true, false] {
                match move_diagonal(field, up, right) {
                    Some((one, two)) => {
                        for tgt in [Some(one), two] {
                            if let Some(tgt) = tgt {
                                if self.gen_king_slide_unless_check(field.loc, tgt.loc, moves, opts)
                                    && opts.only_one
                                {
                                    return;
                                }
                            };
                        }
                    }
                    None => {}
                }
            }
        }
        for castle in [self.gen_move_castling(false), self.gen_move_castling(true)] {
            if let Some(m) = castle {
                moves.push(m);
                if opts.only_one {
                    return;
                }
            }
        }
    }
    fn try_gen_pawn_capture(
        &mut self,
        src: AnnotatedFieldLocation,
        tgt: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) -> bool {
        let piece_value = self.get_packed_field_value(tgt.loc);
        match FieldValue::from(piece_value) {
            FieldValue(Some((color, _))) if color != self.turn => {
                if tgt.rank == RS {
                    let mut mov = Move {
                        source: src.loc,
                        target: tgt.loc,
                        move_type: CapturePromotion(piece_value, Queen),
                    };
                    if !self.would_non_king_move_bypass_check(mov) {
                        for promotion_piece in [Queen, Bishop, Knight, Rook] {
                            mov.move_type = CapturePromotion(piece_value, promotion_piece);
                            moves.push(mov);
                            if opts.only_one {
                                return true;
                            }
                        }
                        true
                    } else {
                        false
                    }
                } else {
                    self.gen_non_king_move_unless_check(
                        src.loc,
                        tgt.loc,
                        Capture(piece_value),
                        moves,
                    )
                }
            }
            FieldValue(None) => {
                if tgt.rank == ROW_SIZE as i8 - 2
                    && self.possible_en_passant[usize::from(tgt.hb)] == Some(tgt.loc)
                {
                    let ep_pawn_pos = move_rank(tgt, false).unwrap().loc;
                    self.gen_non_king_move_unless_check(
                        src.loc,
                        tgt.loc,
                        EnPassant(self.board[usize::from(ep_pawn_pos)], ep_pawn_pos),
                        moves,
                    )
                } else {
                    false
                }
            }
            _ => false,
        }
    }
    fn gen_moves_pawn(
        &mut self,
        field: FieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) {
        let src = AnnotatedFieldLocation::from_with_origin(self.turn, field);
        if !opts.captures_only {
            if let Some(up) = move_rank(src, true) {
                if let FieldValue(None) = self.get_field_value(up.loc) {
                    if src.rank == 2 {
                        let up2 = move_rank(up, true).unwrap();
                        if let FieldValue(None) = self.get_field_value(up2.loc) {
                            if self.gen_non_king_move_unless_check(src.loc, up2.loc, Slide, moves)
                                && opts.only_one
                            {
                                return;
                            }
                        }
                    }
                    if src.rank == 7 {
                        let mut mov = Move {
                            source: src.loc,
                            target: up.loc,
                            move_type: Promotion(Queen),
                        };
                        if !self.would_non_king_move_bypass_check(mov) {
                            for promotion_piece in [Queen, Bishop, Knight, Rook] {
                                mov.move_type = Promotion(promotion_piece);
                                moves.push(mov);
                                if opts.only_one {
                                    return;
                                }
                            }
                        }
                    } else {
                        if self.gen_non_king_move_unless_check(src.loc, up.loc, Slide, moves)
                            && opts.only_one
                        {
                            return;
                        }
                    }
                }
            }
        }
        for right in [true, false] {
            match move_diagonal(src, true, right) {
                Some((one, two)) => {
                    self.try_gen_pawn_capture(src, one, moves, opts);
                    if opts.only_one {
                        return;
                    }
                    if let Some(two) = two {
                        if self.try_gen_pawn_capture(src, two, moves, opts) && opts.only_one {
                            return;
                        }
                    }
                }
                None => {}
            }
        }
    }
    fn gen_moves_queen(
        &mut self,
        field: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) {
        let moves_len = moves.len();
        self.gen_moves_rook(field, moves, opts);
        if opts.only_one && moves.len() != moves_len {
            return;
        }
        self.gen_moves_bishop(field, moves, opts);
    }
    pub fn fifty_move_rule_applies(&self) -> bool {
        self.move_index >= self.last_capture_or_pawn_move_index + DRAW_AFTER_N_SLIDES as u16
    }
    pub fn threefold_repetition_applies(&self) -> bool {
        false //TODO: implement this
    }
    pub fn gen_moves_for_field(
        &mut self,
        field: FieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) {
        if self.game_status != GameStatus::Ongoing {
            return;
        }
        self.gen_moves_for_field_unchecked(field, moves, opts);
    }
    pub fn gen_moves_for_field_unchecked(
        &mut self,
        field: FieldLocation,
        moves: &mut Vec<Move>,
        opts: MovegenOptions,
    ) {
        match self.get_field_value(field) {
            FieldValue(Some((color, piece_type))) if color == self.turn => {
                let field = field;
                let field_a = AnnotatedFieldLocation::from(field);
                match piece_type {
                    PieceType::Pawn => self.gen_moves_pawn(field, moves, opts),
                    PieceType::Knight => self.gen_moves_knight(field_a, moves, opts),
                    PieceType::Bishop => self.gen_moves_bishop(field_a, moves, opts),
                    PieceType::Rook => self.gen_moves_rook(field_a, moves, opts),
                    PieceType::Queen => self.gen_moves_queen(field_a, moves, opts),
                    PieceType::King => self.gen_moves_king(field_a, moves, opts),
                }
            }
            _ => {}
        }
    }
    pub fn gen_moves_with_options(&mut self, moves: &mut Vec<Move>, opts: MovegenOptions) {
        if self.game_status != GameStatus::Ongoing {
            return;
        }
        for i in 0..self.board.len() {
            self.gen_moves_for_field_unchecked(FieldLocation::from(i), moves, opts);
            if !moves.is_empty() && opts.only_one {
                return;
            }
        }
        if self.fifty_move_rule_applies() {
            moves.push(Move {
                move_type: ClaimDraw(DrawClaimBasis::FiftyMoveRule),
                source: Default::default(),
                target: Default::default(),
            });
        }
        if self.threefold_repetition_applies() {
            moves.push(Move {
                move_type: ClaimDraw(DrawClaimBasis::ThreefoldRepetition),
                source: Default::default(),
                target: Default::default(),
            });
        }
    }
    pub fn gen_moves(&mut self) -> Vec<Move> {
        let mut moves = Vec::new();
        self.gen_moves_with_options(&mut moves, MovegenOptions::default());
        moves
    }
    pub fn get_packed_field_value(&self, field: FieldLocation) -> PackedFieldValue {
        self.board[usize::from(field)]
    }
    pub fn get_field_value(&self, field: FieldLocation) -> FieldValue {
        FieldValue::from(self.get_packed_field_value(field))
    }
    pub fn is_valid_move(&mut self, mov: Move) -> bool {
        // this is not used by engines, and therfore not performance critical
        // we are therefore fine with using a rather inefficient implementation
        let mut dv = self.dummy_vec.take().unwrap();
        self.gen_moves_for_field(mov.source, &mut dv, MovegenOptions::default());
        for candidate_move in dv.iter() {
            if mov == *candidate_move {
                dv.clear();
                self.dummy_vec = Some(dv);
                return true;
            }
        }
        dv.clear();
        self.dummy_vec = Some(dv);
        false
    }
}
