use crate::board::MoveType::*;
use crate::board::PieceType::*;
use crate::board::*;
use arrayvec::ArrayVec;
use std::cmp::{max, min};
use std::option::Option::*;
use MovegenResult::*;
pub const HBRC: i8 = HB_ROW_COUNT as i8;
pub const RS: i8 = ROW_SIZE as i8;

const MOVES_FOR_FIELD_DIAGONAL_LINES_MAX_SQUARES: usize = 17;
const MOVES_FOR_FIELD_DIAGONAL_LINE_COUNT: usize = 5;

const MOVES_FOR_FIELD_ORTHOGONAL_LINE_COUNT: usize = 4;
const MOVES_FOR_FIELD_ORTHOGONAL_LINES_SQUARE_COUNT: usize = ROW_SIZE * 2 - 2;
const MAX_KNIGHT_MOVES_PER_SQUARE: usize = 10;
#[repr(packed)]
#[derive(Copy, Clone)]
pub struct MovegenOptions {
    pub gen_slides: bool,
    pub gen_null_move: bool,
    pub only_one: bool,
}

pub struct MovegenParams {
    opts: MovegenOptions,
    potential_checks: MovesForField,
    king_is_safe: bool,
}

impl MovegenParams {
    pub fn new(board: &ThreePlayerChess, opts: MovegenOptions) -> MovegenParams {
        let turn = board.turn;
        let pc = &board.moves_for_board[usize::from(board.king_positions[usize::from(turn)])];
        let mut mp = MovegenParams {
            opts: opts,
            potential_checks: MovesForField::new_empty(),
            king_is_safe: true,
        };
        for m in pc.knight_moves.iter() {
            if let Some((color, piece_type)) = *FieldValue::from(board.board[usize::from(*m)]) {
                if piece_type == Knight && color != turn {
                    mp.potential_checks.knight_moves.push(*m);
                    mp.king_is_safe = false;
                }
            }
        }
        let mut line_ends = 0;
        for (oln, oli) in pc.orthogonal_lines_iter().enumerate() {
            const XX: usize = usize::MAX;
            let mut first_enemy = XX;
            let mut second_enemy = XX;
            let mut first_friend = XX;
            let mut second_friend = XX;
            for (i, m) in oli.iter().enumerate() {
                if let Some((color, piece_type)) = *FieldValue::from(board.board[usize::from(*m)]) {
                    if color == turn
                        || (piece_type == Pawn || piece_type == Knight || piece_type == King)
                    {
                        if first_friend == XX {
                            first_friend = i;
                        } else {
                            second_friend = i;
                            break;
                        }
                    } else {
                        if first_enemy == XX {
                            first_enemy = i;
                            second_enemy = i;
                        } else {
                            second_enemy = i;
                            break;
                        }
                    }
                }
            }
            if first_enemy != XX && second_friend >= first_enemy {
                let len = second_enemy + 1;
                mp.potential_checks.orthogonal_lines[line_ends..line_ends + len]
                    .copy_from_slice(&oli[0..second_enemy + 1]);
                line_ends += len;
                mp.king_is_safe = false;
            }
            mp.potential_checks.orthogonal_line_ends[oln] = line_ends as u8;
        }
        line_ends = 0;
        for (dln, dli) in pc.diagonal_lines_iter().enumerate() {
            const XX: usize = usize::MAX;
            let mut first_enemy = XX;
            let mut second_enemy = XX;
            let mut first_friend = XX;
            let mut second_friend = XX;
            for (i, m) in dli.iter().enumerate() {
                if let Some((color, piece_type)) = *FieldValue::from(board.board[usize::from(*m)]) {
                    if color == turn
                        || (piece_type == Rook
                            || piece_type == Knight
                            || piece_type == King
                            || (piece_type == Pawn && i > 0))
                    {
                        if first_friend == XX {
                            first_friend = i;
                        } else {
                            second_friend = i;
                            break;
                        }
                    } else {
                        if first_enemy == XX {
                            first_enemy = i;
                            second_enemy = i;
                        } else {
                            second_enemy = i;
                            break;
                        }
                    }
                }
            }
            if first_enemy != XX && second_friend >= first_enemy {
                let len = second_enemy + 1;
                mp.potential_checks.diagonal_lines[line_ends..line_ends + len]
                    .copy_from_slice(&dli[0..second_enemy + 1]);
                line_ends += len;
                mp.king_is_safe = false;
            }
            mp.potential_checks.diagonal_line_ends[dln] = line_ends as u8;
        }
        mp
    }
}

enum MovegenResult {
    SuccessCapture,
    SuccessSlide,
    SlideButDisabled,
    SameColorCollision,
    CaptureButCheck,
    SlideButCheck,
}

pub struct SegmentedArrayIter<
    'a,
    T,
    INDICES: Into<usize> + Copy,
    const ARR_CAP: usize,
    const SEGMENT_COUNT: usize,
> {
    storage_arr: &'a [T; ARR_CAP],
    segment_ends: &'a [INDICES; SEGMENT_COUNT],
    segment_idx: usize,
}

impl<'a, T, INDICES: Into<usize> + Copy, const ARR_CAP: usize, const SEGMENT_COUNT: usize> Iterator
    for SegmentedArrayIter<'a, T, INDICES, ARR_CAP, SEGMENT_COUNT>
{
    type Item = &'a [T];

    fn next(&mut self) -> Option<&'a [T]> {
        if self.segment_idx < SEGMENT_COUNT {
            let start = if self.segment_idx == 0 {
                0usize
            } else {
                self.segment_ends[self.segment_idx - 1].into()
            };
            self.segment_idx += 1;
            Some(&self.storage_arr[start as usize..self.segment_ends[self.segment_idx - 1].into()])
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct MovesForField {
    pub knight_moves: ArrayVec<FieldLocation, MAX_KNIGHT_MOVES_PER_SQUARE>,

    pub diagonal_lines: [FieldLocation; MOVES_FOR_FIELD_DIAGONAL_LINES_MAX_SQUARES],
    pub diagonal_line_ends: [u8; MOVES_FOR_FIELD_DIAGONAL_LINE_COUNT],

    pub orthogonal_lines: [FieldLocation; MOVES_FOR_FIELD_ORTHOGONAL_LINES_SQUARE_COUNT],
    pub orthogonal_line_ends: [u8; MOVES_FOR_FIELD_ORTHOGONAL_LINE_COUNT],
}

impl MovesForField {
    pub fn diagonal_lines_iter<'a>(
        &'a self,
    ) -> SegmentedArrayIter<
        'a,
        FieldLocation,
        u8,
        MOVES_FOR_FIELD_DIAGONAL_LINES_MAX_SQUARES,
        MOVES_FOR_FIELD_DIAGONAL_LINE_COUNT,
    > {
        SegmentedArrayIter {
            storage_arr: &self.diagonal_lines,
            segment_ends: &self.diagonal_line_ends,
            segment_idx: 0,
        }
    }
    pub fn orthogonal_lines_iter<'a>(
        &'a self,
    ) -> SegmentedArrayIter<
        'a,
        FieldLocation,
        u8,
        MOVES_FOR_FIELD_ORTHOGONAL_LINES_SQUARE_COUNT,
        MOVES_FOR_FIELD_ORTHOGONAL_LINE_COUNT,
    > {
        SegmentedArrayIter {
            storage_arr: &&self.orthogonal_lines,
            segment_ends: &self.orthogonal_line_ends,
            segment_idx: 0,
        }
    }
}

pub type MovesForBoard = [MovesForField; BOARD_SIZE];

lazy_static! {
    pub static ref MOVES_FOR_BOARD: MovesForBoard = {
        let mut cps = ArrayVec::new();
        for i in 0..BOARD_SIZE {
            cps.push(MovesForField::new_full(FieldLocation::from(i)));
        }
        cps.into_inner().unwrap()
    };
}

impl Default for MovegenOptions {
    fn default() -> Self {
        MovegenOptions {
            gen_slides: true,
            gen_null_move: false,
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

impl MovesForField {
    pub fn add_orthogonal_lines(&mut self, afl: AnnotatedFieldLocation) {
        let mut f = afl.loc;
        let mut i = 0;
        for _ in 0..afl.file - 1 {
            f = FieldLocation::from(u8::from(f) - 1);
            self.orthogonal_lines[i] = f;
            i += 1;
        }
        self.orthogonal_line_ends[0] = i as u8;
        f = afl.loc;
        for _ in afl.file..RS {
            f = FieldLocation::from(u8::from(f) + 1);
            self.orthogonal_lines[i] = f;
            i += 1;
        }
        self.orthogonal_line_ends[1] = i as u8;
        let mut f = afl;
        for _ in 0..afl.rank - 1 {
            f = move_rank(f, false).unwrap();
            self.orthogonal_lines[i] = f.loc;
            i += 1;
        }
        self.orthogonal_line_ends[2] = i as u8;
        f = afl;
        for _ in afl.rank..RS {
            f = move_rank(f, true).unwrap();
            self.orthogonal_lines[i] = f.loc;
            i += 1;
        }
        self.orthogonal_line_ends[3] = i as u8;
    }
    pub fn add_diagonal_lines(&mut self, afl: AnnotatedFieldLocation) {
        let mut lines_idx = 0;
        let mut lines_count = 0;
        let mut line_begin = 0;
        for (length, up, right) in [
            (min(RS - afl.file, RS - afl.rank), true, true), // up right
            (min(afl.file, RS - afl.rank), true, false),     // up left
            (min(afl.file, afl.rank), false, false),         // down left
            (min(RS - afl.file, afl.rank), false, true),     // down right
        ] {
            let mut pos = afl;
            for i in 0..length {
                match move_diagonal(pos, up, right) {
                    None => break,
                    Some((one, None)) => {
                        self.diagonal_lines[lines_idx] = one.loc;
                        lines_idx += 1;
                        pos = one;
                    }
                    Some((mut one, Some(mut two))) => {
                        let swap_dir = pos.hb != afl.origin;
                        if swap_dir && one.hb != afl.origin {
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
                        self.diagonal_line_ends[lines_count] = line_end as u8;
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
            self.diagonal_line_ends[lines_count] = lines_idx as u8;
            line_begin = lines_idx;
            lines_count += 1;
        }
        if lines_count != MOVES_FOR_FIELD_DIAGONAL_LINE_COUNT {
            debug_assert!(lines_count + 1 == MOVES_FOR_FIELD_DIAGONAL_LINE_COUNT);
            self.diagonal_line_ends[lines_count] = self.diagonal_line_ends[lines_count - 1];
        }
    }
    pub fn add_knight_moves(&mut self, afl: AnnotatedFieldLocation) {
        get_knight_moves_for_field(afl, &mut self.knight_moves);
    }
    pub fn new_empty() -> Self {
        Self {
            ..Default::default()
        }
    }
    pub fn new_full(field: FieldLocation) -> MovesForField {
        let mut mff = MovesForField::new_empty();
        let afl = AnnotatedFieldLocation::from(field);
        mff.add_knight_moves(afl);
        mff.add_orthogonal_lines(afl);
        mff.add_diagonal_lines(afl);
        mff
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
    let dir: u8 = if clockwise { 1 } else { 3 - 1 };
    Color::from_u8((u8::from(color) + dir) % COLOR_COUNT)
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

pub fn move_diagonal(
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
            let hb1 = field.hb.next();
            let hb2 = hb1.next();
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
    pub fn perform_reversable_move(&mut self, rm: &ReversableMove, update_status: bool) {
        self.movegen_sanity_check(rm, true, false);
        self.apply_move(rm.mov);
        self.apply_move_sideeffects(rm.mov, update_status);
        self.movegen_sanity_check(rm, false, false);
    }
    pub fn perform_move(&mut self, m: Move) {
        self.apply_move(m);
        self.apply_move_sideeffects(m, true);
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
        debug_assert!(
            FieldValue::from(self.board[tgt]).piece_type() != Some(King),
            "trying to capture king with {}->{} at: {}",
            m.source.to_str_fancy(),
            m.target.to_str_fancy(),
            self.state_string()
        );
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

            NullMove | ClaimDraw(_) => return,
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
    pub fn apply_move_sideeffects(&mut self, m: Move, update_status: bool) {
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
            NullMove => {
                self.zobrist_hash.fifty_move_rule_move_inc(
                    self.move_index,
                    self.last_capture_or_pawn_move_index,
                );
            }
        }
        self.turn = self.turn.next();
        self.zobrist_hash.next_turn(self.turn);
        if update_status {
            self.game_status = self.game_status();
        }
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
            NullMove | ClaimDraw(_) => return,
        }
    }
    pub fn unapply_move_sideffects(&mut self, rm: &ReversableMove) {
        self.turn = self.turn.prev();
        self.game_status = GameStatus::Ongoing;
        self.move_index -= 1;
        self.last_capture_or_pawn_move_index = rm.last_capture_or_pawn_move_index;
        debug_assert!(self.move_index >= rm.last_capture_or_pawn_move_index);

        self.possible_rooks_for_castling = rm.possible_rooks_for_castling;
        self.possible_en_passant = rm.possible_en_passant;
        self.zobrist_hash.value = rm.zobrist_hash_value;

        if let NullMove | ClaimDraw(_) = rm.mov.move_type {
            return;
        }
        if self.get_field_value(rm.mov.target).piece_type().unwrap() == King {
            self.king_positions[usize::from(self.turn)] = rm.mov.source;
        }
    }
    fn would_king_move_bypass_check(&mut self, mv: Move) -> bool {
        self.apply_move(mv);
        let would_be_check = self.is_king_in_check(
            self.turn,
            mv.target,
            &self.moves_for_board[usize::from(mv.target)],
        );
        self.unapply_move(mv);
        would_be_check
    }
    fn would_non_king_move_bypass_check(
        &mut self,
        mv: Move,
        potential_king_checks: &MovesForField,
    ) -> bool {
        self.apply_move(mv);
        let would_be_check = self.is_king_in_check(
            self.turn,
            self.king_positions[usize::from(self.turn)],
            potential_king_checks,
        );
        self.unapply_move(mv);
        would_be_check
    }
    fn append_non_king_move_unless_check(
        &mut self,
        mv: Move,
        mp: &MovegenParams,
        moves: &mut Vec<Move>,
    ) -> bool {
        if mp.king_is_safe || !self.would_non_king_move_bypass_check(mv, &mp.potential_checks) {
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
        mp: &MovegenParams,
        moves: &mut Vec<Move>,
    ) -> bool {
        let m = Move {
            move_type,
            source: src,
            target: tgt,
        };
        self.append_non_king_move_unless_check(m, mp, moves)
    }
    pub fn is_piece_capturable_at(
        &mut self,
        loc: FieldLocation,
        capturing_color: Option<Color>,
        potential_king_checks: Option<&MovesForField>,
    ) -> Option<Move> {
        //checking for checks only makes sense for the current player
        debug_assert!(potential_king_checks.is_none() || capturing_color == Some(self.turn));
        let field_value = self.get_packed_field_value(loc);
        debug_assert!(
            FieldValue::from(field_value).is_some(),
            "checking for capturable piece without a pice at {}",
            self.state_string()
        );
        let (piece_color, piece_type) = FieldValue::from(field_value).unwrap();
        fn color_may_capture(
            col: Color,
            piece_color: Color,
            capturing_color: Option<Color>,
        ) -> bool {
            col != piece_color && capturing_color.map(|cc| cc == col).unwrap_or(true)
        }
        let kp = AnnotatedFieldLocation::from(loc);
        let mff = &self.moves_for_board[usize::from(loc)];
        for oli in mff.orthogonal_lines_iter() {
            for (i, f) in oli.iter().enumerate() {
                match *self.get_field_value(*f) {
                    Some((color, piece_type)) => match piece_type {
                        PieceType::Rook | PieceType::Queen
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            let m = Move::new(*f, loc, Capture(field_value));
                            if potential_king_checks
                                .map_or(true, |pkc| !self.would_non_king_move_bypass_check(m, pkc))
                            {
                                return Some(m);
                            }
                            break;
                        }
                        PieceType::King
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            if i == 0 {
                                let m = Move::new(*f, loc, Capture(field_value));
                                if potential_king_checks
                                    .map_or(true, |_| !self.would_king_move_bypass_check(m))
                                {
                                    return Some(m);
                                }
                            }
                            break;
                        }
                        _ => break,
                    },
                    None => (),
                }
            }
        }
        for dli in mff.diagonal_lines_iter() {
            for (i, f) in dli.iter().enumerate() {
                let board_val = self.get_field_value(*f);
                match *board_val {
                    None => continue,
                    Some((color, piece_type)) => match piece_type {
                        PieceType::Bishop | PieceType::Queen
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            let m = Move::new(*f, loc, Capture(field_value));
                            if potential_king_checks
                                .map_or(true, |pkc| !self.would_non_king_move_bypass_check(m, pkc))
                            {
                                return Some(m);
                            }
                            break;
                        }
                        PieceType::King
                            if color_may_capture(color, piece_color, capturing_color) =>
                        {
                            if i == 0 {
                                let m = Move::new(*f, loc, Capture(field_value));
                                if potential_king_checks
                                    .map_or(true, |_| !self.would_king_move_bypass_check(m))
                                {
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
                                if potential_king_checks.map_or(true, |pkc| {
                                    !self.would_non_king_move_bypass_check(m, pkc)
                                }) {
                                    return Some(m);
                                }
                            }
                            break;
                        }
                        _ => break,
                    },
                }
            }
        }
        for f in mff.knight_moves.iter() {
            let board_val = self.get_field_value(*f);
            match *FieldValue::from(board_val) {
                None => continue,
                Some((color, piece_type)) => match piece_type {
                    PieceType::Knight if color_may_capture(color, piece_color, capturing_color) => {
                        let m = Move::new(*f, loc, Capture(field_value));
                        if potential_king_checks
                            .map_or(true, |pkc| !self.would_non_king_move_bypass_check(m, pkc))
                        {
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
                            if let Some((color, piece_type)) = *self.get_field_value(src_loc) {
                                if piece_type == PieceType::Pawn
                                    && color_may_capture(color, piece_color, capturing_color)
                                {
                                    let tgt_loc = FieldLocation::new(kp.hb, kp.file, kp.rank - 1);
                                    let m =
                                        Move::new(src_loc, tgt_loc, EnPassant(field_value, loc));
                                    if potential_king_checks.map_or(true, |pkc| {
                                        !self.would_non_king_move_bypass_check(m, pkc)
                                    }) {
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
    // this is a special case of is_piece_capturable_at.
    // but because it is close to being the hottest function in the entire codebase
    // and a lot of additional assumptions can be made
    // (no en passent, no check_for_checks, no king besides king, no need to return the capturing move, ...),
    // we have a separate handrolled version of it
    pub fn is_king_in_check(
        &mut self,
        king_color: Color,
        king_location: FieldLocation,
        mff: &MovesForField,
    ) -> bool {
        for oli in mff.orthogonal_lines_iter() {
            for (i, f) in oli.iter().enumerate() {
                match *self.get_field_value(*f) {
                    Some((color, piece_type)) if color != king_color => match piece_type {
                        PieceType::Rook | PieceType::Queen => {
                            return true;
                        }
                        // we use this func to test legal moves, so this can happen
                        PieceType::King if i == 0 => {
                            return true;
                        }
                        _ => break,
                    },
                    Some(_) => break,
                    None => continue,
                }
            }
        }
        for dli in mff.diagonal_lines_iter() {
            for (i, f) in dli.iter().enumerate() {
                match *self.get_field_value(*f) {
                    Some((color, piece_type)) if color != king_color => match piece_type {
                        PieceType::Bishop | PieceType::Queen => {
                            return true;
                        }
                        PieceType::King if i == 0 => {
                            return true;
                        }
                        PieceType::Pawn if i == 0 => {
                            let field_pos =
                                AnnotatedFieldLocation::from_with_origin(color, king_location);
                            let pawn_pos = AnnotatedFieldLocation::from_with_origin(color, *f);
                            if pawn_pos.rank + 1 == field_pos.rank {
                                return true;
                            }
                            break;
                        }
                        _ => break,
                    },
                    Some(_) => break,
                    None => continue,
                }
            }
        }
        for f in mff.knight_moves.iter() {
            match *self.get_field_value(*f) {
                Some((color, piece_type)) if color != king_color => match piece_type {
                    PieceType::Knight => {
                        return true;
                    }
                    _ => continue,
                },
                _ => continue,
            }
        }
        false
    }
    pub fn game_status(&mut self) -> GameStatus {
        if self.game_status != GameStatus::Ongoing {
            return self.game_status;
        }
        // check whether the current player has a legal move (doesn't disregard check)
        // that would capture the third player's king
        // (piece of previous player moved out of the way uncovering the king). in that case the current player wins
        let next_player = self.turn.next();
        let next_player_king_pos = self.king_positions[usize::from(next_player)];
        // NOTE: not sure if we should we check for checks here?
        if self
            .is_piece_capturable_at(next_player_king_pos, Some(self.turn), None)
            .is_some()
        {
            return GameStatus::Win(self.turn, WinReason::CapturableKing(next_player));
        }
        // check whether the current player has no move to get out of check
        // which would mean somebody won, or we have a stalemate
        let opts = MovegenOptions {
            gen_slides: true,
            gen_null_move: false,
            only_one: true,
        };
        let mut dv = self.dummy_vec.take().unwrap();
        self.gen_moves_with_options(&mut dv, opts);
        let no_legal_moves = dv.is_empty();
        dv.clear();
        self.dummy_vec = Some(dv);

        if no_legal_moves {
            let king_pos = self.king_positions[usize::from(self.turn)];
            let potential_checks = &self.moves_for_board[usize::from(king_pos)];
            if self.is_king_in_check(self.turn, king_pos, potential_checks) {
                let winner = if self
                    .is_piece_capturable_at(king_pos, Some(next_player), None)
                    .is_some()
                {
                    next_player
                } else {
                    next_player.next()
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
        mp: &MovegenParams,
    ) -> MovegenResult {
        let piece_value = self.get_packed_field_value(tgt);
        match FieldValue::from(piece_value) {
            FieldValue(None) => {
                if !mp.opts.gen_slides {
                    SlideButDisabled
                } else if self.gen_non_king_move_unless_check(src, tgt, Slide, mp, moves) {
                    SuccessSlide
                } else {
                    SlideButCheck
                }
            }
            FieldValue(Some((color, _))) if color != self.turn => {
                let cap = Capture(piece_value);
                if self.gen_non_king_move_unless_check(src, tgt, cap, mp, moves) {
                    SuccessCapture
                } else {
                    CaptureButCheck
                }
            }
            _ => MovegenResult::SameColorCollision,
        }
    }

    fn gen_moves_rook(&mut self, field: FieldLocation, moves: &mut Vec<Move>, mp: &MovegenParams) {
        let mff = &self.moves_for_board[usize::from(field)];
        for oli in mff.orthogonal_lines_iter() {
            for f in oli {
                match self.gen_non_king_slide_move(field, *f, moves, mp) {
                    SuccessCapture | SuccessSlide if mp.opts.only_one => return,
                    SuccessCapture | CaptureButCheck | SameColorCollision => break,
                    SlideButCheck | SuccessSlide | SlideButDisabled => {}
                }
            }
        }
    }
    fn gen_moves_bishop(
        &mut self,
        field: FieldLocation,
        moves: &mut Vec<Move>,
        mp: &MovegenParams,
    ) {
        let mff = &self.moves_for_board[usize::from(field)];
        for oli in mff.diagonal_lines_iter() {
            for f in oli {
                match self.gen_non_king_slide_move(field, *f, moves, mp) {
                    SuccessCapture | SuccessSlide if mp.opts.only_one => return,
                    SuccessCapture | CaptureButCheck | SameColorCollision => break,
                    SlideButCheck | SuccessSlide | SlideButDisabled => {}
                };
            }
        }
    }
    fn gen_moves_knight(
        &mut self,
        field: FieldLocation,
        moves: &mut Vec<Move>,
        mp: &MovegenParams,
    ) {
        let mff = &self.moves_for_board[usize::from(field)];
        for m in mff.knight_moves.iter() {
            let mt = self.gen_non_king_slide_move(field, *m, moves, mp);
            if mp.opts.only_one && mt.success() {
                return;
            };
        }
    }
    pub fn gen_move_castling(&mut self, short: bool, mp: &MovegenParams) -> Option<Move> {
        let hb = self.turn;
        let king_src = AnnotatedFieldLocation::from(self.king_positions[usize::from(hb)]);
        let rook_src = AnnotatedFieldLocation::from(
            self.possible_rooks_for_castling[usize::from(hb)][short as usize]?,
        );

        // we only check this after testing if the rook is allowed because that's much cheaper
        if self.is_king_in_check(self.turn, king_src.loc, &mp.potential_checks) {
            return None;
        }

        let king_tgt = AnnotatedFieldLocation::from(get_castling_target(hb, true, short));
        let rook_tgt = AnnotatedFieldLocation::from(get_castling_target(hb, false, short));

        let king_val = self.get_packed_field_value(king_src.loc);
        let rook_val = self.get_packed_field_value(rook_src.loc);

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
            conflict = self.is_king_in_check(
                self.turn,
                kp.loc,
                &self.moves_for_board[usize::from(kp.loc)],
            );
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
        mp: &MovegenParams,
    ) -> bool {
        let move_type;
        let piece_val = self.get_packed_field_value(tgt);
        if let Some((color, _)) = *FieldValue::from(piece_val) {
            if color == self.turn {
                return false;
            }
            move_type = Capture(piece_val);
        } else {
            if !mp.opts.gen_slides {
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
        let would_be_check =
            self.is_king_in_check(self.turn, tgt, &self.moves_for_board[usize::from(tgt)]);
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
        mp: &MovegenParams,
    ) {
        for tgt in [
            move_rank(field, true),
            move_rank(field, false),
            move_file(field, true),
            move_file(field, false),
        ] {
            if let Some(tgt) = tgt {
                if self.gen_king_slide_unless_check(field.loc, tgt.loc, moves, mp)
                    && mp.opts.only_one
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
                                if self.gen_king_slide_unless_check(field.loc, tgt.loc, moves, mp)
                                    && mp.opts.only_one
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
        for castle in [
            self.gen_move_castling(false, mp),
            self.gen_move_castling(true, mp),
        ] {
            if let Some(m) = castle {
                moves.push(m);
                if mp.opts.only_one {
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
        mp: &MovegenParams,
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
                    if !self.would_non_king_move_bypass_check(mov, &mp.potential_checks) {
                        for promotion_piece in [Queen, Bishop, Knight, Rook] {
                            mov.move_type = CapturePromotion(piece_value, promotion_piece);
                            moves.push(mov);
                            if mp.opts.only_one {
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
                        mp,
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
                        EnPassant(self.get_packed_field_value(ep_pawn_pos), ep_pawn_pos),
                        mp,
                        moves,
                    )
                } else {
                    false
                }
            }
            _ => false,
        }
    }
    fn gen_moves_pawn(&mut self, field: FieldLocation, moves: &mut Vec<Move>, mp: &MovegenParams) {
        let src = AnnotatedFieldLocation::from_with_origin(self.turn, field);
        if mp.opts.gen_slides {
            if let Some(up) = move_rank(src, true) {
                if let FieldValue(None) = self.get_field_value(up.loc) {
                    if src.rank == 2 {
                        let up2 = move_rank(up, true).unwrap();
                        if let FieldValue(None) = self.get_field_value(up2.loc) {
                            if self
                                .gen_non_king_move_unless_check(src.loc, up2.loc, Slide, mp, moves)
                                && mp.opts.only_one
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
                        if !self.would_non_king_move_bypass_check(mov, &mp.potential_checks) {
                            for promotion_piece in [Queen, Bishop, Knight, Rook] {
                                mov.move_type = Promotion(promotion_piece);
                                moves.push(mov);
                                if mp.opts.only_one {
                                    return;
                                }
                            }
                        }
                    } else {
                        if self.gen_non_king_move_unless_check(src.loc, up.loc, Slide, mp, moves)
                            && mp.opts.only_one
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
                    self.try_gen_pawn_capture(src, one, moves, mp);
                    if mp.opts.only_one {
                        return;
                    }
                    if let Some(two) = two {
                        if self.try_gen_pawn_capture(src, two, moves, mp) && mp.opts.only_one {
                            return;
                        }
                    }
                }
                None => {}
            }
        }
    }
    fn gen_moves_queen(&mut self, field: FieldLocation, moves: &mut Vec<Move>, mp: &MovegenParams) {
        self.gen_moves_rook(field, moves, mp);
        if mp.opts.only_one && !moves.is_empty() {
            return;
        }
        self.gen_moves_bishop(field, moves, mp);
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
        mp: &MovegenParams,
    ) {
        if self.game_status != GameStatus::Ongoing {
            return;
        }
        self.gen_moves_for_field_unchecked(field, moves, mp);
    }
    pub fn gen_moves_for_field_unchecked(
        &mut self,
        field: FieldLocation,
        moves: &mut Vec<Move>,
        mp: &MovegenParams,
    ) {
        match self.get_field_value(field) {
            FieldValue(Some((color, piece_type))) if color == self.turn => match piece_type {
                PieceType::Pawn => self.gen_moves_pawn(field, moves, mp),
                PieceType::Knight => self.gen_moves_knight(field, moves, mp),
                PieceType::Bishop => self.gen_moves_bishop(field, moves, mp),
                PieceType::Rook => self.gen_moves_rook(field, moves, mp),
                PieceType::Queen => self.gen_moves_queen(field, moves, mp),
                PieceType::King => {
                    self.gen_moves_king(AnnotatedFieldLocation::from(field), moves, mp)
                }
            },
            _ => {}
        }
    }
    pub fn is_king_safe(&mut self) -> bool {
        false
    }
    pub fn gen_moves_with_options(&mut self, moves: &mut Vec<Move>, opts: MovegenOptions) {
        let mp = MovegenParams::new(self, opts);
        if self.game_status != GameStatus::Ongoing {
            return;
        }
        for i in 0..self.board.len() {
            self.gen_moves_for_field_unchecked(FieldLocation::from(i), moves, &mp);
            if mp.opts.only_one && !moves.is_empty() {
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
        if mp.opts.gen_null_move {
            if !self.is_king_in_check(
                self.turn,
                self.king_positions[usize::from(self.turn)],
                &mp.potential_checks,
            ) {
                moves.push(Move {
                    move_type: NullMove,
                    source: Default::default(),
                    target: Default::default(),
                });
            }
        }
    }
    pub fn gen_moves(&mut self) -> Vec<Move> {
        let mut moves = Vec::new();
        self.gen_moves_with_options(&mut moves, MovegenOptions::default());
        moves
    }
    #[inline(always)]
    pub fn get_packed_field_value(&self, field: FieldLocation) -> PackedFieldValue {
        // this is safe because FieldLocation asserts on construction
        // that it's numerical value (-1 for NonZeroU8) is less than BOARD_SIZE
        unsafe { *self.board.get_unchecked(usize::from(field)) }
    }
    #[inline(always)]
    pub fn get_field_value(&self, field: FieldLocation) -> FieldValue {
        FieldValue::from(self.get_packed_field_value(field))
    }
    pub fn is_valid_move(&mut self, mov: Move) -> bool {
        // this is not used by engines, and therfore not performance critical
        // we are therefore fine with using a rather inefficient implementation
        let mut dv = self.dummy_vec.take().unwrap();
        let mp = MovegenParams::new(self, MovegenOptions::default());
        self.gen_moves_for_field(mov.source, &mut dv, &mp);
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
