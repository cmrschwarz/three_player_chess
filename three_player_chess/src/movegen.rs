use crate::board::*;
use arrayvec::ArrayVec;
use num_traits::FromPrimitive;
use std::cmp::min;
use std::option::Option::*;
pub const HBRC: i8 = HB_ROW_COUNT as i8;
pub const RS: i8 = ROW_SIZE as i8;

const CHECK_LINES_DIAGONALS_MAX_SQUARES: usize = 17;
const CHECK_LINES_DIAGONALS_COUNT: usize = 5;
const MAX_KNIGHT_MOVES_PER_SQUARE: usize = 10;

#[derive(Clone, PartialEq, Eq, Default)]
pub struct CheckPossibilities {
    knight_moves: ArrayVec<FieldLocation, MAX_KNIGHT_MOVES_PER_SQUARE>,

    diagonal_lines: [FieldLocation; CHECK_LINES_DIAGONALS_MAX_SQUARES],
    diagonal_line_ends: [usize; CHECK_LINES_DIAGONALS_COUNT],

    file: [FieldLocation; ROW_SIZE],
    rank: [FieldLocation; ROW_SIZE],
}

impl CheckPossibilities {
    fn add_cardinal_directions(&mut self, field: AnnotatedFieldLocation) {
        let mut rank_it = FieldLocation::new(field.hb, 1, field.rank);
        for i in 0..ROW_SIZE {
            self.rank[i] = rank_it;
            rank_it = FieldLocation::from(u8::from(rank_it) + 1);
        }
        self.file[0] = FieldLocation::new(field.origin, field.file, 1);
        self.file[HB_ROW_COUNT] = FieldLocation::new(
            get_next_hb(field.origin, field.file <= HBRC),
            invert_coord(field.file),
            1,
        );
        for i in 1..HB_ROW_COUNT {
            self.file[i] = FieldLocation::from(u8::from(self.file[i - 1]) + ROW_SIZE as u8);
            self.file[i + HB_ROW_COUNT] =
                FieldLocation::from(u8::from(self.file[i + HB_ROW_COUNT - 1]) + ROW_SIZE as u8);
        }
    }
    fn add_diagonal_directions(&mut self, field: AnnotatedFieldLocation) {
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
                        for i in line_begin..line_split_point + 1 {
                            self.diagonal_lines[lines_idx] = self.diagonal_lines[i];
                            lines_idx += 1;
                        }
                        line_begin = line_end;
                    }
                }
            }
            self.diagonal_line_ends[lines_count] = lines_idx;
            line_begin = lines_idx;
            lines_count += 1;
        }
        if lines_count != CHECK_LINES_DIAGONALS_COUNT {
            assert!(lines_count + 1 == CHECK_LINES_DIAGONALS_COUNT);
            self.diagonal_line_ends[lines_count] = self.diagonal_line_ends[lines_count - 1];
        }
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
        let afl = AnnotatedFieldLocation::from_field(king_pos);
        get_knight_moves_for_field(afl, &mut cp.knight_moves);
        cp.add_cardinal_directions(afl);
        cp.add_diagonal_directions(afl);
        cp
    }
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
    pub fn from_field_with_origin(origin: Color, field: FieldLocation) -> Self {
        Self::from_field_with_origin_and_hb(origin, get_hb(field), field)
    }
    pub fn from_field(field: FieldLocation) -> Self {
        let hb = get_hb(field);
        Self::from_field_with_origin_and_hb(hb, hb, field)
    }
    pub fn reorient(&self, origin: Color) -> Self {
        let mut res = self.clone();
        if origin != self.origin {
            res.origin = origin;
            res.file = invert_coord(self.file);
            res.rank = invert_coord(self.rank);
        }
        res
    }
}

fn get_hb(field: FieldLocation) -> Color {
    Color::from_usize(usize::from(field) / HB_SIZE).unwrap()
}

fn get_next_hb(color: Color, clockwise: bool) -> Color {
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
fn adjust_coord(coord: i8, invert: bool) -> i8 {
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
    adjust_coord(get_raw_rank(field), get_hb(field) != color)
}

pub fn get_raw_file(field: FieldLocation) -> i8 {
    (u8::from(field) % RS as u8 + 1) as i8
}
pub fn get_file(field: FieldLocation, color: Color) -> i8 {
    adjust_coord(get_raw_file(field), get_hb(field) != color)
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
    assert!(hb != field.origin || field.hb != field.origin);
    let file = adjust_coord(field.file, field.origin != hb);
    Some(AnnotatedFieldLocation::new(
        field.origin,
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
    for up in [true, false] {
        let rank_adjusted = adjust_coord(field.rank, !up);
        if rank_adjusted != HBRC - 1 && rank_adjusted != HBRC {
            continue;
        }
        let u1 = move_rank(field, up).unwrap();
        let u2 = move_rank(u1, up).unwrap();
        for right in [true, false] {
            move_file(u2, right).map(|m| moves.push(m.loc));
        }
        if rank_adjusted == HBRC && field.file > HBRC - 2 && field.file <= HBRC + 2 {
            let right = field.file < HBRC + 1;
            let u1r1 = move_file(u1, right).unwrap();
            let u1r2 = move_file(u1r1, right).unwrap();
            moves.push(u1r2.loc);
        }
    }
}

fn get_field_on_next_hb(loc: FieldLocation) -> FieldLocation {
    FieldLocation::from((u8::from(loc) + HB_SIZE as u8) % (HB_COUNT * HB_SIZE) as u8)
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
            let loc1 = get_field_on_next_hb(field.loc);
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
    let rank_dir = coord_dir(up == (field.hb == field.origin));
    let file_dir = coord_dir(right == (field.hb == field.origin));
    Some((
        AnnotatedFieldLocation::new(
            field.origin,
            FieldLocation::new(field.hb, field.file + file_dir, field.rank + rank_dir),
            field.hb,
            tgt_file,
            tgt_rank,
        ),
        None,
    ))
}

impl ThreePlayerChess {
    pub fn make_move(&mut self, m: Move) {
        let src = usize::from(m.source);
        let tgt = usize::from(m.target);
        match m.move_type {
            MoveType::Slide => {
                self.board[tgt] = self.board[src];
                self.board[src] = Default::default();
            }
            MoveType::Capture(_) => {
                self.board[tgt] = self.board[src];
            }
            MoveType::EnPassant(_, captured_pawn_loc) => {
                self.board[tgt] = self.board[src];
                self.board[usize::from(captured_pawn_loc)] = Default::default();
                self.board[src] = Default::default();
            }
            MoveType::Castle(rook_source, rook_target) => {
                let king = self.board[src];
                let rook = self.board[usize::from(rook_source)];
                self.board[src] = Default::default();
                self.board[usize::from(rook_source)] = Default::default();
                self.board[usize::from(rook_target)] = rook;
                self.board[tgt] = king;
            }
            MoveType::Promotion(piece_type) => {
                self.board[tgt] = FieldValue(Some((self.turn, piece_type))).into();
                self.board[src] = Default::default();
            }
            MoveType::ClaimDraw => return,
        }
    }
    pub fn apply_move_sideeffects(&mut self, m: Move) {
        let player_to_move = self.turn;
        let pid = usize::from(player_to_move);
        self.turn = get_next_hb(self.turn, true);
        self.move_index += 1;
        self.possible_en_passant[pid] = None;
        match m.move_type {
            MoveType::Slide => match FieldValue::from(self.board[usize::from(m.target)]).unwrap() {
                (_, PieceType::King) => {
                    self.king_positions[usize::from(player_to_move)] = m.target;
                    for r in self.possible_rooks_for_castling[pid].iter_mut() {
                        *r = None;
                    }
                    self.check_possibilities[usize::from(player_to_move)] =
                        CheckPossibilities::from_king_pos(m.target);
                }
                (_, PieceType::Pawn) => {
                    self.last_capture_or_pawn_move_index = self.move_index;
                    let source =
                        AnnotatedFieldLocation::from_field_with_origin(player_to_move, m.source);
                    let target =
                        AnnotatedFieldLocation::from_field_with_origin(player_to_move, m.target);
                    if source.rank == 2 && target.rank == 4 {
                        self.possible_en_passant[pid] = Some(move_rank(source, true).unwrap().loc);
                    }
                }
                (_, PieceType::Rook) => {
                    for r in self.possible_rooks_for_castling[pid].iter_mut() {
                        if *r == Some(m.source) {
                            *r = None;
                        }
                    }
                }
                _ => {}
            },
            MoveType::Castle(_, _) => {
                self.possible_rooks_for_castling[pid] = Default::default();
            }
            MoveType::EnPassant(_, _) => {
                self.last_capture_or_pawn_move_index = self.move_index;
            }
            MoveType::ClaimDraw => {
                self.game_status = GameStatus::Draw(if self.fifty_move_rule_applies() {
                    DrawReason::FiftyMoveRule
                } else {
                    DrawReason::ThreefoldRepetition
                });
            }
            _ => (),
        }
        if self.is_king_capturable(None) {
            // PERF: don't do full movegen here, one legal move suffices
            if self.gen_moves().is_empty() {
                let next_player = get_next_hb(self.turn, true);
                let winner = if self.is_king_capturable(Some(next_player)) {
                    next_player
                } else {
                    player_to_move
                };
                self.game_status = GameStatus::Win(winner, WinReason::Checkmate(self.turn))
            }
        }
    }
    pub fn undo_move(&mut self, m: Move) {
        let src = usize::from(m.source);
        let tgt = usize::from(m.target);
        match m.move_type {
            MoveType::Slide => {
                self.board[src] = self.board[tgt];
                self.board[tgt] = Default::default();
            }
            MoveType::Capture(captured_piece) => {
                self.board[src] = self.board[tgt];
                self.board[tgt] = captured_piece;
            }
            MoveType::EnPassant(captured_pawn, captured_pawn_loc) => {
                self.board[usize::from(captured_pawn_loc)] = captured_pawn;
                self.board[src] = self.board[tgt];
                self.board[tgt] = Default::default();
            }
            MoveType::Castle(rook_source, rook_target) => {
                let king = self.board[tgt];
                let rook = self.board[usize::from(rook_target)];
                self.board[tgt] = Default::default();
                self.board[usize::from(rook_target)] = Default::default();
                self.board[src] = king;
                self.board[usize::from(rook_source)] = rook;
            }
            MoveType::Promotion(_) => {
                self.board[tgt] = Default::default();
                self.board[src] = FieldValue(Some((self.turn, PieceType::Pawn))).into();
            }
            MoveType::ClaimDraw => return,
        }
    }
    fn append_move_unless_check(&mut self, mv: Move, moves: &mut Vec<Move>) -> bool {
        self.make_move(mv);
        let would_be_check = self.is_king_capturable(None);
        self.undo_move(mv);
        if !would_be_check {
            moves.push(mv);
        }
        !would_be_check
    }
    fn gen_move_unless_check(
        &mut self,
        src: AnnotatedFieldLocation,
        tgt: AnnotatedFieldLocation,
        move_type: MoveType,
        moves: &mut Vec<Move>,
    ) -> bool {
        let m = Move {
            move_type,
            source: src.loc,
            target: tgt.loc,
        };
        self.append_move_unless_check(m, moves)
    }
    fn is_king_capturable_at(
        &self,
        kp: FieldLocation,
        cp: &CheckPossibilities,
        capturing_color: Option<Color>,
    ) -> bool {
        fn color_may_capture(
            tpc: &ThreePlayerChess,
            col: Color,
            capturing_color: Option<Color>,
        ) -> bool {
            col != tpc.turn && capturing_color.map(|cc| cc == col).unwrap_or(true)
        }
        let kp = AnnotatedFieldLocation::from_field(kp);
        for fields in [
            &cp.file[0..kp.rank as usize - 1],
            &cp.file[kp.rank as usize..ROW_SIZE],
            &cp.rank[0..kp.file as usize - 1],
            &cp.rank[kp.file as usize..ROW_SIZE],
        ] {
            for f in fields {
                let board_val = self.board[usize::from(*f)];
                match *FieldValue::from(board_val) {
                    None => continue,
                    Some((color, piece_type)) => match piece_type {
                        PieceType::Rook | PieceType::Queen
                            if color_may_capture(self, color, capturing_color) =>
                        {
                            return true
                        }
                        _ => break,
                    },
                }
            }
        }
        let mut line_start = 0;
        for line_end in cp.diagonal_line_ends {
            for f in &cp.diagonal_lines[line_start..line_end] {
                let board_val = self.board[usize::from(*f)];
                match *FieldValue::from(board_val) {
                    None => continue,
                    Some((color, piece_type)) => match piece_type {
                        PieceType::Bishop | PieceType::Queen
                            if color_may_capture(self, color, capturing_color) =>
                        {
                            return true
                        }
                        PieceType::Pawn if color_may_capture(self, color, capturing_color) => {
                            let pos = AnnotatedFieldLocation::from_field(*f);
                            if pos.rank + 1 == kp.reorient(pos.hb).rank {
                                return true;
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
                    PieceType::Knight if color_may_capture(self, color, capturing_color) => {
                        return true
                    }
                    _ => break,
                },
            }
        }
        false
    }
    fn is_king_capturable(&mut self, capturing_color: Option<Color>) -> bool {
        let cp = &self.check_possibilities[usize::from(self.turn)];
        let kp = self.king_positions[usize::from(self.turn)];
        self.is_king_capturable_at(kp, cp, capturing_color)
    }
    fn gen_move(
        &mut self,
        src: AnnotatedFieldLocation,
        tgt: AnnotatedFieldLocation,
        moves: &mut Vec<Move>,
    ) -> bool {
        let piece_value = self.get_packed_field_value(tgt.loc);
        match FieldValue::from(piece_value) {
            FieldValue(None) => self.gen_move_unless_check(src, tgt, MoveType::Slide, moves),
            FieldValue(Some((color, _))) if color != self.turn => {
                self.gen_move_unless_check(src, tgt, MoveType::Capture(piece_value), moves);
                false // we don't want to continue in this direction regardless of check
            }
            _ => false,
        }
    }
    fn gen_moves_rook(&mut self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
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
    fn gen_moves_bishop(&mut self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
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
    fn gen_moves_knight(&mut self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        let mut knight_moves = arrayvec::ArrayVec::new();
        get_knight_moves_for_field(field, &mut knight_moves);
        for m in knight_moves {
            self.gen_move(field, AnnotatedFieldLocation::from_field(m), moves);
        }
    }
    pub fn gen_move_castling(&mut self, short: bool) -> Option<Move> {
        let hb = self.turn;
        let rook_src = self.possible_rooks_for_castling[usize::from(hb)][short as usize]?;
        let rook_tgt =
            AnnotatedFieldLocation::from_file_and_rank(hb, hb, [3, 7][short as usize], 1);
        let king_tgt =
            AnnotatedFieldLocation::from_file_and_rank(hb, hb, [4, 6][short as usize], 1);
        for tgt in [king_tgt, rook_tgt] {
            if FieldValue::from(self.board[usize::from(tgt.loc)]).is_some() {
                return None;
            }
        }
        let king_src = AnnotatedFieldLocation::from_field(self.king_positions[usize::from(hb)]);
        let (fbegin, fend) = [
            (king_tgt.file, king_src.file - 1),
            (king_src.file + 1, king_tgt.file),
        ][short as usize];
        let mut conflict = false;
        self.board[usize::from(rook_tgt.loc)] = self.board[usize::from(rook_src)];
        self.board[usize::from(rook_src)] = FieldValue(None).into();
        let king_val = self.board[usize::from(king_src.loc)];
        self.board[usize::from(king_src.loc)] = FieldValue(None).into();
        for f in fbegin..fend {
            self.board[usize::from(rook_src)] = FieldValue(None).into();
            let kp = AnnotatedFieldLocation::from_file_and_rank(hb, hb, f, 1);
            let cp = CheckPossibilities::from_king_pos(kp.loc);
            self.board[usize::from(kp.loc)] = king_val;
            conflict = self.is_king_capturable_at(kp.loc, &cp, None);
            self.board[usize::from(kp.loc)] = FieldValue(None).into();
            if conflict {
                break;
            }
        }
        if conflict {
            return None;
        }
        self.board[usize::from(king_src.loc)] = king_val;
        self.board[usize::from(rook_src)] = self.board[usize::from(rook_tgt.loc)];
        self.board[usize::from(rook_tgt.loc)] = FieldValue(None).into();
        Some(Move {
            move_type: MoveType::Castle(rook_src, rook_tgt.loc),
            source: king_src.loc,
            target: king_tgt.loc,
        })
    }
    fn gen_king_slide_unless_check(
        &mut self,
        src: FieldLocation,
        tgt: FieldLocation,
    ) -> Option<Move> {
        if FieldValue::from(self.board[usize::from(tgt)]).is_some() {
            return None;
        }
        let cp = CheckPossibilities::from_king_pos(tgt);
        let mov = Move {
            move_type: MoveType::Slide,
            source: src,
            target: tgt,
        };
        self.make_move(mov);
        let would_be_check = self.is_king_capturable_at(tgt, &cp, None);
        self.undo_move(mov);
        if would_be_check {
            None
        } else {
            Some(mov)
        }
    }
    fn gen_moves_king(&mut self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        for tgt in [
            move_file(field, false),
            move_file(field, true),
            move_rank(field, false),
            move_rank(field, false),
        ] {
            if let Some(tgt) = tgt {
                self.gen_king_slide_unless_check(field.loc, tgt.loc)
                    .map(|mov| moves.push(mov));
            }
        }
        for right in [true, false] {
            for up in [true, false] {
                match move_diagonal(field, up, right) {
                    Some((one, two)) => {
                        for tgt in [Some(one), two] {
                            tgt.map(|tgt| {
                                self.gen_king_slide_unless_check(field.loc, tgt.loc)
                                    .map(|mov| moves.push(mov))
                            });
                        }
                    }
                    None => {}
                }
            }
        }
        for castle in [self.gen_move_castling(false), self.gen_move_castling(true)] {
            castle.map(|mov| moves.push(mov));
        }
    }
    fn try_gen_pawn_capture(
        &mut self,
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
                    && self.possible_en_passant[usize::from(tgt.hb)] == Some(tgt.loc)
                {
                    let ep_pawn_pos = move_rank(tgt, true).unwrap().loc;
                    self.gen_move_unless_check(
                        src,
                        tgt,
                        MoveType::EnPassant(self.board[usize::from(ep_pawn_pos)], ep_pawn_pos),
                        moves,
                    )
                } else {
                    false
                }
            }
            _ => false,
        }
    }
    fn gen_moves_pawn(&mut self, field: FieldLocation, moves: &mut Vec<Move>) {
        let src = AnnotatedFieldLocation::from_field_with_origin(self.turn, field);
        if let Some(up) = move_rank(src, true) {
            if let FieldValue(None) = self.get_field_value(up.loc) {
                self.gen_move_unless_check(src, up, MoveType::Slide, moves);
                if src.rank == 2 {
                    let up2 = move_rank(up, true).unwrap();
                    if let FieldValue(None) = self.get_field_value(up2.loc) {
                        self.gen_move_unless_check(src, up2, MoveType::Slide, moves);
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
    fn gen_moves_queen(&mut self, field: AnnotatedFieldLocation, moves: &mut Vec<Move>) {
        self.gen_moves_rook(field, moves);
        self.gen_moves_bishop(field, moves);
    }
    pub fn fifty_move_rule_applies(&self) -> bool {
        self.move_index >= self.last_capture_or_pawn_move_index + 50 * HB_COUNT as u16
    }
    pub fn threefold_repetition_applies(&self) -> bool {
        false //TODO: implement this
    }
    pub fn gen_moves_for_field(&mut self, field: FieldLocation, moves: &mut Vec<Move>) {
        match FieldValue::from(self.board[usize::from(field)]) {
            FieldValue(Some((color, piece_type))) if color == self.turn => {
                let field = field;
                let field_a = AnnotatedFieldLocation::from_field(field);
                match piece_type {
                    PieceType::Pawn => self.gen_moves_pawn(field, moves),
                    PieceType::Knight => self.gen_moves_knight(field_a, moves),
                    PieceType::Bishop => self.gen_moves_bishop(field_a, moves),
                    PieceType::Rook => self.gen_moves_rook(field_a, moves),
                    PieceType::Queen => self.gen_moves_queen(field_a, moves),
                    PieceType::King => self.gen_moves_king(field_a, moves),
                }
            }
            _ => {}
        }
    }
    pub fn gen_moves(&mut self) -> Vec<Move> {
        let mut moves = Vec::new();
        if self.game_status != GameStatus::Ongoing {
            return moves;
        }
        for i in 0..self.board.len() {
            self.gen_moves_for_field(FieldLocation::from(i), &mut moves)
        }
        if self.fifty_move_rule_applies() || self.threefold_repetition_applies() {
            moves.push(Move {
                move_type: MoveType::ClaimDraw,
                source: Default::default(),
                target: Default::default(),
            });
        }
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
        let moves = self.gen_moves();
        for candidate_move in moves {
            if mov == candidate_move {
                return true;
            }
        }
        false
    }
}
