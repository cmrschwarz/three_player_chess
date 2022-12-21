use three_player_chess::board::PieceType::*;
use three_player_chess::board::*;

pub type Eval = i16;
pub type Score = [i16; HB_COUNT];
pub const EVAL_WIN: Eval = 15000;
pub const EVAL_DRAW: Eval = 0;
pub const EVAL_NEUTRAL: Eval = -5000;
pub const EVAL_LOSS: Eval = -10000;
pub const EVAL_MAX: Eval = Eval::MAX;

// e.g. two rooks and three pawns per side, just a heuristic
const ENDGAME_MATERIAL_THRESHOLD: i16 = 1300 * HB_COUNT as i16;

const FIELD_BONUS_PAWN: [[i16; ROW_SIZE]; ROW_SIZE] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [50, 50, 50, 50, 50, 50, 50, 50],
    [10, 10, 20, 30, 30, 20, 10, 10],
    [5, 5, 10, 25, 25, 10, 5, 5],
    [0, 0, 0, 20, 20, 0, 0, 0],
    [5, -5, -10, 0, 0, -10, -5, 5],
    [5, 10, 10, -20, -20, 10, 10, 5],
    [0, 0, 0, 0, 0, 0, 0, 0],
];

const FIELD_BONUS_KNIGHT: [[i16; ROW_SIZE]; ROW_SIZE] = [
    [-50, -40, -30, -30, -30, -30, -40, -50],
    [-40, -20, 0, 0, 0, 0, -20, -40],
    [-30, 0, 10, 15, 15, 10, 0, -30],
    [-30, 5, 15, 20, 20, 15, 5, -30],
    [-30, 0, 15, 20, 20, 15, 0, -30],
    [-30, 5, 10, 15, 15, 10, 5, -30],
    [-40, -20, 0, 5, 5, 0, -20, -40],
    [-50, -40, -30, -30, -30, -30, -40, -50],
];

const FIELD_BONUS_BISHOP: [[i16; ROW_SIZE]; ROW_SIZE] = [
    [-20, -10, -10, -10, -10, -10, -10, -20],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 10, 10, 5, 0, -10],
    [-10, 5, 5, 10, 10, 5, 5, -10],
    [-10, 0, 10, 10, 10, 10, 0, -10],
    [-10, 10, 10, 10, 10, 10, 10, -10],
    [-10, 5, 0, 0, 0, 0, 5, -10],
    [-20, -10, -10, -10, -10, -10, -10, -20],
];

const FIELD_BONUS_ROOK: [[i16; ROW_SIZE]; ROW_SIZE] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [5, 10, 10, 10, 10, 10, 10, 5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [0, 0, 0, 5, 5, 0, 0, 0],
];

const FIELD_BONUS_QUEEN: [[i16; ROW_SIZE]; ROW_SIZE] = [
    [-20, -10, -10, -5, -5, -10, -10, -20],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 5, 5, 5, 0, -10],
    [-5, 0, 5, 5, 5, 5, 0, -5],
    [0, 0, 5, 5, 5, 5, 0, -5],
    [-10, 5, 5, 5, 5, 5, 0, -10],
    [-10, 0, 5, 0, 0, 0, 0, -10],
    [-20, -10, -10, -5, -5, -10, -10, -20],
];

const CASTLING_AVAILABLE_BONUS: i16 = 5;
const FIELD_BONUS_KING_MIDDLEGAME: [[i16; ROW_SIZE]; ROW_SIZE] = [
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [30, -40, -40, -50, -50, -40, -40, -30],
    [30, -40, -40, -50, -50, -40, -40, -30],
    [30, -40, -40, -50, -50, -40, -40, -30],
    [20, -30, -30, -40, -40, -30, -30, -20],
    [10, -20, -20, -20, -20, -20, -20, -10],
    [20, 20, 0, 0, 0, 0, 20, 20],
    [20, 30, 10, 0, 0, 10, 30, 20],
];

const FIELD_BONUS_KING_ENDGAME: [[i16; ROW_SIZE]; ROW_SIZE] = [
    [-50, -40, -30, -20, -20, -30, -40, -50],
    [-30, -20, -10, 0, 0, -10, -20, -30],
    [-30, -10, 20, 30, 30, 20, -10, -30],
    [-30, -10, 30, 40, 40, 30, -10, -30],
    [-30, -10, 30, 40, 40, 30, -10, -30],
    [-30, -10, 20, 30, 30, 20, -10, -30],
    [-30, -30, 0, 0, 0, 0, -30, -30],
    [-50, -30, -30, -30, -30, -30, -30, -50],
];

pub fn piece_score(pt: PieceType) -> Eval {
    match pt {
        Pawn => 100,
        Knight => 300,
        Bishop => 400,
        Rook => 500,
        Queen => 900,
        King => 0,
    }
}

fn get_indices_for_field_bonus(color: Color, loc: FieldLocation) -> (usize, usize) {
    let afl = AnnotatedFieldLocation::from_with_origin(color, loc);
    let f = afl.file as usize - 1;
    // the rank is flipped so the arrays look like a chess board in the code
    let r = ROW_SIZE - afl.rank as usize;
    (f, r)
}

fn add_location_score(score: &mut Score, loc: FieldLocation, piece_type: PieceType, color: Color) {
    let mut sc = piece_score(piece_type);
    let (f, r) = get_indices_for_field_bonus(color, loc);
    sc += match piece_type {
        Pawn => FIELD_BONUS_PAWN[r][f],
        Knight => FIELD_BONUS_KNIGHT[r][f],
        Bishop => FIELD_BONUS_BISHOP[r][f],
        Rook => FIELD_BONUS_ROOK[r][f],
        Queen => FIELD_BONUS_QUEEN[r][f],
        King => 0, // we must delay this because there are two different tables depending on the total piece score
    };
    score[usize::from(color)] += sc;
}

fn add_castling_scores(score: &mut Score, tpc: &mut ThreePlayerChess) {
    for c in Color::iter() {
        let ci = usize::from(*c);
        for cr in tpc.possible_rooks_for_castling[ci] {
            if cr.is_some() {
                score[ci] += CASTLING_AVAILABLE_BONUS;
            }
        }
    }
}

fn add_king_location_scores(score: &mut Score, tpc: &mut ThreePlayerChess) {
    let endgame = score.iter().sum::<i16>() <= ENDGAME_MATERIAL_THRESHOLD;
    for c in Color::iter() {
        let (f, r) = get_indices_for_field_bonus(*c, tpc.king_positions[usize::from(*c)]);
        score[usize::from(*c)] += if endgame {
            FIELD_BONUS_KING_ENDGAME[r][f]
        } else {
            FIELD_BONUS_KING_MIDDLEGAME[r][f]
        };
    }
}
pub fn board_has_captures(tpc: &mut ThreePlayerChess) -> bool {
    for i in 0..BOARD_SIZE {
        if let Some((color, _)) = *FieldValue::from(tpc.board[i]) {
            let loc = FieldLocation::from(i);
            if color != tpc.turn {
                if tpc
                    .is_piece_capturable_at(loc, Some(tpc.turn), true)
                    .is_some()
                {
                    return true;
                }
            }
        }
    }
    false
}

pub fn calculate_raw_board_score(tpc: &mut ThreePlayerChess) -> (Score, bool) {
    let mut captures_exist = false;
    let mut score: Score;
    match tpc.game_status {
        GameStatus::Draw(_) => score = [EVAL_DRAW; HB_COUNT],
        GameStatus::Win(winner, win_reason) => {
            // we encourage winning earlier and loosing later
            // by adding / subtracting the move index from the score
            score = [EVAL_NEUTRAL + tpc.move_index as i16; HB_COUNT];
            score[usize::from(winner)] = EVAL_WIN - tpc.move_index as i16;
            let (WinReason::Checkmate(looser) | WinReason::CapturableKing(looser)) = win_reason;
            score[usize::from(looser)] = EVAL_LOSS + tpc.move_index as i16;
        }
        GameStatus::Ongoing => {
            score = [0; HB_COUNT];
            for i in 0..BOARD_SIZE {
                if let Some((color, piece_type)) = *FieldValue::from(tpc.board[i]) {
                    let loc = FieldLocation::from(i);
                    add_location_score(&mut score, loc, piece_type, color);
                    if color != tpc.turn && !captures_exist {
                        captures_exist = tpc
                            .is_piece_capturable_at(loc, Some(tpc.turn), true)
                            .is_some();
                    }
                }
            }
            add_castling_scores(&mut score, tpc);
            add_king_location_scores(&mut score, tpc);
        }
    };
    (score, captures_exist)
}
pub fn calculate_position_score(tpc: &mut ThreePlayerChess) -> (Score, bool) {
    let (bs, caps_exist) = calculate_raw_board_score(tpc);
    let mut score: Score = [Default::default(); HB_COUNT];
    for i in 0..HB_COUNT {
        score[i] = bs[i]
            .saturating_add(bs[i])
            .saturating_sub(bs[(i + 1) % HB_COUNT])
            .saturating_sub(bs[(i + 2) % HB_COUNT]);
    }
    (score, caps_exist)
}
pub fn calculate_position_eval(tpc: &mut ThreePlayerChess, perspective: Color) -> (Eval, bool) {
    let (score, caps_exist) = calculate_raw_board_score(tpc);
    let p = usize::from(perspective);
    (
        2 * score[p] - score[(p + 1) % HB_COUNT] - score[(p + 2) % HB_COUNT],
        caps_exist,
    )
}
