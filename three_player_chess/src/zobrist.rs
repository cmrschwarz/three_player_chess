use crate::board::*;
use rand::prelude::StdRng;

const PIECE_TYPE_CASTLABLE_ROOK: usize = PIECE_COUNT;
const PIECE_TYPE_EN_PASSENT_SQUARE: usize = PIECE_COUNT + 1;

#[derive(Clone, PartialEq, Eq)]
pub struct ZobristData {
    field_values: [[u64; (PIECE_COUNT + 2) * HB_COUNT]; BOARD_SIZE],
    turn_values: [u64; HB_COUNT],
}
lazy_static! {
    static ref ZOBRIST_DATA: ZobristData = ZobristData {
        field_values: {
            let mut rng = StdRng::seed_from_u64(17);
            let mut zobrist_vals = [[0u64; (PIECE_COUNT + 2) * HB_COUNT]; BOARD_SIZE];
            for field in zobrist_vals.iter_mut() {
                for col in field.iter_mut() {
                    *col = rng.gen();
                }
            }
            zobrist_vals
        },
        turn_values: {
            let mut rng = StdRng::seed_from_u64(42);
            let mut zobrist_turns = [0; HB_COUNT];
            for t in zobrist_turns.iter_mut() {
                *t = rng.gen();
            }
            zobrist_turns
        }
    };
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ZobristHash {
    pub zobrist_data: &'static ZobristData, //since accessing a lazy static requires an atomic access, we cache this
    pub value: u64,                         //actual board hash
}

impl Default for ZobristHash {
    fn default() -> ZobristHash {
        ZobristHash {
            zobrist_data: &*ZOBRIST_DATA,
            value: 0,
        }
    }
}

impl ZobristHash {
    pub fn new(tpc: &ThreePlayerChess) -> ZobristHash {
        let mut zh = ZobristHash::default();
        zh.recalc_zobrist(tpc);
        zh
    }
    pub fn recalc_zobrist(&mut self, tpc: &ThreePlayerChess) -> u64 {
        let mut res = 0;
        for i in 0..BOARD_SIZE {
            if let Some((color, piece)) = *FieldValue::from(tpc.board[i]) {
                let mut piece_idx = usize::from(piece);
                piece_idx *= 1 + usize::from(color);
                res ^= self.zobrist_data.field_values[i][piece_idx];
            }
        }
        for c in Color::iter() {
            for cp in tpc.possible_rooks_for_castling[usize::from(*c)] {
                if let Some(loc) = cp {
                    res ^= self.zobrist_data.field_values[usize::from(loc)]
                        [PIECE_TYPE_CASTLABLE_ROOK * (1 + usize::from(*c))];
                }
            }
            if let Some(pos) = tpc.possible_en_passant[usize::from(*c)] {
                res ^= self.zobrist_data.field_values[usize::from(pos)]
                    [PIECE_TYPE_EN_PASSENT_SQUARE * (1 + usize::from(*c))];
            }
        }
        res ^= self.zobrist_data.turn_values[usize::from(tpc.turn)];
        res ^= (tpc.move_index - tpc.last_capture_or_pawn_move_index) as u64;
        self.value = res;
        res
    }

    pub fn toggle_square(&mut self, loc: FieldLocation, val: FieldValue) {
        let (color, piece) = val.0.unwrap();
        self.value ^= self.zobrist_data.field_values[usize::from(loc)]
            [usize::from(piece) * (usize::from(color) + 1)];
    }
    pub fn toggle_en_passent_square(&mut self, loc: FieldLocation) {
        self.value ^= self.zobrist_data.field_values[usize::from(loc)]
            [PIECE_TYPE_EN_PASSENT_SQUARE * (1 + usize::from(loc.hb()))];
    }
    pub fn toggle_possible_castling_rook(&mut self, loc: FieldLocation) {
        self.value ^= self.zobrist_data.field_values[usize::from(loc)]
            [PIECE_TYPE_CASTLABLE_ROOK * (1 + usize::from(loc.hb()))];
    }
    pub fn toggle_turn(&mut self, color: Color) {
        self.value ^= self.zobrist_data.turn_values[usize::from(color)];
    }
    pub fn next_turn(&mut self, turn_now: Color) {
        self.toggle_turn(turn_now.prev());
        self.toggle_turn(turn_now);
    }
    pub fn prev_turn(&mut self, turn_now: Color) {
        self.toggle_turn(turn_now.next());
        self.toggle_turn(turn_now);
    }
    pub fn toggle_fifty_move_rule(&mut self, move_index: u16, last_cap_or_pawn_move: u16) {
        self.value ^= (move_index - last_cap_or_pawn_move) as u64;
    }
    pub fn fifty_move_rule_move_inc(&mut self, move_index_new: u16, last_cap_or_pawn_move: u16) {
        self.toggle_fifty_move_rule(move_index_new - 1, last_cap_or_pawn_move);
        self.toggle_fifty_move_rule(move_index_new, last_cap_or_pawn_move);
    }
    pub fn fifty_move_rule_move_reset(
        &mut self,
        move_index_new: u16,
        last_cap_or_pawn_move_old: u16,
    ) {
        self.toggle_fifty_move_rule(move_index_new - 1, last_cap_or_pawn_move_old);
        self.toggle_fifty_move_rule(move_index_new, move_index_new);
    }
}
