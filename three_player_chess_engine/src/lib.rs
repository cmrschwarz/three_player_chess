#[macro_use]
extern crate lazy_static;

use std::cmp::Ordering;
use std::collections::btree_map::VacantEntry;

use three_player_chess::board::PieceType::*;
use three_player_chess::board::*;
use three_player_chess::movegen::get_next_hb;

type Eval: i16;
type Score = [i16; HB_COUNT];

const EVAL_WIN: Eval = 15000;
const EVAL_DRAW: Eval = 0;
const EVAL_NEUTRAL: Eval = -5000;
const EVAL_LOSS: Eval = -10000;

const SCORE_ALL_LOSE: Score = [EVAL_LOSS; HB_COUNT];

const PIECE_TYPE_CASTLABLE_ROOK: usize = PIECE_COUNT;
const PIECE_TYPE_EN_PASSENT_SQUARE: usize = PIECE_COUNT + 1;

fn piece_score(pt: PieceType) -> Eval {
    match pt {
        Pawn => 100,
        Knight => 300,
        Bishop => 400,
        Rook => 500,
        Queen => 900,
        King => 0,
    }
}

fn evaluate_position(tpc: &ThreePlayerChess) -> Score {
    match tpc.game_status {
        GameStatus::Draw(_) => [EVAL_DRAW; HB_COUNT],
        GameStatus::Win(winner, win_reason) => {
            let mut score = [0 as Eval; HB_COUNT];
            let windex = usize::from(winner);
            score[windex] = EVAL_WIN;
            match win_reason {
                WinReason::DoubleResign => {
                    score[(windex + 1) % 3] = EVAL_LOSS;
                    score[(windex + 2) % 3] = EVAL_LOSS;
                }
                WinReason::Checkmate(p) => {
                    score[usize::from(p)] = EVAL_LOSS;
                    let mut neutral = if get_next_hb(winner, true) == p {
                        windex + 1
                    } else {
                        windex + 2
                    };
                    score[neutral % 3] = EVAL_NEUTRAL;
                }
            }
            score
        }
        GameStatus::Ongoing => {
            let mut board_score = [0; HB_COUNT];
            for v in tpc.board {
                if let Some((color, piece_type)) = *FieldValue::from(v) {
                    board_score[usize::from(color)] += piece_score(piece_type);
                }
            }
            let mut score = [0; HB_COUNT];
            for i in 0..HB_COUNT {
                score[i] = 2 * board_score[i] - board_score[(i + 1) % 3] - board_score[(i + 2) % 3];
            }
            score
        }
    }
}

lazy_static! {
    static ref ZOBRIST_VALS: [[u64; (PIECE_COUNT + 2) * HB_COUNT]; BOARD_SIZE] = {
        let mut zobrist_vals = [[0u64; (PIECE_COUNT + 2) * HB_COUNT]; BOARD_SIZE];
        for field in zobrist_vals.iter_mut() {
            for col in field.iter_mut() {
                *col = rand::random::<u64>();
            }
        }
        zobrist_vals
    };
    static ref ZOBRIST_TURNS: [u64; HB_COUNT] = {
        let mut zobrist_turns = [0; HB_COUNT];
        for t in zobrist_turns.iter_mut() {
            *t = rand::random::<u64>();
        }
        zobrist_turns
    };
}

fn hash_board(tpc: &ThreePlayerChess) -> u64 {
    let zvs = &*ZOBRIST_VALS;
    let mut res = 0;
    for i in 0..BOARD_SIZE {
        if let Some((color, piece)) = *FieldValue::from(tpc.board[i]) {
            let mut piece_idx = usize::from(piece);
            if piece == Rook {
                for pr in tpc.possible_rooks_for_castling[usize::from(color)] {
                    if pr == Some(FieldLocation::from(i)) {
                        piece_idx = PIECE_TYPE_CASTLABLE_ROOK;
                        break;
                    }
                }
            }
            piece_idx *= 1 + usize::from(color);
            res ^= zvs[i][piece_idx];
        }
    }
    for c in Color::iter() {
        if let Some(pos) = tpc.possible_en_passant[usize::from(*c)] {
            res ^= zvs[usize::from(pos)][PIECE_TYPE_EN_PASSENT_SQUARE * (1 + usize::from(*c))];
        }
    }
    res ^= (tpc.move_index - tpc.last_capture_or_pawn_move_index) as u64;
    res ^= ZOBRIST_TURNS[usize::from(tpc.turn)];
    res ^= ZOBRIST_TURNS[usize::from(tpc.turn)];
    res
}

#[derive(Copy, Clone, Default, Eq, PartialEq)]
struct Transposition {
    score: Score,
    eval_depth: u16,
}

struct Engine {
    transposition_table: std::collections::HashMap<u64, Transposition>,
    engine_stack: Vec<EngineDepth>,
    board: ThreePlayerChess,
}
struct EngineDepth {
    moves: Vec<EngineMove>,
    index: usize,
    move_rev: Option<ReversableMove>,
    achieved_eval: Score,
}

struct EngineMove {
    score: Score,
    mov: Move,
    hash: u64,
}

enum PromotionResult {
    Pruned,
    Ok,
}

impl Engine {
    fn search_start(&mut self, tpc: &ThreePlayerChess) {
        self.board = tpc.clone();
        self.transposition_table.clear();
    }
    fn evaluate_stable(&mut self, hash: u64, depth: u16) -> Score {
        //TODO: don't evaluate unstable positions
        evaluate_position(&mut self.board)
    }
    fn gen_engine_depth(&mut self, depth: u16) -> &EngineDepth {
        let mut ed = if self.engine_stack.len() == depth as usize {
            self.engine_stack.push(EngineDepth {
                moves: Vec::new(),
                index: 0,
                move_rev: None,
                achieved_eval: SCORE_ALL_LOSE,
            });
            self.engine_stack.last().unwrap()
        } else {
            let ed = &self.engine_stack[depth as usize];
            assert!(ed.moves.is_empty());
            ed.index = 0;
            ed.achieved_eval = SCORE_ALL_LOSE;
            ed
        };
        let moves = self.board.gen_moves();
        ed.moves.reserve(moves.len());
        for m in moves {
            let rm = ReversableMove::new(&self.board, m);
            self.board.perform_move(rm.mov);
            let hash = hash_board(&self.board);
            self.board.revert_move(&rm);
            let score;
            let eval_depth;
            if let std::collections::hash_map::Entry::Occupied(e) =
                self.transposition_table.entry(hash)
            {
                let tt = e.get();
                score = tt.score;
                eval_depth = tt.eval_depth;
            } else {
                score = Default::default();
                eval_depth = 0;
            }
            ed.moves.push(EngineMove {
                score,
                mov: rm.mov,
                hash: hash,
            });
        }
        let pid = usize::from(self.board.turn);
        ed.moves
            .sort_by(|m_l, m_r| m_r.score[pid].cmp(&m_l.score[pid]));
        ed
    }
    fn propagate_move_score(
        &mut self,
        depth: u16,
        eval_depth: u16,
        score: Score,
    ) -> PromotionResult {
        let player_to_move = usize::from(self.board.turn);
        let other_players = [(player_to_move + 1) % 3, (player_to_move + 2) % 3];
        let parent_ed = &self.engine_stack[(depth - 1) as usize];
        let parent_em = &parent_ed.moves[parent_ed.index];
        let mut worse_for_others = false;
        for i in other_players {
            if score[i] < parent_em.score[i] {
                parent_em.score[i] = score[i];
                if parent_ed.achieved_eval[i] > score[i] {
                    if worse_for_others {
                        // if this branch is already worse than another option
                        // for all other players, there's no need to
                        // look for an even better move here, since
                        // it won't change the final evaluation
                        return PromotionResult::Pruned;
                    }
                    worse_for_others = true;
                }
            }
        }
        if score[player_to_move] > parent_em.score[player_to_move] {
            parent_em.score[player_to_move] = score[player_to_move];
            if score[player_to_move] > parent_ed.achieved_eval[player_to_move] {
                parent_ed.achieved_eval[player_to_move] = score[player_to_move];
            }
        }
        let tte = self.transposition_table.entry(parent_em.hash).or_default();
        tte.score = score;
        tte.eval_depth = eval_depth;
        PromotionResult::Ok
    }
    fn apply_move_score(&mut self) {}
    fn search_iterate(&mut self, depth_max: u16) {
        self.gen_engine_depth(0);
        let mut depth = 1;
        loop {
            let mut ed = &self.engine_stack[depth as usize];
            while ed.index == ed.moves.len() {
                if depth == 0 {
                    break;
                }
                self.propagate_move_score(depth, depth_max, ed.achieved_eval);
                ed.moves.clear();
                ed.move_rev.map(|mr| self.board.revert_move(&mr));
                depth -= 1;
                ed = &self.engine_stack[depth as usize];
            }
            let em = ed.moves[ed.index];
            ed.index += 1;
            let rm = ReversableMove::new(&self.board, em.mov);
            ed.move_rev = Some(rm);
            self.board.perform_move(em.mov);
            depth += 1;
            if depth == depth_max {
                em.score = self.evaluate_stable(em.hash, depth);
                self.propagate_move_score(depth, depth_max, ed.achieved_eval);
                depth -= 1;
                self.board.revert_move(&rm);
            } else {
                self.gen_engine_depth(depth);
            }
        }
    }
}
