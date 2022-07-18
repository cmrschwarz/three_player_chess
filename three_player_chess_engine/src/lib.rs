#[macro_use]
extern crate lazy_static;

mod eval;

use eval::evaluate_position;
use std::time::{Duration, Instant};
use three_player_chess::board::MoveType::*;
use three_player_chess::board::PieceType::*;
use three_player_chess::board::*;
use three_player_chess::movegen::get_next_hb;

type Eval = i16;
type Score = [i16; HB_COUNT];

const EVAL_WIN: Eval = 15000;
const EVAL_DRAW: Eval = 0;
const EVAL_NEUTRAL: Eval = -5000;
const EVAL_LOSS: Eval = -10000;
const FORCE_EVAL_COST: Eval = 1000;

const SCORE_ALL_LOSE: Score = [EVAL_LOSS; HB_COUNT];

const PIECE_TYPE_CASTLABLE_ROOK: usize = PIECE_COUNT;
const PIECE_TYPE_EN_PASSENT_SQUARE: usize = PIECE_COUNT + 1;
const MAX_UNSTABLE_LINE_DEPTH: u16 = 3;

#[derive(Copy, Clone, Eq, PartialEq)]
struct Transposition {
    score: Score,
    eval_depth: u16,
    best_move_code: u64,
}

pub struct Engine {
    transposition_table: std::collections::HashMap<u64, Transposition>,
    engine_stack: Vec<EngineDepth>,
    board: ThreePlayerChess,
    depth_max: u16,
    eval_depth_max: u16,
    transposition_count: usize,
    prune_count: usize,
    pos_count: usize,
    deciding_player: usize,
}

#[derive(Default)]
struct EngineDepth {
    moves: Vec<EngineMove>,
    hash: u64,
    index: usize,
    score: Score,
    best_move: Option<Move>,
    move_rev: Option<ReversableMove>,
}

struct EngineMove {
    hash: u64,
    mov: Move,
    eval: Eval,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd)]
enum PropagationResult {
    Pruned(u8),
    Ok,
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

impl Transposition {
    fn new(mov: Option<Move>, score: Score, eval_depth: u16) -> Transposition {
        Transposition {
            eval_depth,
            score: score,
            best_move_code: mov.map_or(0, &u64::from),
        }
    }
}

impl PropagationResult {
    fn prune_depth(self) -> u8 {
        match self {
            PropagationResult::Ok => 0,
            PropagationResult::Pruned(pr) => pr,
        }
    }
}

impl Default for Transposition {
    fn default() -> Self {
        Transposition::new(None, SCORE_ALL_LOSE, 0)
    }
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
    res ^= ZOBRIST_TURNS[usize::from(tpc.turn)];
    res ^= ZOBRIST_TURNS[usize::from(tpc.turn)];
    res ^= (tpc.move_index - tpc.last_capture_or_pawn_move_index) as u64;
    res
}

fn get_initial_score(player_to_move: Color) -> Score {
    let pid = usize::from(player_to_move);
    let mut score = Score::default();
    score[pid] = EVAL_LOSS;
    score[(pid + 1) % 3] = EVAL_WIN;
    score[(pid + 2) % 3] = EVAL_WIN;
    score
}

fn get_initial_pos_eval_for_sort(tpc: &mut ThreePlayerChess, mov: Move) -> i16 {
    let mut sc = 0;
    if tpc.is_king_capturable(None) {
        sc += 99;
    }
    sc += match mov.move_type {
        ClaimDraw | SlideClaimDraw => 90,
        CapturePromotion(..) => 70,
        Promotion(_) => 60,
        Capture(_) | EnPassant(..) => 50,
        Castle(..) => 40,
        Slide => EVAL_DRAW,
    };
    sc
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            transposition_table: Default::default(),
            board: Default::default(),
            engine_stack: Default::default(),
            depth_max: 0,
            eval_depth_max: 0,
            prune_count: 0,
            transposition_count: 0,
            pos_count: 0,
            deciding_player: 0,
        }
    }
    pub fn search_position(
        &mut self,
        tpc: &ThreePlayerChess,
        depth: u16,
        max_time_seconds: f32,
    ) -> Option<Move> {
        self.board = tpc.clone();
        self.eval_depth_max = self.board.move_index;
        self.depth_max = 0;
        //self.transposition_table.retain(|_, tp| tp.eval_depth > self.eval_depth_max);
        self.transposition_table.clear(); // since we use
        self.transposition_count = 0;
        self.prune_count = 0;
        self.pos_count = 0;
        self.deciding_player = usize::from(self.board.turn);
        let end = Instant::now()
            .checked_add(Duration::from_secs_f32(max_time_seconds))
            .unwrap();
        let mut bm = self.board.gen_moves().get(0).map(|m| *m);
        let mut bm_str = bm.map_or("".to_owned(), |bm| {
            bm.to_string(&mut self.board).as_str().to_owned()
        });
        for _ in 0..depth {
            self.depth_max += 1;
            self.eval_depth_max += 1;
            if self.search_iterate(end).is_ok() {
                bm = self.engine_stack[0].best_move;
                bm_str = self.transposition_line_str(bm);
            } else {
                self.depth_max -= 1;
                break;
            }
        }
        println!(
            "evaluated {} positions (depth {}), pruned {} branches, skipped {} transpositions, result: {}",
            self.pos_count, self.depth_max , self.prune_count, self.transposition_count, bm_str
        );

        return bm;
    }
    fn gen_engine_depth(&mut self, depth: u16, rm: Option<ReversableMove>) -> &EngineDepth {
        let parent_hash = if depth > 0 {
            let parent = &self.engine_stack[depth as usize - 1];
            parent.moves[parent.index - 1].hash
        } else {
            hash_board(&mut self.board)
        };
        let ed = if self.engine_stack.len() == depth as usize {
            self.engine_stack.push(Default::default());
            self.engine_stack.last_mut().unwrap()
        } else {
            let ed = &mut self.engine_stack[depth as usize];
            ed.index = 0;
            ed.best_move = None;
            ed.moves.clear();
            ed
        };
        ed.hash = parent_hash;
        ed.move_rev = rm;
        ed.score = get_initial_score(self.board.turn);
        let moves = self.board.gen_moves();
        ed.moves.reserve(moves.len());
        let pid = usize::from(self.board.turn);
        for mov in moves {
            let rm = ReversableMove::new(&self.board, mov);
            self.board.perform_move(rm.mov);
            let hash = hash_board(&self.board);
            let eval = self.transposition_table.get(&hash).map_or_else(
                || get_initial_pos_eval_for_sort(&mut self.board, mov),
                |tp| tp.score[pid],
            );
            self.board.revert_move(&rm);
            ed.moves.push(EngineMove { eval, hash, mov });
        }
        ed.moves.sort_by(|m_l, m_r| m_r.eval.cmp(&m_l.eval));
        ed
    }
    fn propagate_move_score(
        &mut self,
        depth: u16,
        mov: Option<Move>,
        score: Score,
    ) -> PropagationResult {
        let depth = depth as usize;
        let mut result = PropagationResult::Ok;
        let player_to_move = usize::from(self.board.turn);
        let ed = &mut self.engine_stack[depth as usize];
        if score[player_to_move] > ed.score[player_to_move] {
            ed.score[player_to_move] = score[player_to_move];
            ed.best_move = mov;
        }
        let mut score_modified = false;
        let mut ed_score = ed.score;
        for i in 0..2 {
            let pp = (player_to_move + 2 - i) % 3;
            if score[pp] < ed_score[pp] {
                ed_score[pp] = score[pp];
                score_modified = true;
                if pp == self.deciding_player  // only prune based on decisions we can actually make
                   && depth as usize > i
                   && self.engine_stack[depth  - i - 1].score[pp] > score[pp]
                {
                    result = PropagationResult::Pruned(i as u8 + 1);
                }
            }
        }
        if player_to_move == self.deciding_player {
            if depth >= 3
                && self.engine_stack[depth - 3].score[player_to_move] > score[player_to_move]
            {
                result = PropagationResult::Pruned(3);
            }
        }
        if score_modified {
            self.engine_stack[depth].score = ed_score;
        }
        if result != PropagationResult::Ok {
            self.prune_count += 1;
        }
        result
    }
    pub fn engine_line_str(&mut self, depth: u16, last_move: Option<&ReversableMove>) -> String {
        let mut res = String::new();
        let mut depth = depth as usize;
        if let Some(ref rm) = last_move {
            depth -= 1;
            self.board.revert_move(&rm);
            res += rm.mov.to_string(&mut self.board).as_str();
        }
        for i in 0..depth {
            let rm = self.engine_stack[depth - i].move_rev.clone().unwrap();
            self.board.revert_move(&rm);
            let mut mov_str = rm.mov.to_string(&mut self.board).as_str().to_owned();
            mov_str.push_str(" ");
            mov_str.push_str(res.as_str());
            res = mov_str;
        }
        for i in 1..depth + 1 {
            let rm = self.engine_stack[i].move_rev.clone().unwrap();
            self.board.perform_move(rm.mov);
        }
        if let Some(ref rm) = last_move {
            self.board.perform_move(rm.mov);
        }
        res
    }
    fn transposition_line_str(&mut self, mov: Option<Move>) -> String {
        let mut res = String::new();
        let mut board = self.board.clone();
        let mut mov = mov;
        loop {
            if let Some(mov) = mov {
                res.push_str(mov.to_string(&mut board).as_str());
                board.perform_move(mov);
            }
            if let Some(tp) = self.transposition_table.get(&hash_board(&board)) {
                if mov.is_some() {
                    let turn = usize::from(get_next_hb(board.turn, false));
                    let tp_eval = tp.score[turn];
                    let board_eval = evaluate_position(&mut board, true).unwrap()[turn];
                    res += &format_args!(" ({} / {})", tp_eval, board_eval).to_string();
                }
                mov = Move::try_from(tp.best_move_code).ok();
                if mov.is_none() {
                    break;
                }
                if tp.best_move_code == 0 {
                    break;
                }
            } else {
                break;
            }
            res.push(' ');
        }
        res
    }
    fn search_iterate(&mut self, end: Instant) -> Result<(), ()> {
        self.gen_engine_depth(0, None);
        let mut depth = 0;
        let mut propagation_result = PropagationResult::Ok;
        loop {
            let mut ed = &mut self.engine_stack[depth as usize];
            while ed.index == ed.moves.len() || propagation_result != PropagationResult::Ok {
                self.transposition_table.insert(
                    ed.hash,
                    Transposition::new(ed.best_move, ed.score, self.eval_depth_max),
                );
                if depth == 0 {
                    return Ok(());
                }
                let rm = ed.move_rev.take().unwrap();
                self.board.revert_move(&rm);
                depth -= 1;
                ed.moves.clear();
                let score = ed.score;
                let prev_prune_depth = propagation_result.prune_depth();
                propagation_result = self.propagate_move_score(depth, Some(rm.mov), score);
                if prev_prune_depth > 1 && propagation_result.prune_depth() < prev_prune_depth - 1 {
                    propagation_result = PropagationResult::Pruned(prev_prune_depth - 1);
                }
                ed = &mut self.engine_stack[depth as usize];
            }
            let em = &mut ed.moves[ed.index];
            ed.index += 1;

            if let Some(tp) = self.transposition_table.get(&em.hash) {
                if tp.eval_depth >= self.eval_depth_max {
                    let mov = em.mov;
                    self.transposition_count += 1;
                    propagation_result = self.propagate_move_score(depth, Some(mov), tp.score);
                    continue;
                }
            }
            let rm = ReversableMove::new(&self.board, em.mov);

            self.board.perform_move(em.mov);
            depth += 1;
            if depth >= self.depth_max || self.board.game_status != GameStatus::Ongoing {
                let score = evaluate_position(
                    &mut self.board,
                    depth > self.depth_max + MAX_UNSTABLE_LINE_DEPTH,
                );

                if let Some(score) = score {
                    let hash = em.hash;
                    self.pos_count += 1;
                    self.board.revert_move(&rm);
                    depth -= 1;
                    propagation_result = self.propagate_move_score(depth, Some(rm.mov), score);
                    self.transposition_table
                        .insert(hash, Transposition::new(None, score, self.eval_depth_max));
                    if propagation_result == PropagationResult::Ok {
                        if score[usize::from(self.board.turn)] >= EVAL_WIN {
                            self.prune_count += 1;
                            propagation_result = PropagationResult::Pruned(1);
                        }
                    }
                    continue;
                }
            }
            if Instant::now().gt(&end) {
                return Err(());
            }
            self.gen_engine_depth(depth, Some(rm));
        }
    }
}
