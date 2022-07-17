#[macro_use]
extern crate lazy_static;

use three_player_chess::board::PieceType::*;
use three_player_chess::board::*;
use three_player_chess::movegen::get_next_hb;

type Eval = i16;
type Score = [i16; HB_COUNT];

const EVAL_WIN: Eval = 15000;
const EVAL_DRAW: Eval = 0;
const EVAL_NEUTRAL: Eval = -5000;
const EVAL_LOSS: Eval = -10000;

const SCORE_ALL_LOSE: Score = [EVAL_LOSS; HB_COUNT];

const PIECE_TYPE_CASTLABLE_ROOK: usize = PIECE_COUNT;
const PIECE_TYPE_EN_PASSENT_SQUARE: usize = PIECE_COUNT + 1;

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
    transposition_count: usize,
    prune_count: usize,
    pos_count: usize,
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
}

#[derive(PartialEq, Eq, PartialOrd)]
enum PropagationResult {
    Pruned,
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

impl Transposition {
    fn new(mov: Option<Move>, score: Score, eval_depth: u16) -> Transposition {
        Transposition {
            eval_depth,
            score: score,
            best_move_code: mov.map_or(0, &u64::from),
        }
    }
}

impl Default for Transposition {
    fn default() -> Self {
        Transposition::new(None, SCORE_ALL_LOSE, 0)
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
                    let neutral = if get_next_hb(winner, true) == p {
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

impl Engine {
    pub fn new() -> Engine {
        Engine {
            transposition_table: Default::default(),
            board: Default::default(),
            engine_stack: Default::default(),
            depth_max: 0,
            prune_count: 0,
            transposition_count: 0,
            pos_count: 0,
        }
    }
    pub fn search_position(&mut self, tpc: &ThreePlayerChess, depth: u16) -> Option<Move> {
        self.board = tpc.clone();
        if depth == 0 {
            return self.board.gen_moves().get(0).map(|m| *m);
        }
        self.transposition_table.clear();
        self.transposition_count = 0;
        self.prune_count = 0;
        self.pos_count = 0;
        for i in 1..depth + 1 {
            self.depth_max = i;
            self.search_iterate()
        }
        let bm = self.engine_stack[0].best_move;
        let bm_str = self.transposition_line_str(bm);
        println!(
            "evaluated {} positions, pruned {} branches, skipped {} transpositions, result: {}",
            self.pos_count, self.prune_count, self.transposition_count, bm_str
        );
        return bm;
    }
    fn evaluate_stable(&mut self, _depth: u16) -> Score {
        //TODO: don't evaluate unstable positions
        self.pos_count += 1;
        evaluate_position(&mut self.board)
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
            ed
        };
        ed.hash = parent_hash;
        ed.move_rev = rm;
        ed.score = get_initial_score(self.board.turn);
        let moves = self.board.gen_moves();
        ed.moves.reserve(moves.len());
        for m in moves {
            let rm = ReversableMove::new(&self.board, m);
            self.board.perform_move(rm.mov);
            let hash = hash_board(&self.board);
            self.board.revert_move(&rm);
            ed.moves.push(EngineMove {
                mov: rm.mov,
                hash: hash,
            });
        }
        let pid = usize::from(self.board.turn);
        ed.moves.sort_by(|m_l, m_r| {
            let score_l = self
                .transposition_table
                .get(&m_l.hash)
                .map_or(EVAL_DRAW, |tp| tp.score[pid]);
            let score_r = self
                .transposition_table
                .get(&m_r.hash)
                .map_or(EVAL_DRAW, |tp| tp.score[pid]);
            score_r.cmp(&score_l)
        });
        ed
    }
    fn propagate_move_score(
        &mut self,
        depth: u16,
        mov: Option<Move>,
        score: Score,
    ) -> PropagationResult {
        let player_to_move = usize::from(self.board.turn);
        let other_players = [(player_to_move + 1) % 3, (player_to_move + 2) % 3];
        let grandparent_score = if depth > 1 {
            self.engine_stack[depth as usize - 1].score
        } else {
            SCORE_ALL_LOSE
        };
        let ed = &mut self.engine_stack[depth as usize];
        let mut worse_for_others = false;
        for i in other_players {
            if score[i] < ed.score[i] {
                ed.score[i] = score[i];
                if score[i] < grandparent_score[i] {
                    if worse_for_others {
                        // if this branch is already worse than another option
                        // for all other players, there's no need to
                        // look for an even better move here, since
                        // it won't change the final evaluation
                        self.prune_count += 1;
                        return PropagationResult::Pruned;
                    }
                    worse_for_others = true;
                }
            }
        }
        if score[player_to_move] > ed.score[player_to_move] {
            ed.score[player_to_move] = score[player_to_move];
            ed.best_move = mov;
        }
        PropagationResult::Ok
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
                if tp.best_move_code == 0 {
                    break;
                }
                mov = Some(Move::try_from(tp.best_move_code).unwrap());
            } else {
                break;
            }
            res.push(' ');
        }
        res
    }
    fn search_iterate(&mut self) {
        self.gen_engine_depth(0, None);
        let mut depth = 0;
        let mut propagation_result = PropagationResult::Ok;
        loop {
            let mut ed = &mut self.engine_stack[depth as usize];
            while ed.index == ed.moves.len() || propagation_result == PropagationResult::Pruned {
                self.transposition_table.insert(
                    ed.hash,
                    Transposition::new(ed.best_move, ed.score, self.depth_max),
                );
                if depth == 0 {
                    return;
                }
                let rm = ed.move_rev.take().unwrap();
                self.board.revert_move(&rm);
                depth -= 1;
                ed.moves.clear();
                let score = ed.score;
                propagation_result = self.propagate_move_score(depth, Some(rm.mov), score);
                ed = &mut self.engine_stack[depth as usize];
            }
            let em = &mut ed.moves[ed.index];
            ed.index += 1;

            if let Some(tp) = self.transposition_table.get(&em.hash) {
                if tp.eval_depth >= self.depth_max {
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
                let score = self.evaluate_stable(depth);
                self.pos_count += 1;
                // println!("evaluating {}: {:?}", self.line_str(depth, Some(&rm)), score);
                self.board.revert_move(&rm);
                depth -= 1;
                propagation_result = self.propagate_move_score(depth, Some(rm.mov), score);
                if propagation_result != PropagationResult::Pruned {
                    if score[usize::from(self.board.turn)] == EVAL_WIN {
                        self.prune_count += 1;
                        propagation_result = PropagationResult::Pruned;
                    }
                }
            } else {
                self.gen_engine_depth(depth, Some(rm));
            }
        }
    }
}
