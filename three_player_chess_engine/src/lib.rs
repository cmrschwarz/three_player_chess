#[macro_use]
extern crate lazy_static;

mod eval;

use eval::evaluate_position;
use std::time::{Duration, Instant};
use three_player_chess::board::MoveType::*;
use three_player_chess::board::PieceType::*;
use three_player_chess::board::*;

type Eval = i16;
type Score = [i16; HB_COUNT];

const EVAL_WIN: Eval = 15000;
const EVAL_DRAW: Eval = 0;
const EVAL_NEUTRAL: Eval = -5000;
const EVAL_LOSS: Eval = -10000;
const EVAL_MAX: Eval = Eval::MAX;

const PIECE_TYPE_CASTLABLE_ROOK: usize = PIECE_COUNT;
const PIECE_TYPE_EN_PASSENT_SQUARE: usize = PIECE_COUNT + 1;
const MAX_CAPTURE_LINE_LENGTH: u16 = 6;

#[derive(Copy, Clone, Eq, PartialEq)]
struct Transposition {
    eval: Eval,
    eval_depth: u16,
    best_move_code: u64,
}

pub struct Engine {
    transposition_table: std::collections::HashMap<u64, Transposition>,
    engine_stack: Vec<EngineDepth>,
    pub board: ThreePlayerChess,
    pub depth_max: u16,
    pub eval_depth_max: u16,
    pub transposition_count: usize,
    pub prune_count: usize,
    pub pos_count: usize,
    pub deciding_player: Color,
    pub debug_log: bool,
}

#[derive(Default)]
struct EngineDepth {
    moves: Vec<EngineMove>,
    hash: u64,
    index: usize,
    eval: Eval,
    best_move: Option<Move>,
    move_rev: Option<ReversableMove>,
}

struct EngineMove {
    hash: u64,
    mov: Option<Move>, // we allow null moves
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
    fn new(mov: Option<Move>, eval: Eval, eval_depth: u16) -> Transposition {
        Transposition {
            eval,
            eval_depth,
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
        Transposition::new(None, EVAL_LOSS, 0)
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
    res ^= (tpc.move_index - tpc.last_capture_or_pawn_move_index) as u64;
    res
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

fn flip_eval(flip: bool, eval: Eval) -> Eval {
    if flip {
        -eval
    } else {
        eval
    }
}

fn is_depth_of_us(depth: u16) -> bool {
    depth % 3 == 0
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
            deciding_player: Color::C0,
            debug_log: false,
        }
    }

    pub fn search_position(
        &mut self,
        tpc: &ThreePlayerChess,
        depth: u16,
        max_time_seconds: f32,
    ) -> Option<(Move, String, i16)> {
        self.board = tpc.clone();
        self.eval_depth_max = self.board.move_index;
        self.depth_max = 0;
        //self.transposition_table.retain(|_, tp| tp.eval_depth > self.eval_depth_max);
        self.transposition_table.clear(); // since we use
        self.transposition_count = 0;
        self.prune_count = 0;
        self.pos_count = 0;
        self.deciding_player = self.board.turn;
        let end = Instant::now()
            .checked_add(Duration::from_secs_f32(max_time_seconds))
            .unwrap();
        let mut bm = self.board.gen_moves().get(0).map(|m| *m);
        let mut bm_str = bm.map_or("".to_owned(), |bm| {
            bm.to_string(&mut self.board).as_str().to_owned()
        });
        let mut eval = -1;
        for _ in 0..depth {
            self.depth_max += 1;
            self.eval_depth_max += 1;
            if self.search_iterate(end).is_ok() {
                eval = self.engine_stack[0].eval;
                if let Some(best_move) = self.engine_stack[0].best_move {
                    bm = Some(best_move);
                    bm_str = self.transposition_line_str(bm);
                }
                if self.debug_log {
                    println!("finished depth {} [{}]: {}", self.depth_max, eval, bm_str);
                }
            } else {
                if self.debug_log {
                    println!(
                        "aborted depth {} (after {} positions)",
                        self.depth_max, self.pos_count
                    );
                }
                self.depth_max -= 1;
                break;
            }
        }
        if let Some(bm) = bm {
            return Some((bm, bm_str, eval));
        }
        None
    }
    fn gen_engine_depth(
        &mut self,
        depth: u16,
        rm: Option<ReversableMove>,
        captures_only: bool,
    ) -> &EngineDepth {
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
        ed.eval = -EVAL_MAX;
        let moves = self.board.gen_moves();
        ed.moves.reserve(moves.len());
        if captures_only {
            ed.moves.push(EngineMove {
                eval: EVAL_DRAW,
                hash: parent_hash,
                mov: None,
            });
        }
        for mov in moves {
            if captures_only {
                match mov.move_type {
                    Capture(..) | CapturePromotion(..) => (),
                    _ => continue,
                }
            }
            let rm = ReversableMove::new(&self.board, mov);
            self.board.perform_move(rm.mov);
            let hash = hash_board(&self.board);
            let eval = self.transposition_table.get(&hash).map_or_else(
                || get_initial_pos_eval_for_sort(&mut self.board, mov),
                |tp| -tp.eval,
            );
            self.board.revert_move(&rm);
            ed.moves.push(EngineMove {
                eval,
                hash,
                mov: Some(mov),
            });
        }
        ed.moves.sort_by(|m_l, m_r| m_r.eval.cmp(&m_l.eval));
        ed
    }
    fn propagate_move_eval(
        &mut self,
        depth: u16,
        mov: Option<Move>,
        eval: Eval,
    ) -> PropagationResult {
        let eval = flip_eval(!is_depth_of_us(depth + 2), eval);
        let mut result = PropagationResult::Ok;
        let mut ed = &mut self.engine_stack[depth as usize];
        if eval > ed.eval {
            ed.eval = eval;
            ed.best_move = mov;

            let us = is_depth_of_us(depth);
            let prev1_us = is_depth_of_us(depth + 2);
            let prev2_us = is_depth_of_us(depth + 1);

            if depth >= 1 && (prev1_us || us) {
                let ed_move_ref = ed.move_rev.as_ref().map(|mr| mr.mov);
                let ed1 = &self.engine_stack[depth as usize - 1];
                if eval >= -ed1.eval && ed1.best_move != ed_move_ref {
                    self.prune_count += 1;
                    result = PropagationResult::Pruned(1);
                }
            } else if depth >= 2 && (prev2_us || us) {
                let ed_move_ref = ed.move_rev.as_ref().map(|mr| mr.mov);
                let ed1 = &self.engine_stack[depth as usize - 1];
                let ed1_best_move = ed1.best_move;
                let ed1_move_ref = ed1.move_rev.as_ref().map(|mr| mr.mov);
                let ed2 = &self.engine_stack[depth as usize - 2];
                if eval >= -ed2.eval
                    && ed2.best_move != ed1_move_ref
                    && ed1_best_move != ed_move_ref
                {
                    self.prune_count += 1;
                    result = PropagationResult::Pruned(2);
                }
            }
            if self.debug_log && mov.is_some() {
                // we really like this as debug break point, so we keep separate lines
                let flavor_text;
                if depth > 0 {
                    flavor_text = "new best move";
                } else {
                    flavor_text = "new main line";
                }
                let (p1, p2, p3) = if result.prune_depth() > 0 {
                    (" [prune: ", result.prune_depth().to_string(), "]")
                } else {
                    ("", "".to_string(), "")
                };
                println!(
                    "{} (@depth {}): [{}({})]: {} --> {} {}{}{}",
                    flavor_text,
                    depth,
                    eval,
                    flip_eval(!is_depth_of_us(depth), eval),
                    self.engine_line_str(depth, None),
                    self.transposition_line_str(mov),
                    p1,
                    p2,
                    p3
                );
            }
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
                    let tp_eval = tp.eval;
                    let (board_eval, _) = evaluate_position(&mut board, self.deciding_player);
                    res += &format_args!(
                        " ({} ({}) / {})",
                        tp_eval,
                        flip_eval(board.turn != self.deciding_player, tp_eval),
                        board_eval
                    )
                    .to_string();
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
        self.gen_engine_depth(0, None, false);
        let mut depth = 0;
        let mut propagation_result = PropagationResult::Ok;
        loop {
            let mut ed = &mut self.engine_stack[depth as usize];
            while ed.index == ed.moves.len() || propagation_result != PropagationResult::Ok {
                ed.moves.clear();
                if ed.index > 0 {
                    self.transposition_table.insert(
                        ed.hash,
                        Transposition::new(ed.best_move, ed.eval, self.eval_depth_max),
                    );
                }
                if depth == 0 {
                    return Ok(());
                }
                let rm = ed.move_rev.take().unwrap();
                self.board.revert_move(&rm);
                depth -= 1;
                let prev_prune_depth = propagation_result.prune_depth();
                if ed.index > 0 {
                    let eval = ed.eval;
                    propagation_result = self.propagate_move_eval(depth, Some(rm.mov), eval);
                } else {
                    propagation_result = PropagationResult::Ok;
                }
                if prev_prune_depth > 1 && propagation_result.prune_depth() < prev_prune_depth - 1 {
                    propagation_result = PropagationResult::Pruned(prev_prune_depth - 1);
                }
                ed = &mut self.engine_stack[depth as usize];
            }
            let em = &mut ed.moves[ed.index];
            ed.index += 1;

            let rm;
            if let Some(mov) = em.mov {
                if let Some(tp) = self.transposition_table.get(&em.hash) {
                    if tp.eval_depth >= self.eval_depth_max {
                        self.transposition_count += 1;
                        propagation_result = self.propagate_move_eval(depth, Some(mov), tp.eval);
                        continue;
                    }
                }
                rm = Some(ReversableMove::new(&self.board, mov));
                self.board.perform_move(mov);
            } else {
                rm = None;
            }
            // even for null moves, we increase the depth, because we want them to be
            // 'overrulable' by actual moves
            depth += 1;
            let force_eval = rm.is_none() || depth > self.depth_max + MAX_CAPTURE_LINE_LENGTH;
            if depth >= self.depth_max
                || force_eval
                || self.board.game_status != GameStatus::Ongoing
            {
                self.pos_count += 1;
                let (eval_perspective, captures_exist) =
                    evaluate_position(&mut self.board, self.deciding_player);
                let hash = em.hash;

                let eval = flip_eval(!is_depth_of_us(depth), eval_perspective);

                if self.debug_log {
                    let line_depth = depth - rm.as_ref().map_or(1, |_| 0);
                    println!(
                        "eval@ {}{}: {} ({}) (cap: {}, force: {})",
                        self.engine_line_str(line_depth, rm.as_ref()),
                        rm.as_ref().map_or(" NULL", |_| ""),
                        eval,
                        eval_perspective,
                        captures_exist,
                        force_eval
                    );
                }
                self.transposition_table
                    .insert(hash, Transposition::new(None, eval, self.eval_depth_max));
                if !captures_exist || force_eval {
                    if let Some(ref rm) = rm {
                        self.board.revert_move(rm);
                    }
                    depth -= 1;
                    propagation_result = self.propagate_move_eval(depth, rm.map(|rm| rm.mov), eval);
                    if propagation_result == PropagationResult::Ok {
                        if eval >= EVAL_WIN {
                            self.prune_count += 1;
                            propagation_result = PropagationResult::Pruned(1);
                        }
                    }
                    continue;
                }
                if self.debug_log {
                    if self.debug_log {
                        println!(
                            "expanding unstable position: (@depth {}): {}",
                            depth,
                            self.engine_line_str(depth, rm.as_ref()),
                        );
                    }
                }
            }
            if Instant::now().gt(&end) {
                return Err(());
            }
            self.gen_engine_depth(depth, rm, depth >= self.depth_max);
        }
    }
}
