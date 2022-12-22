use std::ops::Sub;
use std::time::{Duration, Instant};
use three_player_chess::board::MoveType::*;
use three_player_chess::board::*;
use three_player_chess::movegen::MovegenOptions;
use three_player_chess::zobrist::ZOBRIST_NULL_MOVE_HASH;
use three_player_chess_board_eval::*;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Transposition {
    eval: Eval,
    eval_cap_line_max_move_index: u16,
    best_move_code: u64,
}

pub struct ParanoidEngine {
    pub transposition_table: std::collections::HashMap<u64, Transposition>,
    engine_stack: Vec<EngineDepth>,
    pub board: ThreePlayerChess,
    pub depth_max: u16,
    pub cap_line_len: u16,
    pub eval_max_move_index: u16,
    pub eval_cap_line_max_move_index: u16,
    pub eval_cap_line_len: u16,
    pub transposition_count: usize,
    pub prune_count: usize,
    pub pos_count: usize,
    pub deciding_player: Color,
    pub debug_log: bool,
    pub dummy_vec: Vec<Move>,
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
    // we store this in here so we can check the transposition table
    // without actually applying (and then reverting) the move
    hash: u64,
    mov: Option<Move>, // we allow null moves
    eval: Eval,
    captures_available: Option<bool>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd)]
enum PropagationResult {
    Pruned(u8),
    Ok,
}

impl Transposition {
    fn new(mov: Option<Move>, eval: Eval, eval_cap_line_max_move_index: u16) -> Transposition {
        Transposition {
            eval,
            eval_cap_line_max_move_index,
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

impl ParanoidEngine {
    pub fn new() -> ParanoidEngine {
        ParanoidEngine {
            transposition_table: Default::default(),
            board: Default::default(),
            engine_stack: Default::default(),
            depth_max: 0,
            cap_line_len: 0,
            eval_max_move_index: 0,
            eval_cap_line_len: 0,
            eval_cap_line_max_move_index: 0,
            prune_count: 0,
            transposition_count: 0,
            pos_count: 0,
            deciding_player: Color::C0,
            debug_log: false,
            dummy_vec: Vec::new(),
        }
    }
    pub fn report_search_depth_results(&mut self, search_start: Instant, depth_start: Instant) {
        let mut eval_str = "".to_owned();

        if let Some(bm) = self.engine_stack[0].best_move {
            let eval = self.engine_stack[0].eval;
            eval_str = format!("({}): {}", eval, self.transposition_line_str(Some(bm)));
            let mut moves: Vec<_> = self
                .board
                .gen_moves()
                .iter()
                .map(|m| {
                    (
                        {
                            let rm = ReversableMove::new(&self.board, *m);
                            self.board.perform_reversable_move(&rm);
                            let te = self.transposition_table.get(&self.board.get_zobrist_hash());
                            self.board.revert_move(&rm);
                            te.map_or(EVAL_LOSS, |te| te.eval)
                        },
                        *m,
                    )
                })
                .collect();
            moves.sort_by(|l, r| l.0.cmp(&r.0));
            for i in 0..moves.len().min(3) {
                let em = &moves[i];
                eval_str = format!(
                    "{}\n   {}({}): {}",
                    eval_str,
                    if i == 0 { "*" } else { " " },
                    em.0,
                    self.transposition_line_str(Some(em.1))
                )
                .to_string();
            }
        }
        let now = Instant::now();
        let depth_elapsed = now.sub(depth_start).as_secs_f32();
        println!(
            "depth {}+{} ({:.1}s [{:.1} s], {:.2} kN/s): {}",
            self.depth_max,
            self.cap_line_len,
            depth_elapsed,
            now.sub(search_start).as_secs_f32(),
            (self.pos_count as f32 / depth_elapsed) / 1000.,
            eval_str
        );
    }
    pub fn search_position(
        &mut self,
        tpc: &ThreePlayerChess,
        depth: u16,
        max_time_seconds: f32,
        report_results_per_depth: bool,
    ) -> Option<Move> {
        self.board = tpc.clone();
        self.eval_max_move_index = self.board.move_index;
        self.depth_max = 0;
        //self.transposition_table.retain(|_, tp| tp.eval_depth > self.eval_depth_max);
        self.transposition_table.clear(); // since we use
        self.deciding_player = self.board.turn;
        let search_start = Instant::now();
        let end = Instant::now()
            .checked_add(Duration::from_secs_f32(max_time_seconds))
            .unwrap();
        let mut start_mov = Vec::new();
        self.board.gen_moves_with_options(
            &mut start_mov,
            MovegenOptions {
                captures_only: false,
                only_one: true,
            },
        );
        let mut best_move = start_mov.pop();

        for _ in 0..depth {
            let depth_start = Instant::now();
            let (transp_count, prune_count, pos_count) =
                (self.transposition_count, self.prune_count, self.pos_count);
            self.transposition_count = 0;
            self.prune_count = 0;
            self.pos_count = 0;
            self.depth_max += 1;
            self.cap_line_len = self.depth_max;
            if self.search_iterate(end).is_ok() {
                best_move = self.engine_stack[0].best_move;
                if report_results_per_depth || self.debug_log {
                    self.report_search_depth_results(search_start, depth_start);
                }
            } else {
                self.transposition_count = transp_count;
                self.prune_count = prune_count;
                self.pos_count = pos_count;
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
        if let Some(bm) = best_move {
            return Some(bm);
        }
        None
    }
    fn gen_engine_depth(
        &mut self,
        depth: u16,
        rm: Option<ReversableMove>,
        captures_only: bool,
    ) -> &EngineDepth {
        let (parent_hash, parent_eval, parent_has_caps) = if depth > 0 {
            let parent = &self.engine_stack[depth as usize - 1];
            let p_mov = &parent.moves[parent.index - 1];
            (p_mov.hash, p_mov.eval, p_mov.captures_available)
        } else {
            let (eval, has_caps) = calculate_position_eval(&mut self.board, self.deciding_player);
            (
                self.board.get_zobrist_hash(),
                flip_eval(!is_depth_of_us(depth + 2), eval),
                Some(has_caps),
            )
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
        self.dummy_vec.clear();
        self.board.gen_moves_with_options(
            &mut self.dummy_vec,
            MovegenOptions {
                captures_only: captures_only,
                only_one: false,
            },
        );
        if captures_only {
            // null move
            ed.moves.push(EngineMove {
                eval: flip_eval(!is_depth_of_us(depth + 1), parent_eval),
                captures_available: parent_has_caps,
                hash: parent_hash ^ ZOBRIST_NULL_MOVE_HASH,
                mov: None,
            });
        }
        for mov in self.dummy_vec.iter() {
            if captures_only {
                match mov.move_type {
                    Capture(..) | CapturePromotion(..) => (),
                    _ => continue,
                }
            }
            let rm = ReversableMove::new(&self.board, *mov);
            self.board.perform_reversable_move(&rm);
            let hash = self.board.get_zobrist_hash();
            let (eval, has_caps) = self.transposition_table.get(&hash).map_or_else(
                || {
                    let (ev, caps) = calculate_position_eval(&mut self.board, self.deciding_player);
                    (
                        flip_eval(self.board.turn != self.deciding_player, ev),
                        Some(caps),
                    )
                },
                |tp| (-tp.eval, None),
            );
            self.board.revert_move(&rm);
            ed.moves.push(EngineMove {
                eval,
                captures_available: has_caps,
                hash,
                mov: Some(*mov),
            });
        }
        self.dummy_vec.clear();
        ed.moves.sort_by(|m_l, m_r| m_l.eval.cmp(&m_r.eval));
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
            } else if eval >= EVAL_WIN - self.board.move_index as Eval - 1 {
                self.prune_count += 1;
                result = PropagationResult::Pruned(1);
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
            self.board.perform_reversable_move(&rm);
        }
        if let Some(ref rm) = last_move {
            self.board.perform_reversable_move(&rm);
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
            if let Some(tp) = self.transposition_table.get(&board.get_zobrist_hash()) {
                /*if mov.is_some() {
                    let tp_eval = tp.eval;
                    let (board_eval, _) = calculate_position_eval(&mut board, self.deciding_player);
                    res += &format_args!(
                        " ({} ({}) / {})",
                        tp_eval,
                        flip_eval(board.turn != self.deciding_player, tp_eval),
                        board_eval
                    )
                    .to_string();
                }*/
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
        self.eval_max_move_index = self.board.move_index + self.depth_max;
        self.eval_cap_line_max_move_index = self.eval_max_move_index + self.cap_line_len;
        self.gen_engine_depth(0, None, false);
        let mut depth: u16 = 0;
        let mut propagation_result = PropagationResult::Ok;
        loop {
            let mut ed = &mut self.engine_stack[depth as usize];
            while ed.index == ed.moves.len() || propagation_result != PropagationResult::Ok {
                ed.moves.clear();
                if ed.index > 0 {
                    self.transposition_table.insert(
                        ed.hash,
                        Transposition::new(
                            ed.best_move,
                            ed.eval,
                            self.eval_cap_line_max_move_index,
                        ),
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
            let only_move = ed.moves.len() == 1;
            let em = &mut ed.moves[ed.index];
            ed.index += 1;

            let rm;
            if let Some(mov) = em.mov {
                if let Some(tp) = self.transposition_table.get(&em.hash) {
                    if tp.eval_cap_line_max_move_index >= self.eval_cap_line_max_move_index {
                        self.transposition_count += 1;
                        self.pos_count += 1;
                        let tp_eval = tp.eval;
                        propagation_result = self.propagate_move_eval(depth, Some(mov), tp_eval);
                        continue;
                    }
                }
                rm = Some(ReversableMove::new(&self.board, mov));
                self.board.perform_reversable_move(rm.as_ref().unwrap());
            } else {
                rm = None;
            }
            // even for null moves, we increase the depth, because we want them to be
            // 'overrulable' by actual moves
            depth += 1;
            let game_over = self.board.game_status != GameStatus::Ongoing;
            let force_eval =
                rm.is_none() || depth >= self.depth_max + self.eval_cap_line_len || only_move;
            if depth >= self.depth_max || force_eval || game_over {
                self.pos_count += 1;
                let eval = em.eval;
                let eval_now = if game_over || force_eval {
                    true
                } else {
                    !em.captures_available
                        .unwrap_or_else(|| board_has_captures(&mut self.board))
                };
                let hash = em.hash;

                if self.debug_log {
                    let line_depth = depth - rm.as_ref().map_or(1, |_| 0);
                    println!(
                        "eval@ {}{}: {} ({}) (eval_now: {})",
                        self.engine_line_str(line_depth, rm.as_ref()),
                        rm.as_ref().map_or(" NULL", |_| ""),
                        eval,
                        flip_eval(!is_depth_of_us(depth), eval),
                        eval_now
                    );
                }

                if eval_now {
                    self.transposition_table.insert(
                        hash,
                        Transposition::new(None, eval, self.eval_cap_line_max_move_index),
                    );
                    if let Some(ref rm) = rm {
                        self.board.revert_move(rm);
                    }
                    depth -= 1;

                    propagation_result = self.propagate_move_eval(depth, rm.map(|rm| rm.mov), eval);
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
