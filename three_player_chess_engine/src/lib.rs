use std::ops::Sub;
use std::time::{Duration, Instant};
use three_player_chess::board::MoveType::*;
use three_player_chess::board::*;
use three_player_chess::movegen::MovegenOptions;
use three_player_chess_board_eval::*;

const MAX_CAPTURE_LINE_LENGTH: u16 = 6;

#[derive(Copy, Clone, Eq, PartialEq)]
struct Transposition {
    score: Score,
    eval_depth: u16,
    best_move: Option<Move>,
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
    pub dummy_vec: Vec<Move>,
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
    // we store this in here so we can check the transposition table
    // without actually applying (and then reverting) the move
    hash: u64,
    mov: Option<Move>, // we allow null moves
    score: Score,
    captures_available: Option<bool>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd)]
enum PropagationResult {
    Pruned(u8),
    Ok,
}

impl Transposition {
    fn new(mov: Option<Move>, score: Score, eval_depth: u16) -> Transposition {
        Transposition {
            score,
            eval_depth,
            best_move: mov,
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

pub fn score_str(score: Score) -> String {
    return format!(
        "{:.2}/{:.2}/{:.2}",
        score[0] as f32 / 100.,
        score[1] as f32 / 100.,
        score[2] as f32 / 100.
    );
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
            dummy_vec: Vec::new(),
        }
    }
    pub fn report_search_depth_results(&mut self, start: Instant) {
        let score = self.engine_stack[0].score;
        let mut line_str = "".to_owned();
        if let Some(bm) = self.engine_stack[0].best_move {
            line_str = self.transposition_line_str(Some(bm));
        }
        let time = Instant::now().sub(start).as_secs_f32();
        println!(
            "(depth {} took {:.1}s, {:.2} kN/s, {} positions, {} pruned branches, {} transpositions): eval ({}) with {}",
            self.depth_max ,
            time,
            (self.pos_count as f32 / time) / 1000.,
            self.pos_count,
            self.prune_count,
            self.transposition_count,
            score_str(score),
            line_str
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
        self.eval_depth_max = self.board.move_index;
        self.depth_max = 0;
        //self.transposition_table.retain(|_, tp| tp.eval_depth > self.eval_depth_max);
        self.transposition_table.clear(); // since we use
        self.deciding_player = self.board.turn;
        let mut start = Instant::now();
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
            let (transp_count, prune_count, pos_count) =
                (self.transposition_count, self.prune_count, self.pos_count);
            self.transposition_count = 0;
            self.prune_count = 0;
            self.pos_count = 0;
            self.depth_max += 1;
            self.eval_depth_max += 1;
            if self.search_iterate(end).is_ok() {
                best_move = self.engine_stack[0].best_move;
                if report_results_per_depth || self.debug_log {
                    self.report_search_depth_results(start);
                }
                start = Instant::now();
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
        let (parent_hash, parent_score, parent_has_caps) = if depth > 0 {
            let parent = &self.engine_stack[depth as usize - 1];
            let p_mov = &parent.moves[parent.index - 1];
            (p_mov.hash, p_mov.score, p_mov.captures_available)
        } else {
            let (score, has_caps) = calculate_position_score(&mut self.board);
            (self.board.get_zobrist_hash(), score, Some(has_caps))
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
        ed.score = [-EVAL_MAX; HB_COUNT];
        self.dummy_vec.clear();
        self.board.gen_moves_with_options(
            &mut self.dummy_vec,
            MovegenOptions {
                captures_only: captures_only,
                only_one: false,
            },
        );
        if captures_only {
            //null move
            ed.moves.push(EngineMove {
                score: parent_score,
                captures_available: parent_has_caps,
                hash: parent_hash,
                mov: None,
            });
        }
        for mov in self.dummy_vec.iter() {
            let rm = ReversableMove::new(&self.board, *mov);
            self.board.perform_reversable_move(&rm);
            let hash = self.board.get_zobrist_hash();
            let (score, has_caps) = self.transposition_table.get(&hash).map_or_else(
                || {
                    let (score, caps) = calculate_position_score(&mut self.board);
                    (score, Some(caps))
                },
                |tp| (tp.score, None),
            );
            self.board.revert_move(&rm);
            ed.moves.push(EngineMove {
                score,
                captures_available: has_caps,
                hash,
                mov: Some(*mov),
            });
        }
        self.dummy_vec.clear();
        ed.moves.sort_by(|m_l, m_r| {
            // reversed l and r because we want the highest scoreing move for the deciding player to be checked first
            m_r.score[usize::from(self.board.turn)].cmp(&m_l.score[usize::from(self.board.turn)])
        });
        ed
    }
    fn propagate_move_score(
        &mut self,
        depth: u16,
        mov: Option<Move>,
        score: Score,
    ) -> PropagationResult {
        let mut result = PropagationResult::Ok;
        let mover = usize::from(self.board.turn);
        let mut ed = &mut self.engine_stack[depth as usize];
        if score[mover] > ed.score[mover] {
            //TODO
            ed.score = score;
            ed.best_move = mov;

            if depth >= 1 {
                let ed_move_ref = ed.move_rev.as_ref().map(|mr| mr.mov);
                let prev_mover = usize::from(self.board.turn.prev());
                let ed1 = &self.engine_stack[depth as usize - 1];
                if score[prev_mover] <= ed1.score[prev_mover] && ed1.best_move != ed_move_ref {
                    self.prune_count += 1;
                    result = PropagationResult::Pruned(1);
                }
            } else if depth >= 2 {
                let ed_move_ref = ed.move_rev.as_ref().map(|mr| mr.mov);
                let ed1 = &self.engine_stack[depth as usize - 1];
                let ed1_best_move = ed1.best_move;
                let ed1_move_ref = ed1.move_rev.as_ref().map(|mr| mr.mov);
                let ed2 = &self.engine_stack[depth as usize - 2];
                let prev_prev_mover = usize::from(self.board.turn.prev());
                if score[prev_prev_mover] <= ed2.score[prev_prev_mover]
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
                    "{} (@depth {}): [{}]: {} --> {} {}{}{}",
                    flavor_text,
                    depth,
                    score_str(score),
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
                if mov.is_some() {
                    //let tp_score = tp.score;
                    //res += &format_args!(" ({})", score_str(tp_score),).to_string();
                }
                mov = tp.best_move;
                if mov.is_none() {
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
        let mut depth: u16 = 0;
        let mut propagation_result = PropagationResult::Ok;
        loop {
            let mut ed = &mut self.engine_stack[depth as usize];
            while ed.index == ed.moves.len() || propagation_result != PropagationResult::Ok {
                ed.moves.clear();
                if ed.index > 0 {
                    self.transposition_table.insert(
                        ed.hash,
                        Transposition::new(ed.best_move, ed.score, self.eval_depth_max),
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
                    let score = ed.score;
                    propagation_result = self.propagate_move_score(depth, Some(rm.mov), score);
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
                        self.pos_count += 1;
                        let tp_score = tp.score;
                        propagation_result = self.propagate_move_score(depth, Some(mov), tp_score);
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
            let force_eval = rm.is_none() || depth > self.depth_max + MAX_CAPTURE_LINE_LENGTH;
            if depth >= self.depth_max || force_eval || game_over {
                self.pos_count += 1;
                let score = em.score;
                let score_now = if game_over || force_eval {
                    true
                } else {
                    !em.captures_available
                        .unwrap_or_else(|| board_has_captures(&mut self.board))
                };
                let hash = em.hash;

                if self.debug_log {
                    let line_depth = depth - rm.as_ref().map_or(1, |_| 0);
                    println!(
                        "eval@ {}{}: ({}) (eval_now: {})",
                        self.engine_line_str(line_depth, rm.as_ref()),
                        rm.as_ref().map_or(" NULL", |_| ""),
                        score_str(score),
                        score_now
                    );
                }

                if score_now {
                    self.transposition_table
                        .insert(hash, Transposition::new(None, score, self.eval_depth_max));
                    if let Some(ref rm) = rm {
                        self.board.revert_move(rm);
                    }
                    depth -= 1;

                    propagation_result =
                        self.propagate_move_score(depth, rm.map(|rm| rm.mov), score);
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
