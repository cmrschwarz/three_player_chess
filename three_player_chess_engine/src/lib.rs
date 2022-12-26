use std::hash::{BuildHasherDefault, Hasher};
use std::ops::Sub;
use std::time::{Duration, Instant};
use three_player_chess::board::*;
use three_player_chess::movegen::MovegenOptions;
use three_player_chess_board_eval::*;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Transposition {
    score: Score,
    // prevent the most eggregious hash collisions
    // since the fifty move rule kills non same depths transpositions
    // anyways, this does not really hurt the tp's effectiveness
    // NOTE: this is a hack, really. the fifty move rule thing could also
    // be changed to where only for the last 10 moves or so does the value
    // impact the zobrist
    move_index: u16,
    eval_max_move_index: u16,
    best_move: Option<Move>,
}
#[derive(Default)]
pub struct IdentityHasher {
    value: u64,
}

impl Hasher for IdentityHasher {
    fn finish(&self) -> u64 {
        return self.value;
    }

    fn write(&mut self, _bytes: &[u8]) {
        unreachable!();
    }
    fn write_u64(&mut self, i: u64) {
        debug_assert!(self.value == 0);
        self.value = i;
    }
}

pub struct Engine {
    pub transposition_table:
        std::collections::HashMap<u64, Transposition, BuildHasherDefault<IdentityHasher>>,
    engine_stack: Vec<EngineDepth>,
    pub board: ThreePlayerChess,
    pub eval_depth: u16,
    pub eval_cap_line_len: u16,
    pub eval_max_move_index: u16, // the deepest move index that we evaluate to
    pub eval_cap_line_max_move_index: u16,
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
    hash: u64, // hash of position before any of the moves
    index: usize,
    score: Score,
    best_move: Option<Move>,
    move_rev: Option<ReversableMove>,
}

struct EngineMove {
    // we store this in here so we can check the transposition table
    // without actually applying (and then reverting) the move
    hash: u64, // hash of position after the move
    mov: Move,
    score: Score,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd)]
enum PropagationResult {
    Pruned(u8),
    Ok,
}

impl Transposition {
    fn new(
        mov: Option<Move>,
        score: Score,
        move_index: u16,
        eval_max_move_index: u16,
    ) -> Transposition {
        Transposition {
            score,
            move_index: move_index,
            eval_max_move_index,
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
            eval_depth: 0,
            eval_cap_line_len: 0,
            eval_max_move_index: 0,
            eval_cap_line_max_move_index: 0,
            prune_count: 0,
            transposition_count: 0,
            pos_count: 0,
            deciding_player: Color::default(),
            debug_log: false,
            dummy_vec: Vec::new(),
        }
    }
    pub fn report_search_depth_results(&mut self, search_start: Instant, depth_start: Instant) {
        let mut eval_str = "".to_owned();

        if let Some(bm) = self.engine_stack[0].best_move {
            eval_str = format!(
                "({}): {}",
                score_str(self.engine_stack[0].score),
                self.transposition_line_str(bm)
            );
            let mut moves: Vec<_> = self
                .board
                .gen_moves()
                .iter()
                .map(|m| {
                    (
                        {
                            let rm = ReversableMove::new(&self.board, *m);
                            self.board.perform_reversable_move(&rm, false);
                            let te = self.transposition_table.get(&self.board.get_zobrist_hash());
                            self.board.revert_move(&rm);
                            te.map_or([Eval::MIN; HB_COUNT], |te| te.score)
                        },
                        *m,
                    )
                })
                .collect();
            let turn = usize::from(self.board.turn);
            let good_moves_to_display = 3;
            let bad_moves_to_display = 1;
            moves.sort_by(|l, r| r.0[turn].cmp(&l.0[turn]));
            for (i, em) in moves[0..moves.len().min(good_moves_to_display)]
                .iter()
                .enumerate()
            {
                eval_str = format!(
                    "{}\n   {}({}): {}",
                    eval_str,
                    if i == 0 { "*" } else { " " },
                    score_str(em.0),
                    self.transposition_line_str(em.1)
                );
            }
            let bad_moves_present = moves
                .len()
                .saturating_sub(good_moves_to_display)
                .min(bad_moves_to_display);
            for em in moves[moves.len() - bad_moves_present..moves.len()]
                .iter()
                .rev()
            {
                eval_str = format!(
                    "{}\n   ~({}): {}",
                    eval_str,
                    score_str(em.0),
                    self.transposition_line_str(em.1)
                );
            }
        }
        let now = Instant::now();
        let depth_elapsed = now.sub(depth_start).as_secs_f32();
        println!(
            "depth {}+{} ({:.1}s [{:.1} s], {:.2} kN/s): {}",
            self.eval_depth,
            self.eval_cap_line_len,
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
        self.eval_depth = 0;
        self.eval_cap_line_len = 0;
        self.transposition_table
            .retain(|_, tp| tp.eval_max_move_index > self.board.move_index);
        self.transposition_table.clear();
        self.deciding_player = self.board.turn;
        let search_start = Instant::now();
        let end = search_start
            .checked_add(Duration::from_secs_f32(max_time_seconds))
            .unwrap();
        let mut start_mov = Vec::new();
        self.board.gen_moves_with_options(
            &mut start_mov,
            MovegenOptions {
                gen_slides: true,
                gen_null_move: false,
                only_one: true,
            },
        );
        let mut best_move = start_mov.pop();
        let mut total_pos_count = 0;
        'search: for _ in 0..depth {
            self.eval_depth += 1;
            self.eval_cap_line_len = 3.min(self.eval_depth);
            self.transposition_count = 0;
            self.prune_count = 0;
            self.pos_count = 0;
            let start = Instant::now();
            let bm = self.search_iterate(end);
            total_pos_count += self.pos_count;
            if bm.is_ok() {
                best_move = self.engine_stack[0].best_move;
                if report_results_per_depth || self.debug_log {
                    self.report_search_depth_results(search_start, start);
                }
            } else {
                if report_results_per_depth || self.debug_log {
                    let now = Instant::now();
                    let time_elapsed = now.sub(start).as_secs_f32();
                    let total_time_elapsed = now.sub(search_start).as_secs_f32();
                    println!(
                            "depth {}+{} ({:.1}s [{:.1} s], {:.2} kN/s): aborted (after {} positions) [total: {:.2} kN/s]",
                            self.eval_depth,
                            self.eval_cap_line_len,
                            time_elapsed,
                            total_time_elapsed,
                            (self.pos_count as f32 / time_elapsed) / 1000.,
                            self.pos_count,
                            (total_pos_count as f32 /  total_time_elapsed) / 1000.,
                        );
                }
                break 'search;
            }
        }
        if let Some(bm) = best_move {
            return Some(bm);
        }
        None
    }
    // returns whether there are legal moves found -> otherwise force evaluation
    fn gen_engine_depth(
        &mut self,
        depth: u16,
        rm: Option<ReversableMove>,
        captures_only: bool,
    ) -> bool {
        let parent_hash = self.board.get_zobrist_hash();
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
        ed.score = [Eval::MIN; HB_COUNT];
        self.dummy_vec.clear();
        self.board.gen_moves_with_options(
            &mut self.dummy_vec,
            MovegenOptions {
                gen_slides: !captures_only,
                gen_null_move: captures_only,
                only_one: false,
            },
        );
        if captures_only && self.dummy_vec.is_empty() {
            self.board.gen_moves_with_options(
                &mut self.dummy_vec,
                MovegenOptions {
                    gen_slides: true,
                    gen_null_move: false,
                    only_one: false,
                },
            );
        }
        for mov in self.dummy_vec.iter() {
            let rm = ReversableMove::new(&self.board, *mov);
            self.board.perform_reversable_move(&rm, false);
            let hash = self.board.get_zobrist_hash();
            let score = self.transposition_table.get(&hash).map_or_else(
                || {
                    let score = [Eval::MIN; HB_COUNT];
                    score
                },
                |tp| tp.score,
            );
            self.board.revert_move(&rm);
            ed.moves.push(EngineMove {
                score,
                hash,
                mov: *mov,
            });
        }
        self.dummy_vec.clear();
        ed.moves.sort_by(|m_l, m_r| {
            // reversed l and r because we want the highest scoreing move for the deciding player to be checked first
            m_r.score[usize::from(self.board.turn)].cmp(&m_l.score[usize::from(self.board.turn)])
        });
        if let Some(em) = ed.moves.first().as_deref() {
            // so that we apply the first move as 'better than nothing'
            // but don't prune
            ed.best_move = Some(em.mov);
            return true;
        }
        return false;
    }
    fn propagate_move_score(&mut self, depth: u16, mov: Move, score: Score) -> PropagationResult {
        let mut result = PropagationResult::Ok;
        let mover = usize::from(self.board.turn);
        let mut ed = &mut self.engine_stack[depth as usize];
        if score[mover] > ed.score[mover] {
            ed.score = score;
            ed.best_move = Some(mov);
            let ed_move_ref = ed.move_rev.as_ref().map(|mr| mr.mov);
            // we sadly can't prune up two levels here because we don't want player 1 to be afraid of
            // moves that player 3 wouldn't even consider
            // (e.g. a move for player 3 that leads to player 2 winning)
            let prev_mover = usize::from(self.board.turn.prev());
            if depth >= 1 && result == PropagationResult::Ok {
                let ed1 = &self.engine_stack[depth as usize - 1];
                if score[prev_mover] <= ed1.score[prev_mover] && ed1.best_move != ed_move_ref {
                    self.prune_count += 1;
                    result = PropagationResult::Pruned(1);
                }
            }
            if result == PropagationResult::Ok
                && (score[mover] >= EVAL_WIN || score[prev_mover] <= EVAL_LOSS)
            {
                // if we have a checkmate here, we want to stop eval and not
                // do a depth N check for no reason. this does not destroy the
                // 'fighting on' effect, because it only affects moves on the
                // same depth
                self.prune_count += 1;
                result = PropagationResult::Pruned(1);
            }
            if self.debug_log {
                // we really like this as debug break point, so we keep separate lines
                let flavor_text;
                if depth > 1 {
                    flavor_text = "new best move";
                } else if depth > 0 {
                    flavor_text = "new best response";
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
            self.board.perform_reversable_move(&rm, true);
        }
        if let Some(ref rm) = last_move {
            self.board.perform_reversable_move(&rm, true);
        }
        res
    }
    fn transposition_line_str(&mut self, mov: Move) -> String {
        let mut res = String::new();
        let mut board = self.board.clone();
        let mut mov = mov;
        loop {
            res.push_str(mov.to_string(&mut board).as_str());
            board.perform_move(mov);

            if let Some(tp) = self.transposition_table.get(&board.get_zobrist_hash()) {
                if let Some(bm) = tp.best_move {
                    mov = bm;
                } else {
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
        self.eval_max_move_index = self.board.move_index + self.eval_depth;
        self.eval_cap_line_max_move_index = self.eval_max_move_index + self.eval_depth;
        self.gen_engine_depth(0, None, false);
        let mut depth: u16 = 0;
        let mut propagation_result = PropagationResult::Ok;
        loop {
            let mut ed = &mut self.engine_stack[depth as usize];
            while ed.index == ed.moves.len() || propagation_result != PropagationResult::Ok {
                let score = ed.score;
                let best_move = ed.best_move;
                let move_rev = ed.move_rev.clone();
                let hash = ed.hash;
                if depth > 0 {
                    self.board.revert_move(&move_rev.clone().unwrap());
                }
                if depth == 1 && self.debug_log {
                    let mov = ed.move_rev.as_ref().map(|rm| rm.mov).unwrap();
                    let els = self.transposition_line_str(mov);
                    println!(
                        "finished evaluating {}: ({}): {}",
                        move_rev.as_ref().unwrap().mov.to_string(&mut self.board),
                        score_str(score),
                        els
                    );
                }

                self.transposition_table.insert(
                    hash,
                    Transposition::new(
                        best_move,
                        score,
                        self.board.move_index,
                        self.eval_max_move_index,
                    ),
                );
                if depth == 0 {
                    return Ok(());
                }
                self.propagate_move_score(
                    depth - 1,
                    self.engine_stack[depth as usize]
                        .move_rev
                        .as_ref()
                        .unwrap()
                        .mov,
                    score,
                );
                propagation_result = match propagation_result {
                    PropagationResult::Ok | PropagationResult::Pruned(1) => PropagationResult::Ok,
                    PropagationResult::Pruned(n) => PropagationResult::Pruned(n - 1),
                };
                depth -= 1;
                ed = &mut self.engine_stack[depth as usize];
            }
            let em = &mut ed.moves[ed.index];
            ed.index += 1;
            if depth == 0 && self.debug_log {
                println!(
                    "beginning to evaluate {} (currently {})",
                    em.mov.to_string(&mut self.board),
                    score_str(em.score),
                );
                depth = depth; // convenient breakpoint hook
            }

            let rm;
            if depth > HB_COUNT as u16 {
                // cannot have transpositions before at least somebody made
                // two moves. this also prevents (potentially very destructive)
                // hash collisions on the very first depths
                if let Some(tp) = self.transposition_table.get(&em.hash) {
                    if tp.eval_max_move_index >= self.eval_max_move_index
                        && tp.move_index == self.board.move_index
                    {
                        self.transposition_count += 1;
                        self.pos_count += 1;
                        let tp_score = tp.score;
                        let mov = em.mov;
                        propagation_result = self.propagate_move_score(depth, mov, tp_score);
                        continue;
                    }
                    em.score = tp.score;
                }
            }
            rm = ReversableMove::new(&self.board, em.mov);
            self.board.perform_reversable_move(&rm, true);

            // even for null moves, we increase the depth, because we want them to be
            // 'overrulable' by actual moves
            depth += 1;
            let game_over = self.board.game_status != GameStatus::Ongoing;
            let force_eval = depth >= self.eval_depth + self.eval_cap_line_len;
            if depth >= self.eval_depth || force_eval || game_over {
                let hash = em.hash;
                let mut score = em.score;
                let score_now = if game_over || force_eval {
                    true
                } else {
                    !self.gen_engine_depth(depth, Some(rm.clone()), true)
                };

                if score_now {
                    if score[usize::from(self.board.turn)] == Eval::MIN {
                        score = calculate_position_score(&mut self.board)
                    }
                    self.pos_count += 1;
                    if self.debug_log {
                        println!(
                            "score: ({}): {} (score_now: {})",
                            score_str(score),
                            self.engine_line_str(depth, Some(&rm)),
                            score_now
                        );
                    }
                    self.transposition_table.insert(
                        hash,
                        Transposition::new(
                            None,
                            score,
                            self.board.move_index,
                            self.eval_max_move_index,
                        ),
                    );
                    self.board.revert_move(&rm);
                    depth -= 1;

                    propagation_result = self.propagate_move_score(depth, rm.mov, score);
                    continue;
                }
                if self.debug_log {
                    if self.debug_log {
                        println!(
                            "expanding unstable position @depth {}: {}",
                            depth,
                            self.engine_line_str(depth, Some(&rm)),
                        );
                    }
                }
            } else {
                if self.debug_log {
                    println!(
                        "expanding further @depth {}: {}",
                        depth,
                        self.engine_line_str(depth, Some(&rm)),
                    );
                }
                self.gen_engine_depth(depth, Some(rm), depth >= self.eval_depth);
            }
            if Instant::now().gt(&end) {
                return Err(());
            }
        }
    }
}
