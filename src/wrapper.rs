use surena_game::*;

use std::ffi::CStr;
use std::fmt::Write;

use crate::board::*;
use crate::movegen::*;
use surena_game;
use MoveType::*;

const BUF_SIZER: buf_sizer = buf_sizer {
    options_str: 0,
    state_str: MAX_POSITION_STRING_SIZE,
    player_count: HB_COUNT as u8,
    max_players_to_move: 1,
    max_moves: 1024, // TODO: this is a very bad guess.
    max_actions: 0,
    max_results: 1,
    move_str: 4,
    print_str: BOARD_STRING.len(),
};

impl std::convert::From<usize> for Move {
    fn from(v: usize) -> Self {
        let source = v & 0xFF;
        let target = (v >> 8) & 0xFF;
        let mtv = v >> 16;
        let move_type = match mtv & 0xFF {
            0 => Slide,
            1 => PawnDoubleMove,
            2 => Capture(PieceType::from(mtv >> 8)),
            3 => EnPassant(
                PieceType::from(mtv >> 8 & 0xFF),
                FieldLocation::from(mtv >> 16),
            ),
            4 => Castle(
                FieldLocation::from(mtv >> 8 & 0xFF),
                FieldLocation::from(mtv >> 16),
            ),
            5 => Promotion(PieceType::from(mtv >> 8)),
            6 => ThreefoldRepetitionClaim,
            7 => FiftyMoveRuleClaim,
        };
        Move {
            move_type,
            source: source.into(),
            target: target.into(),
        }
    }
}

impl std::convert::From<Move> for usize {
    fn from(m: Move) -> Self {
        let move_type: usize = match m.move_type {
            Slide => 0,
            PawnDoubleMove => 1,
            Capture(piece) => 2 | usize::from(piece) << 8,
            EnPassant(piece, loc) => 3 | usize::from(piece) << 8 | usize::from(loc) << 16,
            Castle(src, tgt) => 4 | usize::from(src) << 8 | usize::from(tgt) << 16,
            Promotion(piece) => 5 | usize::from(piece) << 8,
            ThreefoldRepetitionClaim => 6,
            FiftyMoveRuleClaim => 7,
        };
        move_type << 16 | usize::from(m.source) << 8 | usize::from(m.target)
    }
}

impl GameMethods for ThreePlayerChess {
    fn create_default() -> Result<(Self, buf_sizer)> {
        let game = ThreePlayerChess::default();
        let sizer = BUF_SIZER;
        Ok((game, sizer))
    }

    fn export_options_str(&mut self, str_buf: &mut StrBuf) -> Result<()> {
        Ok(())
    }

    fn copy_from(&mut self, other: &mut Self) -> Result<()> {
        *self = *other;
        Ok(())
    }

    fn import_state(&mut self, string: Option<&str>) -> Result<()> {
        if let Some(state_str) = string {
            match ThreePlayerChess::from_str(state_str) {
                Ok(tpc) => {
                    *self = tpc;
                    Ok(())
                }
                Err(err_str) => Err(Error::new_dynamic(ErrorCode::InvalidInput, err_str.into())),
            }
        } else {
            Err(Error::new_static(
                ErrorCode::InvalidInput,
                b"missing state string",
            ))
        }
    }

    fn export_state(&mut self, str_buf: &mut StrBuf) -> Result<()> {
        self.write_state_str(str_buf).map_err(|_| {
            Error::new_static(
                ErrorCode::OutOfMemory,
                b"state string too large for buffer\0",
            )
        })
    }

    fn players_to_move(&mut self, players: &mut PtrVec<player_id>) -> Result<()> {
        match self.game_status {
            GameStatus::Ongoing => {
                players.push(u8::from(self.turn) + 1);
                Ok(())
            }
            _ => Err(Error::new_static(
                ErrorCode::InvalidInput,
                b"game already over\0",
            )),
        }
    }

    fn get_concrete_moves(
        &mut self,
        player: player_id,
        moves: &mut PtrVec<move_code>,
    ) -> Result<()> {
        if player != u8::from(self.turn) + 1 {
            return Ok(());
        }
        for mov in 1..=self.max_sub.min(self.counter) {
            moves.push(mov.into());
        }
        Ok(())
    }

    fn is_legal_move(&mut self, player: player_id, mov: move_code, _sync_ctr: u32) -> Result<()> {
        if self.counter == 0 {
            return Err(Error::new_static(
                ErrorCode::InvalidInput,
                b"game already over\0",
            ));
        }
        if mov == 0 {
            return Err(Error::new_static(
                ErrorCode::InvalidInput,
                b"need to subtract at least one\0",
            ));
        }
        if player != self.player_id() {
            return Err(Error::new_static(
                ErrorCode::InvalidInput,
                b"this player is not to move\0",
            ));
        }
        sub_too_large(mov as Counter, self.counter)?;
        Ok(())
    }

    fn make_move(&mut self, _player: player_id, mov: move_code) -> Result<()> {
        self.counter -= mov as Counter;
        self.turn = !self.turn;
        Ok(())
    }

    fn get_results(&mut self, players: &mut PtrVec<player_id>) -> Result<()> {
        if self.counter == 0 {
            players.push(self.player_id());
        }
        Ok(())
    }

    fn get_move_code(&mut self, _player: player_id, string: &str) -> Result<move_code> {
        let mov: Counter = string.parse().map_err(|e| {
            Error::new_dynamic(
                ErrorCode::InvalidInput,
                format!("move parsing error: {}", e),
            )
        })?;
        sub_too_large(mov, self.max_sub)?;
        Ok(mov.into())
    }

    fn debug_print(&mut self, str_buf: &mut StrBuf) -> Result<()> {
        self.export_state(str_buf)?;
        writeln!(str_buf).expect("failed to write print buffer");
        Ok(())
    }
}

fn example_game_methods() -> game_methods {
    let mut features = game_feature_flags::default();
    features.set_print(true);
    features.set_options(true);

    create_game_methods::<ThreePlayerChess>(Metadata {
        game_name: cstr(b"Three Player CHess\0"),
        variant_name: cstr(b"Standard\0"),
        impl_name: cstr(b"three_player_chess\0"),
        version: semver {
            major: 0,
            minor: 1,
            patch: 0,
        },
        features,
    })
}

fn sub_too_large(mov: Counter, max: Counter) -> Result<()> {
    if mov > max.into() {
        Err(Error::new_dynamic(
            ErrorCode::InvalidInput,
            format!("can subtract at most {}", max),
        ))
    } else {
        Ok(())
    }
}

fn cstr(bytes: &[u8]) -> &CStr {
    CStr::from_bytes_with_nul(bytes).expect("invalid C string")
}

const fn digits(mut n: Counter) -> Counter {
    let mut digits = 1;
    loop {
        n /= 10;
        if n == 0 {
            return digits;
        }
        digits += 1;
    }
}

plugin_get_game_methods!(example_game_methods());
