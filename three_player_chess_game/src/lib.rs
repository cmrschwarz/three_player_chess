use num_traits::FromPrimitive;
use std::ffi::CStr;
use std::fmt::Write;
use std::ops::{Deref, DerefMut};
use surena_game::*;
use three_player_chess::board::MoveType::*;
use three_player_chess::board::*;

const BUF_SIZER: buf_sizer = buf_sizer {
    options_str: 1,
    state_str: MAX_POSITION_STRING_SIZE,
    player_count: HB_COUNT as u8,
    max_players_to_move: 1,
    max_moves: 1024, // TODO: this is a very bad guess.
    max_actions: 0,
    max_results: 1,
    move_str: 4,
    print_str: BOARD_STRING.len() + 1,
};

#[derive(Clone, PartialEq, Eq, Default)]
struct TpcGame(ThreePlayerChess);

impl Deref for TpcGame {
    type Target = ThreePlayerChess;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TpcGame {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

fn move_from_u64(v: u64) -> std::result::Result<Move, ()> {
    let source = (v & 0xFF) as u8;
    let target = ((v >> 8) & 0xFF) as u8;
    let mtype = ((v >> 16) & 0xFF) as u8;
    let b1 = ((v >> 24) & 0xFF) as u8;
    let b2 = ((v >> 32) & 0xFF) as u8;
    let move_type = match mtype {
        0 => Slide,
        1 => Capture(b1.try_into()?),
        2 => EnPassant(b1.try_into()?, FieldLocation::from_checked(b2)?),
        3 => Castle(b1 > 0),
        4 => Promotion(PieceType::from_u8(b1).ok_or(())?),
        5 => CapturePromotion(b1.try_into()?, PieceType::from_u8(b2).ok_or(())?),
        6 => ClaimDraw,
        _ => return Err(()),
    };
    Ok(Move {
        move_type,
        source: FieldLocation::from_checked(source as u8)?,
        target: FieldLocation::from_checked(target as u8)?,
    })
}

fn move_to_u64(m: Move) -> u64 {
    let move_type: u64 = match m.move_type {
        Slide => 0,
        Capture(piece) => 1 | (u8::from(piece) as u64) << 8,
        EnPassant(piece, loc) => 2 | (u8::from(piece) as u64) << 8 | (u8::from(loc) as u64) << 16,
        Castle(rook_src) => 3 | (u8::from(rook_src) as u64) << 8,
        Promotion(piece) => 4 | (u8::from(piece) as u64) << 8,
        CapturePromotion(cap, piece) => {
            5 | (u8::from(piece) as u64) << 8 | (u8::from(cap) as u64) << 16
        }
        ClaimDraw => 6,
    };
    move_type << 16 | (u8::from(m.target) as u64) << 8 | (u8::from(m.source) as u64)
}

pub fn check_player_to_move(game: &mut ThreePlayerChess, player: player_id) -> Result<()> {
    if player != surena_game::PLAYER_NONE && Color::from(player - 1) != game.turn {
        Err(Error::new_static(
            ErrorCode::InvalidInput,
            b"wrong player\0",
        ))
    } else {
        Ok(())
    }
}

impl GameMethods for TpcGame {
    fn create_with_opts_str(_string: &str) -> Result<(Self, buf_sizer)> {
        Self::create_default() // don't have options yet
    }

    fn create_default() -> Result<(Self, buf_sizer)> {
        let game = Self::default();
        let sizer = BUF_SIZER;
        Ok((game, sizer))
    }

    fn export_options_str(&mut self, _str_buf: &mut StrBuf) -> Result<()> {
        Ok(()) //don't have options yet
    }

    fn copy_from(&mut self, other: &mut Self) -> Result<()> {
        *self = other.clone();
        Ok(())
    }

    fn import_state(&mut self, string: Option<&str>) -> Result<()> {
        if let Some(state_str) = string {
            match ThreePlayerChess::from_str(state_str) {
                Ok(tpc) => {
                    *self = TpcGame(tpc);
                    Ok(())
                }
                Err(err_str) => Err(Error::new_dynamic(ErrorCode::InvalidInput, err_str.into())),
            }
        } else {
            Err(Error::new_static(
                ErrorCode::InvalidInput,
                b"missing state string\0",
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
            _ => Ok(()),
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
        //TODO: optimize away this copy + allocation?
        for mov in self.gen_moves() {
            moves.push(move_to_u64(mov));
        }
        Ok(())
    }

    fn is_legal_move(&mut self, player: player_id, mov: move_code) -> Result<()> {
        let mov = move_from_u64(mov)
            .map_err(|_| Error::new_static(ErrorCode::InvalidInput, b"invalid move code\0"))?;
        check_player_to_move(self, player)?;
        if self.is_valid_move(mov) {
            Ok(())
        } else {
            Err(Error::new_static(
                ErrorCode::InvalidInput,
                b"illegal move\0",
            ))
        }
    }

    fn make_move(&mut self, _player: player_id, mov_code: move_code) -> Result<()> {
        let mov = move_from_u64(mov_code)
            .map_err(|_| Error::new_static(ErrorCode::InvalidInput, b"invalid move code\0"))?;
        self.0.make_move(mov);
        self.apply_move_sideeffects(mov);
        Ok(())
    }

    fn get_results(&mut self, players: &mut PtrVec<player_id>) -> Result<()> {
        match self.game_status {
            GameStatus::Win(color, _) => players.push(u8::from(color) + 1),
            _ => (),
        }
        Ok(())
    }

    fn get_move_code(&mut self, player: player_id, string: &str) -> Result<move_code> {
        check_player_to_move(self, player)?;
        Move::from_str(self, string)
            .map(|mov| move_to_u64(mov))
            .ok_or_else(|| Error::new_static(ErrorCode::InvalidInput, b"failed to parse move\0"))
    }

    fn debug_print(&mut self, str_buf: &mut StrBuf) -> Result<()> {
        write!(str_buf, "{}", **self).expect("failed to write print buffer");
        Ok(())
    }
    fn get_move_str(
        &mut self,
        player: player_id,
        mov: move_code,
        str_buf: &mut StrBuf,
    ) -> Result<()> {
        check_player_to_move(self, player)?;
        move_from_u64(mov)
            .map_err(|_| Error::new_static(ErrorCode::OutOfMemory, b"invalid move code\0"))?
            .write_as_str(self, str_buf)
            .map_err(|_| {
                Error::new_static(
                    ErrorCode::OutOfMemory,
                    b"move string too large for buffer\0",
                )
            })
    }
}

pub fn three_player_chess_game_methods() -> surena_game::surena::game_methods {
    let mut features = game_feature_flags::default();
    features.set_print(true);
    features.set_options(true);

    create_game_methods::<TpcGame>(Metadata {
        game_name: cstr(b"ThreePlayerChess\0"),
        variant_name: cstr(b"Standard\0"),
        impl_name: cstr(b"three_player_chess_game\0"),
        version: semver {
            major: 0,
            minor: 1,
            patch: 0,
        },
        features,
    })
}

fn cstr(bytes: &[u8]) -> &CStr {
    CStr::from_bytes_with_nul(bytes).expect("invalid C string")
}

surena_game::plugin_get_game_methods!(three_player_chess_game_methods());
