use crate::board::*;
use crate::movegen::*;
use num_traits::FromPrimitive;
use std::ffi::CStr;
use std::fmt::Write;
use surena_game;
use surena_game::*;
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

impl std::convert::TryFrom<u64> for Move {
    type Error = ();
    fn try_from(v: u64) -> std::result::Result<Move, ()> {
        let source = (v & 0xFF) as u8;
        let target = ((v >> 8) & 0xFF) as u8;
        let mtype = ((v >> 16) & 0xFF) as u8;
        let b1 = ((v >> 24) & 0xFF) as u8;
        let b2 = ((v >> 32) & 0xFF) as u8;
        let move_type = match mtype {
            0 => Slide,
            1 => Capture(b1.try_into()?),
            2 => EnPassant(b1.try_into()?, FieldLocation::from_checked(b2)?),
            3 => Castle(
                FieldLocation::from_checked(b1)?,
                FieldLocation::from_checked(b2)?,
            ),
            4 => Promotion(PieceType::from_u8(b1).ok_or(())?),
            5 => ClaimDraw,
            _ => return Err(()),
        };
        Ok(Move {
            move_type,
            source: FieldLocation::from_checked(source as u8)?,
            target: FieldLocation::from_checked(target as u8)?,
        })
    }
}

impl std::convert::From<Move> for u64 {
    fn from(m: Move) -> Self {
        let move_type: u64 = match m.move_type {
            Slide => 0,
            Capture(piece) => 1 | (u8::from(piece) as u64) << 8,
            EnPassant(piece, loc) => {
                2 | (u8::from(piece) as u64) << 8 | (u8::from(loc) as u64) << 16
            }
            Castle(src, tgt) => 3 | (u8::from(src) as u64) << 8 | (u8::from(tgt) as u64) << 16,
            Promotion(piece) => 4 | (u8::from(piece) as u64) << 8,
            ClaimDraw => 5,
        };
        move_type << 16 | (u8::from(m.source) as u64) << 8 | (u8::from(m.target) as u64)
    }
}

fn parse_move_string(game: &mut ThreePlayerChess, string: &str) -> Option<Move> {
    if string == "O-O" {
        return game.gen_move_castling(false);
    }
    if string == "O-O-O" {
        return game.gen_move_castling(true);
    }
    if string == "draw" {
        return Some(Move {
            source: Default::default(),
            target: Default::default(),
            move_type: MoveType::ClaimDraw,
        });
    }

    let src = AnnotatedFieldLocation::from_field_with_origin(
        game.turn,
        FieldLocation::from_str(&string[0..2])?,
    );
    let tgt = AnnotatedFieldLocation::from_field_with_origin(
        game.turn,
        FieldLocation::from_str(&string[2..4])?,
    );

    let src_val = FieldValue::from(game.board[usize::from(src.loc)]);

    if src_val.is_none() {
        return None;
    }

    let tgt_val = FieldValue::from(game.board[usize::from(src.loc)]);

    if string.len() > 4 {
        let promotion: [u8; 2] = string[4..].as_bytes().try_into().ok()?;
        if promotion[0] != '='.try_into().unwrap() {
            return None;
        }
        let piece_type = PieceType::try_from(promotion[1]).ok()?;
        return Some(Move {
            move_type: MoveType::Promotion(piece_type),
            source: src.loc,
            target: tgt.loc,
        });
    }
    let (_, src_piece_type) = src_val.unwrap();
    if tgt_val.is_some() {
        Some(Move {
            move_type: MoveType::Capture(tgt_val.into()),
            source: src.loc,
            target: tgt.loc,
        })
    } else if src_piece_type == PieceType::Pawn && tgt.file != src.file {
        let ep_square = move_rank(tgt, false)?;
        Some(Move {
            move_type: MoveType::EnPassant(game.board[usize::from(ep_square.loc)], ep_square.loc),
            source: src.loc,
            target: tgt.loc,
        })
    } else {
        Some(Move {
            move_type: MoveType::Slide,
            source: src.loc,
            target: tgt.loc,
        })
    }
}

fn check_move_valid(game: &mut ThreePlayerChess, mov: Move) -> bool {
    // this is not used by engines, and therfore not performance critical
    // we are therefore fine with using a rather inefficient implementation
    for candidate_move in game.gen_moves() {
        if mov == candidate_move {
            return true;
        }
    }
    false
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
        *self = other.clone();
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
        //TODO: optimize away this copy + allocation?
        for mov in self.gen_moves() {
            moves.push(u64::from(mov));
        }
        Ok(())
    }
    fn is_legal_move(&mut self, player: player_id, mov: move_code, _sync_ctr: u32) -> Result<()> {
        let mov = Move::try_from(mov)
            .map_err(|_| Error::new_static(ErrorCode::InvalidInput, b"invalid move code\0"))?;
        if Color::from(player) == self.turn && check_move_valid(self, mov) {
            Ok(())
        } else {
            Err(Error::new_static(
                ErrorCode::InvalidInput,
                b"illegal move\0",
            ))
        }
    }

    fn make_move(&mut self, _player: player_id, mov: move_code) -> Result<()> {
        let mov = Move::try_from(mov)
            .map_err(|_| Error::new_static(ErrorCode::InvalidInput, b"invalid move code\0"))?;
        self.make_move(mov);
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

    fn get_move_code(&mut self, _player: player_id, string: &str) -> Result<move_code> {
        parse_move_string(self, string)
            .and_then(|mov| {
                if check_move_valid(self, mov) {
                    Some(mov.into())
                } else {
                    None
                }
            })
            .ok_or_else(|| Error::new_static(ErrorCode::InvalidInput, b"failed to parse move\0"))
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

fn cstr(bytes: &[u8]) -> &CStr {
    CStr::from_bytes_with_nul(bytes).expect("invalid C string")
}

plugin_get_game_methods!(example_game_methods());
