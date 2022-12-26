use crate::movegen::*;
use crate::zobrist::*;
use arrayvec::ArrayString;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::char;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::Write;
use std::num::NonZeroU8;
use MoveType::*;
use PieceType::*;

pub const ROW_SIZE: usize = 8; // row == rank
pub const HB_ROW_COUNT: usize = 4; // each halfboard has 4 rows
pub const HB_COUNT: usize = 3; // number of half boards
pub const COLOR_COUNT: u8 = HB_COUNT as u8;
pub const PIECE_COUNT: usize = 6;
pub const HB_SIZE: usize = ROW_SIZE * HB_ROW_COUNT;
pub const BOARD_SIZE: usize = HB_SIZE * HB_COUNT; // 96

pub const DRAW_AFTER_N_SLIDES: usize = 50 * HB_COUNT;

pub const BOARD_STRING: &'static str = include_str!("board.txt");

pub const START_POSITION_STRING: &'static str = concat!(
    "ABCDEFGH2/BG1/CF1/AH1/D1/E1/AH/:",
    "LKJIDCBA7/KB8/JC8/LA8/I8/D8/LA/:",
    "HGFEIJKLb/GKc/FJc/HLc/Ec/Ic/HL/:",
    "0:0"
);
// 2 characters for each board cell
// for each player: 7 slashes + 2 castling + 2 e.p. + 1 ':'
// 2 times 5 characters for move and pawn/capture index each (max value 65535)
// 1 final character for the bar between the two indices
pub const MAX_POSITION_STRING_SIZE: usize = BOARD_SIZE * 2 + 3 * (7 + 2 + 2 + 1) + 2 * 5 + 1;

// regular move has max 11 characters, e.g.: 'exf10 e.p.(#)'
// because of draw claims we have 16 (draw move can't be capture): "Ra3c3(+) draw50"
pub const MAX_MOVE_STRING_SIZE: usize = 16;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug, FromPrimitive)]
pub enum PieceType {
    Pawn = 0,
    Knight = 1,
    Bishop = 2,
    Rook = 3,
    Queen = 4,
    King = 5,
}
impl std::convert::TryFrom<u8> for PieceType {
    type Error = ();
    fn try_from(v: u8) -> Result<PieceType, ()> {
        FromPrimitive::from_u8(v).ok_or(())
    }
}
impl std::convert::From<PieceType> for u8 {
    fn from(v: PieceType) -> u8 {
        v as u8
    }
}
impl std::convert::From<PieceType> for usize {
    fn from(v: PieceType) -> usize {
        v as u8 as usize
    }
}
impl std::fmt::Display for PieceType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_ascii() as char)
    }
}

impl PieceType {
    pub fn iter() -> std::slice::Iter<'static, PieceType> {
        // this is ordered by value (ascending). changing this order would
        // change the string representation
        static PIECE_TYPES: [PieceType; PIECE_COUNT] = [Pawn, Knight, Bishop, Rook, Queen, King];
        PIECE_TYPES.iter()
    }
    pub fn from_ascii(chr: u8) -> Option<Self> {
        match char::try_from(chr).ok()? {
            'P' => Pawn,
            'N' => Knight,
            'B' => Bishop,
            'R' => Rook,
            'Q' => Queen,
            'K' => King,
            _ => return None,
        }
        .into()
    }
    pub fn to_ascii(self) -> u8 {
        match self {
            Pawn => 'P',
            Knight => 'N',
            Bishop => 'B',
            Rook => 'R',
            Queen => 'Q',
            King => 'K',
        }
        .try_into()
        .unwrap()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Color(NonZeroU8);

impl Color {
    pub fn iter() -> std::slice::Iter<'static, Color> {
        static COLORS: [Color; 3] = unsafe {
            [
                Color(NonZeroU8::new_unchecked(1)),
                Color(NonZeroU8::new_unchecked(2)),
                Color(NonZeroU8::new_unchecked(3)),
            ]
        };
        COLORS.iter()
    }
    pub fn next(self) -> Color {
        Color::from_u8((u8::from(self) + 1) % HB_COUNT as u8)
    }
    pub fn prev(self) -> Color {
        Color::from_u8((u8::from(self) + HB_COUNT as u8 - 1) % HB_COUNT as u8)
    }
    pub fn from_u8(v: u8) -> Color {
        Color::try_from(v).unwrap()
    }
}
impl std::convert::TryFrom<u8> for Color {
    type Error = ();
    fn try_from(v: u8) -> Result<Color, ()> {
        Ok(Color(NonZeroU8::new(v + 1).ok_or(())?))
    }
}
impl std::convert::From<Color> for u8 {
    fn from(v: Color) -> u8 {
        u8::from(v.0) - 1
    }
}
impl std::convert::From<Color> for usize {
    fn from(v: Color) -> usize {
        u8::from(v) as usize
    }
}

impl Default for Color {
    fn default() -> Self {
        Color::try_from(0).unwrap()
    }
}
impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", u8::from(self.0))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum WinReason {
    Checkmate(Color),
    CapturableKing(Color),
    //todo: maybe allow resignation?
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum DrawClaimBasis {
    FiftyMoveRule = 0,
    ThreefoldRepetition = 1,
}

impl TryFrom<u8> for DrawClaimBasis {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            0 => Ok(DrawClaimBasis::FiftyMoveRule),
            1 => Ok(DrawClaimBasis::ThreefoldRepetition),
            _ => Err(()),
        }
    }
}
impl From<DrawClaimBasis> for u8 {
    fn from(v: DrawClaimBasis) -> u8 {
        v as u8
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum DrawReason {
    Stalemate(Color),
    InsufficientMaterial, //TODO: implement this
    DrawClaimed(DrawClaimBasis),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum GameStatus {
    Ongoing,
    Win(Color, WinReason),
    Draw(DrawReason),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MoveType {
    Slide,
    ClaimDraw(DrawClaimBasis),
    // when the player that does the move that leads to repetition etc.
    // wants to immediately claim the draw
    // we don't need CaptureClaimDraw since
    // neither repetition nor the 50 move rule can happen on a capture
    // TODO: we need CastleClaimDraw. oof.
    SlideClaimDraw(DrawClaimBasis),
    Capture(PackedFieldValue),                  // (captured piece)
    EnPassant(PackedFieldValue, FieldLocation), // (captured piece, captured piece square)
    Castle(bool), // long castle(left) is false, short castle (right) is true
    Promotion(PieceType),
    CapturePromotion(PackedFieldValue, PieceType), // (captured piece, promotion piece type)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Move {
    pub move_type: MoveType,
    pub source: FieldLocation,
    pub target: FieldLocation,
}

#[derive(Clone, Debug)]
pub struct ReversableMove {
    pub mov: Move,
    pub possible_en_passant: [Option<FieldLocation>; HB_COUNT],
    // like in ThreePlayerChess, long is first slot, short is second
    pub possible_rooks_for_castling: [[Option<FieldLocation>; 2]; HB_COUNT],
    pub last_capture_or_pawn_move_index: u16,
    pub zobrist_hash_value: u64,
    #[cfg(feature = "debug_movegen")]
    pub state_before: String,
    #[cfg(feature = "debug_movegen")]
    pub state_after: String,
}

impl ReversableMove {
    pub fn new(board: &ThreePlayerChess, mov: Move) -> ReversableMove {
        ReversableMove {
            mov,
            possible_en_passant: board.possible_en_passant,
            possible_rooks_for_castling: board.possible_rooks_for_castling,
            last_capture_or_pawn_move_index: board.last_capture_or_pawn_move_index,
            zobrist_hash_value: board.zobrist_hash.value,
            #[cfg(feature = "debug_movegen")]
            state_before: board.state_string(),
            #[cfg(feature = "debug_movegen")]
            state_after: {
                let mut b = board.clone();
                b.apply_move(mov);
                b.apply_move_sideeffects(mov);
                b.state_string()
            },
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub struct AnnotatedFieldLocation {
    pub origin: Color,
    pub loc: FieldLocation,
    pub hb: Color,
    pub file: i8,
    pub rank: i8,
}

// this is not used for board storage because it's sadly
// two bytes large. it can be converted from / into PackedFieldValue alias u8,
// which is actually used for storage
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FieldValue(pub Option<(Color, PieceType)>);

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub struct PackedFieldValue(u8);

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldLocation(std::num::NonZeroU8);

impl Debug for FieldLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("FieldLocation")
            .field(&self.to_str_fancy())
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct ThreePlayerChess {
    pub turn: Color,
    pub possible_en_passant: [Option<FieldLocation>; HB_COUNT],
    // castling long(left) is in the first slot, short(right) in the second one
    pub possible_rooks_for_castling: [[Option<FieldLocation>; 2]; HB_COUNT],
    pub king_positions: [FieldLocation; HB_COUNT],
    pub move_index: u16,
    pub last_capture_or_pawn_move_index: u16,
    pub game_status: GameStatus,
    pub board: [PackedFieldValue; BOARD_SIZE],
    pub zobrist_hash: ZobristHash,
    pub dummy_vec: Option<Vec<Move>>, //used in movegen to avoid uneccessary allocations
    pub moves_for_board: &'static MovesForBoard, // to avoid the atomic for the lazy static during movegen
}

impl ThreePlayerChess {
    pub fn new() -> ThreePlayerChess {
        let mut tpc = ThreePlayerChess {
            turn: Color::from_u8(0),
            possible_en_passant: [None; HB_COUNT],
            possible_rooks_for_castling: Default::default(),
            king_positions: Default::default(),
            move_index: 0,
            last_capture_or_pawn_move_index: 0,
            game_status: GameStatus::Ongoing,
            board: [FieldValue(None).into(); BOARD_SIZE],
            zobrist_hash: ZobristHash::default(),
            dummy_vec: Some(Vec::new()),
            moves_for_board: &*MOVES_FOR_BOARD,
        };
        tpc.recalc_zobrist();
        tpc
    }
    pub fn recalc_zobrist(&mut self) -> u64 {
        let mut zh = self.zobrist_hash;
        zh.recalc_zobrist(self);
        self.zobrist_hash = zh;
        zh.value
    }
    fn player_state_from_str<'a>(
        &mut self,
        color: Color,
        pstr: &'a str,
    ) -> Result<&'a str, &'static str> {
        let mut files = [0u8; ROW_SIZE];
        let bytes = pstr.as_bytes();
        let ci = usize::from(color);
        let mut i = 0;
        let mut exactly_one_king = false;
        for piece_type in PieceType::iter() {
            let mut file_count = 0;
            loop {
                if i == bytes.len() {
                    return Err("unexpected end of string");
                }
                let b = bytes[i];
                let c = char::from(b);
                i += 1;

                if c == '/' {
                    if file_count != 0 {
                        return Err("unterminated file list");
                    }
                    break;
                }
                if b.is_ascii_digit() || (c >= 'a' && c <= 'c') {
                    if file_count == 0 {
                        return Err("rank specified without file");
                    }
                    for f in 0..file_count {
                        let square_str = [files[f], b];
                        let loc = FieldLocation::from_utf8(square_str)
                            .map(|loc| Ok(loc))
                            .unwrap_or(Err("invalid field location"))?;
                        let square = &mut self.board[usize::from(loc)];
                        if FieldValue::from(*square).is_some() {
                            return Err("field respecified");
                        }
                        *square = FieldValue(Some((color, *piece_type))).into();
                        if *piece_type == PieceType::King {
                            if file_count != 1
                                || (bytes.len() == i || bytes[i] != '/'.try_into().unwrap())
                            {
                                exactly_one_king = false;
                                break;
                            }
                            self.king_positions[ci] = loc;
                            exactly_one_king = true;
                        }
                    }
                    file_count = 0;
                } else if c >= 'A' && c <= 'L' {
                    if file_count == ROW_SIZE {
                        return Err("file list too long");
                    }
                    files[file_count] = b;
                    file_count += 1;
                }
            }
        }
        if !exactly_one_king {
            return Err("players must have exactly one king!");
        }
        for _ in 0..2 {
            if i == bytes.len() {
                return Err("unexpected end of string");
            }
            let b = bytes[i];

            if b >= 'A'.try_into().unwrap() && b <= 'L'.try_into().unwrap() {
                let field =
                    FieldLocation::from_utf8([b, FieldLocation::from(ci * HB_SIZE).rank_char()])
                        .ok_or("invalid castling file")?;
                let short = get_raw_file(field) > get_raw_file(self.king_positions[ci]);
                if self.possible_rooks_for_castling[ci][short as usize].is_some() {
                    return Err("conflicting castling files");
                }
                self.possible_rooks_for_castling[ci][short as usize] = Some(field);
                i += 1;
            } else {
                break;
            }
        }
        if i == bytes.len() {
            return Err("unexpected end of string");
        }
        if bytes[i] != '/'.try_into().unwrap() {
            return Err("expected '/' after castling files");
        }
        i += 1;
        let pstr = std::str::from_utf8(&pstr.as_bytes()[i..]).unwrap();
        let pipe_pos = pstr
            .find(":")
            .map(|p| Ok(p))
            .unwrap_or(Err("expected en passant location or ':'"))?;
        if pipe_pos == 0 {
            Ok(&pstr[1..])
        } else {
            let (ep_str, end) = pstr.split_at(pipe_pos);
            let ep_square = FieldLocation::from_str(ep_str)
                .map(|l| Ok(l))
                .unwrap_or(Err("invalid en passant square"))?;
            self.possible_en_passant[usize::from(color)] = Some(ep_square);
            Result::Ok(&end[1..])
        }
    }
    pub fn from_str(pstr: &str) -> Result<ThreePlayerChess, &'static str> {
        let mut tpc = ThreePlayerChess::new();
        let mut pstr_it = pstr;
        for c in Color::iter() {
            pstr_it = tpc.player_state_from_str(*c, pstr_it)?;
        }
        let pipe_pos = pstr_it
            .find(":")
            .map(|x| Ok(x))
            .unwrap_or(Err("expected capture/pawn move index followed by ':'"))?;
        let (cpi, mi) = pstr_it.split_at(pipe_pos);
        tpc.last_capture_or_pawn_move_index = cpi
            .parse()
            .or_else(|_| Err("capture/pawn move index is not a valid integer"))?;
        tpc.move_index = mi[1..]
            .parse()
            .or_else(|_| Err("move index is not a valid integer"))?;
        if tpc.last_capture_or_pawn_move_index > tpc.move_index {
            return Err("capture/pawn move index can't be larger than move index");
        }
        tpc.turn = Color::from_u8((tpc.move_index % HB_COUNT as u16) as u8);
        tpc.recalc_zobrist();
        tpc.game_status = tpc.game_status();
        Ok(tpc)
    }
    pub fn write_state_str<'a, W: std::fmt::Write>(
        &self,
        writer: &mut W,
    ) -> Result<(), std::fmt::Error> {
        for c in Color::iter() {
            for piece_type in PieceType::iter() {
                for hb in Color::iter() {
                    for rank in 1..HB_ROW_COUNT as i8 + 1 {
                        let mut piece_found = false;
                        for file in 1..ROW_SIZE as i8 + 1 {
                            let loc = FieldLocation::new(*hb, file, rank);
                            if let FieldValue(Some((col, piece))) =
                                self.board[usize::from(loc)].into()
                            {
                                if col == *c && piece == *piece_type {
                                    writer
                                        .write_fmt(format_args!("{}", loc.file_char() as char))?;
                                    piece_found = true;
                                }
                            }
                        }
                        if piece_found {
                            writer
                                .write_char(FieldLocation::new(*hb, 1, rank).rank_char() as char)?;
                        }
                    }
                }
                writer.write_char('/')?;
            }
            for i in 0..2 {
                if let Some(loc) = self.possible_rooks_for_castling[usize::from(*c)][i] {
                    writer.write_char(loc.file_char() as char)?;
                }
            }
            writer.write_char('/')?;
            if let Some(loc) = self.possible_en_passant[usize::from(*c)] {
                writer.write_fmt(format_args!("{}", loc.to_str()))?;
            }
            writer.write_char(':')?;
        }
        writer.write_fmt(format_args!(
            "{}:{}",
            self.last_capture_or_pawn_move_index, self.move_index
        ))?;
        Ok(())
    }
    pub fn state_string(&self) -> String {
        let mut string = String::new();
        self.write_state_str(&mut string).unwrap();
        string
    }
    pub fn get_zobrist_hash(&mut self) -> u64 {
        //assert!(self.zobrist_hash.value == ZobristHash::new(self).value);
        self.zobrist_hash.value
    }
}

impl std::ops::Deref for FieldValue {
    type Target = Option<(Color, PieceType)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl std::ops::DerefMut for FieldValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl std::convert::TryFrom<&str> for ThreePlayerChess {
    type Error = &'static str;
    fn try_from(pstr: &str) -> Result<Self, Self::Error> {
        ThreePlayerChess::from_str(pstr)
    }
}
impl Default for ThreePlayerChess {
    fn default() -> Self {
        Self::from_str(START_POSITION_STRING)
            .map_err(|err| println!("{:?}", err))
            .unwrap()
    }
}

lazy_static! {
    static ref BOARD_NOTATION: [[u8; 2]; BOARD_SIZE] = {
        let mut bn = [[0u8; 2]; BOARD_SIZE];
        fn setup_half_board(
            bn: &mut [[u8; 2]; BOARD_SIZE],
            half_board_id: usize,
            col_chars: &[char; ROW_SIZE],
            row_chars: &[char; HB_ROW_COUNT],
        ) {
            for (r, row_char) in row_chars.iter().enumerate() {
                for (c, col_char) in col_chars.iter().enumerate() {
                    bn[(r + half_board_id * HB_ROW_COUNT) * ROW_SIZE + c] =
                        [*col_char as u8, *row_char as u8];
                }
            }
        }
        setup_half_board(
            &mut bn,
            0,
            &['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'],
            &['1', '2', '3', '4'],
        );
        setup_half_board(
            &mut bn,
            1,
            &['L', 'K', 'J', 'I', 'D', 'C', 'B', 'A'],
            &['8', '7', '6', '5'],
        );
        setup_half_board(
            &mut bn,
            2,
            &['H', 'G', 'F', 'E', 'I', 'J', 'K', 'L'],
            &['c', 'b', 'a', '9'],
        );
        bn
    };
    static ref BOARD_NOTATION_MAP: HashMap<[u8; 2], FieldLocation> = {
        let mut nm = HashMap::new();
        for i in 0..BOARD_SIZE {
            nm.insert(BOARD_NOTATION[i], FieldLocation::from(i as u8));
        }
        nm
    };
    static ref BOARD_STRING_FIELD_LOCATIONS: [usize; BOARD_SIZE] = {
        let mut bsfl = [0usize; BOARD_SIZE];
        for (i, field_name) in BOARD_NOTATION.iter().enumerate() {
            bsfl[i] = BOARD_STRING
                .find(&String::from_utf8_lossy(field_name).to_owned().to_string())
                .unwrap();
        }
        bsfl
    };
}

impl std::fmt::Display for FieldValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            FieldValue(None) => write!(f, "__"),
            FieldValue(Some((color, piece_type))) => {
                write!(f, "{}{}", piece_type, color)
            }
        }
    }
}
impl std::fmt::Display for ThreePlayerChess {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut board_str: [u8; BOARD_STRING.len()] = BOARD_STRING.as_bytes().try_into().unwrap();
        for (field, value) in self.board.iter().enumerate() {
            let loc = BOARD_STRING_FIELD_LOCATIONS[field];
            write!(&mut board_str[loc..loc + 2], "{}", FieldValue::from(*value)).unwrap();
        }
        write!(f, "{}", std::str::from_utf8(&board_str).unwrap())
    }
}
impl std::default::Default for FieldLocation {
    fn default() -> FieldLocation {
        FieldLocation::from(0 as u8)
    }
}
impl std::convert::From<u8> for FieldLocation {
    fn from(v: u8) -> FieldLocation {
        assert!(v < BOARD_SIZE as u8);
        FieldLocation(std::num::NonZeroU8::new(v + 1).unwrap())
    }
}
impl std::convert::From<FieldLocation> for u8 {
    fn from(v: FieldLocation) -> u8 {
        v.0.get() - 1
    }
}
impl std::convert::From<usize> for FieldLocation {
    fn from(v: usize) -> FieldLocation {
        FieldLocation::from(u8::try_from(v).unwrap())
    }
}
impl std::convert::From<FieldLocation> for usize {
    fn from(v: FieldLocation) -> usize {
        u8::from(v) as usize
    }
}
impl std::convert::From<FieldLocation> for [u8; 2] {
    fn from(v: FieldLocation) -> [u8; 2] {
        BOARD_NOTATION[usize::from(v)]
    }
}
impl FieldLocation {
    pub fn from_checked(v: u8) -> Result<FieldLocation, ()> {
        if v >= BOARD_SIZE as u8 {
            return Err(());
        }
        Ok(FieldLocation::from(v))
    }
    pub fn from_utf8(board_str: [u8; 2]) -> Option<Self> {
        BOARD_NOTATION_MAP
            .get(&board_str)
            .map(|field_not| *field_not)
    }
    pub fn from_str(field_name: &str) -> Option<Self> {
        let board_str: Result<[u8; 2], _> = field_name.as_bytes()[0..2].try_into();
        if let Result::Ok(board_str) = board_str {
            Self::from_utf8(board_str)
        } else {
            Option::None
        }
    }
    pub fn new(hb: Color, file: i8, rank: i8) -> FieldLocation {
        FieldLocation::from(
            (usize::from(hb)) * HB_SIZE + (rank - 1) as usize * ROW_SIZE + (file - 1) as usize,
        )
    }
    pub fn file_char(self) -> u8 {
        <[u8; 2]>::from(self)[0]
    }
    pub fn file_char_fancy(self) -> u8 {
        self.file_char().to_ascii_lowercase()
    }
    pub fn rank_char(self) -> u8 {
        <[u8; 2]>::from(self)[1]
    }
    pub fn rank_char_fancy(self) -> ArrayString<2> {
        let rc = self.rank_char();
        let mut res = ArrayString::new();
        match rc as char {
            'a' => res.push_str("10"),
            'b' => res.push_str("11"),
            'c' => res.push_str("12"),
            _ => res.push(rc as char),
        }
        res
    }
    pub fn to_str(self) -> &'static str {
        unsafe {
            return std::str::from_utf8_unchecked(&BOARD_NOTATION[usize::from(self)]);
        }
    }
    pub fn to_str_fancy(self) -> ArrayString<3> {
        let mut res = ArrayString::new();
        res.push(self.file_char_fancy() as char);
        res.push_str(&self.rank_char_fancy().as_str());
        res
    }

    pub fn hb(self) -> Color {
        Color::from_u8((usize::from(self) / HB_SIZE) as u8)
    }
    pub fn is_right_side(self) -> bool {
        usize::from(self) % ROW_SIZE >= ROW_SIZE / 2
    }
}
impl FieldValue {
    pub fn piece_type(&self) -> Option<PieceType> {
        if let Some((_, piece_type)) = **self {
            Some(piece_type)
        } else {
            None
        }
    }
    pub fn color(&self) -> Option<Color> {
        if let Some((color, _)) = **self {
            Some(color)
        } else {
            None
        }
    }
}

impl std::convert::From<FieldValue> for PackedFieldValue {
    fn from(v: FieldValue) -> PackedFieldValue {
        let val_raw = match v {
            FieldValue(Some((color, piece_type))) => {
                u8::from(piece_type) << 2 | (u8::from(color) + 1)
            }
            FieldValue(None) => 0,
        };
        PackedFieldValue(val_raw)
    }
}

impl std::convert::From<PackedFieldValue> for FieldValue {
    fn from(v: PackedFieldValue) -> FieldValue {
        let v = u8::from(v);
        if v == 0 {
            FieldValue(None)
        } else {
            FieldValue(Some((
                Color::from_u8((v & 0x3) - 1),
                PieceType::try_from(v >> 2).unwrap(),
            )))
        }
    }
}

impl std::convert::From<PackedFieldValue> for u8 {
    fn from(v: PackedFieldValue) -> u8 {
        v.0
    }
}

impl std::convert::TryFrom<u8> for PackedFieldValue {
    type Error = ();
    fn try_from(v: u8) -> Result<PackedFieldValue, ()> {
        if v == 0 {
            Ok(PackedFieldValue(v))
        } else {
            Color::try_from(v & 0x3 - 1)?;
            PieceType::try_from(v >> 2)?;
            Ok(PackedFieldValue(v))
        }
    }
}

impl Move {
    pub fn new(source: FieldLocation, target: FieldLocation, move_type: MoveType) -> Move {
        Move {
            source,
            target,
            move_type,
        }
    }
    fn parse_special_move(game: &mut ThreePlayerChess, string: &str) -> Option<Move> {
        if string == "O-O" {
            let mp = MovegenParams::new(game, MovegenOptions::default());
            return game.gen_move_castling(true, &mp);
        }
        if string == "O-O-O" {
            let mp = MovegenParams::new(game, MovegenOptions::default());
            return game.gen_move_castling(false, &mp);
        }
        let draw_claim_basis = if string == "draw" {
            if game.threefold_repetition_applies() {
                Some(DrawClaimBasis::FiftyMoveRule)
            } else {
                Some(DrawClaimBasis::ThreefoldRepetition)
            }
        } else if string == "draw50" {
            Some(DrawClaimBasis::FiftyMoveRule)
        } else if string == "draw3" {
            Some(DrawClaimBasis::ThreefoldRepetition)
        } else {
            None
        };
        if let Some(dcb) = draw_claim_basis {
            return Some(Move {
                source: Default::default(),
                target: Default::default(),
                move_type: ClaimDraw(dcb),
            });
        }
        None
    }

    fn parse_regular_move_algebraic(game: &mut ThreePlayerChess, string: &str) -> Option<Move> {
        let src = AnnotatedFieldLocation::from_with_origin(
            game.turn,
            FieldLocation::from_str(&string[0..2])?,
        );
        let tgt = AnnotatedFieldLocation::from_with_origin(
            game.turn,
            FieldLocation::from_str(&string[2..4])?,
        );

        let src_val = game.get_field_value(src.loc);

        if src_val.is_none() {
            return None;
        }

        let tgt_val = game.get_field_value(tgt.loc);
        let mut str_rem = &string[0..];
        let mut draw_claim_basis: Option<DrawClaimBasis> = None;
        if string.ends_with(" draw") {
            // debatable wethere we should have separate move encodings for this
            if game.fifty_move_rule_applies() {
                draw_claim_basis = Some(DrawClaimBasis::FiftyMoveRule)
            } else {
                draw_claim_basis = Some(DrawClaimBasis::ThreefoldRepetition)
            }
            str_rem = &string[0..string.len() - 5];
        } else if string.ends_with(" draw50") {
            draw_claim_basis = Some(DrawClaimBasis::FiftyMoveRule);
            str_rem = &string[0..string.len() - 8];
        } else if string.ends_with(" draw3") {
            draw_claim_basis = Some(DrawClaimBasis::ThreefoldRepetition);
            str_rem = &string[0..string.len() - 7];
        }
        if str_rem.len() > 4 {
            let promotion: [u8; 2] = str_rem[4..].as_bytes().try_into().ok()?;
            if promotion[0] != '='.try_into().unwrap() {
                return None;
            }
            let piece_type = PieceType::from_ascii(promotion[1])?;
            return Some(Move {
                move_type: Promotion(piece_type),
                source: src.loc,
                target: tgt.loc,
            });
        }
        let (_, src_piece_type) = src_val.unwrap();
        let mov = if tgt_val.is_some() {
            Move {
                move_type: Capture(tgt_val.into()),
                source: src.loc,
                target: tgt.loc,
            }
        } else if src_piece_type == PieceType::Pawn && tgt.file != src.file {
            let ep_square = move_rank(tgt, false)?;
            Move {
                move_type: EnPassant(game.board[usize::from(ep_square.loc)], ep_square.loc),
                source: src.loc,
                target: tgt.loc,
            }
        } else {
            Move {
                move_type: if let Some(dcb) = draw_claim_basis {
                    SlideClaimDraw(dcb)
                } else {
                    Slide
                },
                source: src.loc,
                target: tgt.loc,
            }
        };
        if draw_claim_basis.is_some() && mov.move_type != SlideClaimDraw(draw_claim_basis.unwrap())
        {
            return None;
        }
        Some(mov)
    }

    fn parse_move_short(game: &mut ThreePlayerChess, string: &str) -> Option<Move> {
        // PERF: maybe implement this properly?
        for m in game.gen_moves() {
            if m.to_string(game).as_str() == string {
                return Some(m);
            }
        }
        None
    }

    pub fn from_str(game: &mut ThreePlayerChess, string: &str) -> Option<Move> {
        let mut mov = Self::parse_special_move(game, string);
        if mov.is_some() {
            return mov;
        }
        mov = Self::parse_regular_move_algebraic(game, string);
        if mov.is_some() {
            return mov;
        }
        Self::parse_move_short(game, string)
    }
    pub fn get_source_string(&self, game: &mut ThreePlayerChess) -> ArrayString<4> {
        let field_val = game.get_field_value(self.source);
        let (color, piece) = field_val.unwrap();
        let mut mov = *self;
        let mut mff = MovesForField::new_empty();
        let target_afl = AnnotatedFieldLocation::from(self.target);
        let mut disambiguate_file = false;
        let mut disambiguate_rank = false;
        let fc = self.source.file_char();
        let mut check_move_ambiguities = |src: FieldLocation| {
            if src == self.source {
                return;
            }
            if game.get_field_value(src) != field_val {
                return;
            }
            mov.source = src;
            if !game.is_valid_move(mov) {
                return;
            }
            if src.file_char() != fc {
                disambiguate_file = true;
            } else {
                disambiguate_rank = true;
            }
        };
        if piece == Knight {
            mff.add_knight_moves(target_afl);
            for nm in &mff.knight_moves {
                check_move_ambiguities(*nm);
            }
        }
        if piece == Bishop || piece == Queen {
            mff.add_diagonal_lines(target_afl);
            for dli in mff.diagonal_lines_iter() {
                for l in dli {
                    check_move_ambiguities(*l);
                }
            }
        }
        if piece == Rook || piece == Queen {
            mff.add_orthogonal_lines(target_afl);
            for cdi in mff.orthogonal_lines_iter() {
                for l in cdi {
                    check_move_ambiguities(*l);
                }
            }
        }
        let mut res = ArrayString::new();
        if piece == Pawn {
            let src_afl = AnnotatedFieldLocation::from_with_origin(color, self.source);
            let tgt_afl = AnnotatedFieldLocation::from_with_origin(color, self.target);
            if src_afl.file != tgt_afl.file {
                disambiguate_file = true;
            }
        } else {
            res.push(piece.to_ascii() as char);
        }
        if disambiguate_file {
            res.push(self.source.file_char_fancy() as char);
        }
        if disambiguate_rank {
            res.push_str(self.source.rank_char_fancy().as_str());
        }
        res
    }

    pub fn write_as_str<W: std::fmt::Write>(
        &self,
        game: &mut ThreePlayerChess,
        writer: &mut W,
    ) -> Result<(), std::fmt::Error> {
        match self.move_type {
            Castle(_) => {
                if AnnotatedFieldLocation::from(self.source).file
                    < AnnotatedFieldLocation::from(self.target).file
                {
                    writer.write_str("O-O")?
                } else {
                    writer.write_str("O-O-O")?
                }
            }
            Slide => writer.write_fmt(format_args!(
                "{}{}",
                self.get_source_string(game),
                self.target.to_str_fancy().as_str()
            ))?,
            SlideClaimDraw(dcb) => writer.write_fmt(format_args!(
                "{}{} draw{}",
                self.get_source_string(game),
                self.target.to_str_fancy().as_str(),
                if dcb == DrawClaimBasis::FiftyMoveRule {
                    "50"
                } else {
                    "3"
                }
            ))?,
            Capture(_) => writer.write_fmt(format_args!(
                "{}x{}",
                self.get_source_string(game),
                self.target.to_str_fancy().as_str()
            ))?,
            EnPassant(_, _) => writer.write_fmt(format_args!(
                "{}x{} e.p.",
                self.get_source_string(game),
                self.target.to_str_fancy().as_str()
            ))?,
            Promotion(piece_type) => writer.write_fmt(format_args!(
                "{}{}={}",
                self.target.file_char_fancy() as char,
                self.target.rank_char() as char,
                piece_type.to_ascii() as char,
            ))?,
            CapturePromotion(_, piece_type) => writer.write_fmt(format_args!(
                "{}x{}={}",
                self.source.file_char_fancy() as char,
                self.target.to_str_fancy().as_str(),
                piece_type.to_ascii() as char,
            ))?,
            ClaimDraw(dcb) => writer.write_fmt(format_args!(
                "draw{}",
                if dcb == DrawClaimBasis::FiftyMoveRule {
                    "50"
                } else {
                    "3"
                }
            ))?,
        }
        let turn = game.turn;
        let rm = ReversableMove::new(game, *self);

        game.perform_reversable_move(&rm, true);
        let gs = game.game_status;

        match gs {
            GameStatus::Ongoing => {
                if game
                    .is_piece_capturable_at(game.king_positions[usize::from(game.turn)], None, None)
                    .is_some()
                {
                    if game
                        .is_piece_capturable_at(
                            game.king_positions[usize::from(game.turn)],
                            Some(turn),
                            None,
                        )
                        .is_some()
                    {
                        writer.write_char('+')?;
                    } else {
                        writer.write_str("(+)")?;
                    }
                } else if game
                    .is_piece_capturable_at(
                        game.king_positions[usize::from(get_next_hb(turn, false))],
                        Some(turn),
                        None,
                    )
                    .is_some()
                {
                    writer.write_str("(+)")?;
                }
            }
            GameStatus::Win(winner, _) => {
                if winner == game.turn.prev() {
                    writer.write_char('#')?;
                } else {
                    writer.write_str("(#)")?;
                }
            }
            GameStatus::Draw(_) => (),
        }
        game.revert_move(&rm);
        Ok(())
    }
    pub fn to_string(&self, game: &mut ThreePlayerChess) -> ArrayString<MAX_MOVE_STRING_SIZE> {
        let mut res = ArrayString::new();
        self.write_as_str(game, &mut res).unwrap();
        res
    }
}

impl TryFrom<u64> for Move {
    type Error = ();
    fn try_from(v: u64) -> std::result::Result<Move, Self::Error> {
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
            4 => Promotion(PieceType::try_from(b1)?),
            5 => CapturePromotion(b1.try_into()?, PieceType::try_from(b2)?),
            6 => ClaimDraw(DrawClaimBasis::try_from(b1)?),
            7 => SlideClaimDraw(DrawClaimBasis::try_from(b1)?),
            _ => return Err(()),
        };
        Ok(Move {
            move_type,
            source: FieldLocation::from_checked(source as u8)?,
            target: FieldLocation::from_checked(target as u8)?,
        })
    }
}

impl From<Move> for u64 {
    fn from(m: Move) -> u64 {
        let move_type: u64 = match m.move_type {
            Slide => 0,
            Capture(piece) => 1 | (u8::from(piece) as u64) << 8,
            EnPassant(piece, loc) => {
                2 | (u8::from(piece) as u64) << 8 | (u8::from(loc) as u64) << 16
            }
            Castle(rook_src) => 3 | (u8::from(rook_src) as u64) << 8,
            Promotion(piece) => 4 | (u8::from(piece) as u64) << 8,
            CapturePromotion(cap, piece) => {
                5 | (u8::from(piece) as u64) << 8 | (u8::from(cap) as u64) << 16
            }
            ClaimDraw(claim_reason) => 6 | (u8::from(claim_reason) as u64) << 8,
            SlideClaimDraw(claim_reason) => 7 | (u8::from(claim_reason) as u64) << 8,
        };
        move_type << 16 | (u8::from(m.target) as u64) << 8 | (u8::from(m.source) as u64)
    }
}
