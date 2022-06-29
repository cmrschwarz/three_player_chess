use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use crate::movegen::*;
use std::char;
use std::collections::HashMap;
use std::io::Write;

pub const ROW_SIZE: usize = 8; // row == rank
pub const HB_ROW_COUNT: usize = 4; // each halfboard has 4 rows
pub const HB_COUNT: usize = 3; // number of half boards
pub const COLOR_COUNT: u8 = HB_COUNT as u8;
pub const HB_SIZE: usize = ROW_SIZE * HB_ROW_COUNT;
pub const BOARD_SIZE: usize = HB_SIZE * HB_COUNT; // 96

pub const BOARD_STRING: &'static str = include_str!("board.txt");

pub const START_POSITION_STRING: &'static str = concat!(
    "ABCDEFGH2/BG1/CF1/AH1/D1/E1/AH/|",
    "LKJIDCBA7/KB8/JC8/LA8/I8/D8/LA/|",
    "HGFEIJKLb/GKc/FJc/HLc/Ec/Ic/HL/|",
    "0|0"
);
// 2 characters for each board cell
// for each player: 7 slashes + 2 castling + 2 e.p. + 1 '|'
// 2 times 5 characters for move and pawn/capture index each (max value 65535)
// 1 final character for the bar between the two indices
pub const MAX_POSITION_STRING_SIZE: usize = BOARD_SIZE * 2 + 3 * (7 + 2 + 2 + 1) + 2 * 5 + 1;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive, Debug)]
pub enum PieceType {
    Pawn = 0,
    Knight = 1,
    Bishop = 2,
    Rook = 3,
    Queen = 4,
    King = 5,
}

impl PieceType {
    pub fn iter() -> std::slice::Iter<'static, PieceType> {
        use PieceType::*;
        // this is ordered by value (ascending). changing this order would
        // change the string representation
        static PIECE_TYPES: [PieceType; 6] = [Pawn, Knight, Bishop, Rook, Queen, King];
        PIECE_TYPES.iter()
    }
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive, Debug)]
pub enum Color {
    // these are assigned clockwise, with player 1 at the bottom
    C0 = 0,
    C1 = 1,
    C2 = 2,
}

impl Default for Color {
    fn default() -> Self {
        Self::C0
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum WinReason {
    Checkmate(Color),
    DoubleResign,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum DrawReason {
    Stalemate(Color),
    Agreement,
    InsufficientMaterial,
    FiftyMoveRule,
    ThreefoldRepetition,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum GameStatus {
    Ongoing,
    Win(Color, WinReason),
    Draw(DrawReason),
}

// this is not used for board storage because it's sadly
// two bytes large. it can be converted from / into PackedFieldValue alias u8,
// which is actually used for storage
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FieldValue(pub Option<(Color, PieceType)>);

pub type PackedFieldValue = u8;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FieldLocation(std::num::NonZeroU8);

#[derive(Clone)]
pub struct ThreePlayerChess {
    pub turn: Color,
    pub possible_en_passant: [Option<FieldLocation>; HB_COUNT],
    pub possible_rooks_for_castling: [[Option<FieldLocation>; 2]; HB_COUNT],
    pub king_positions: [Option<FieldLocation>; HB_COUNT],
    pub move_index: u16,
    pub last_capture_or_pawn_move_index: u16,
    pub game_status: GameStatus,
    pub resigned_player: Option<Color>,
    pub board: [PackedFieldValue; BOARD_SIZE],
    pub check_possibilities: CheckPossibilities,
}

impl ThreePlayerChess {
    pub fn new() -> ThreePlayerChess {
        ThreePlayerChess {
            turn: Color::C1,
            possible_en_passant: [None; HB_COUNT],
            possible_rooks_for_castling: Default::default(),
            king_positions: [None; HB_COUNT],
            move_index: 0,
            last_capture_or_pawn_move_index: 0,
            game_status: GameStatus::Ongoing,
            resigned_player: None,
            board: [FieldValue(None).into(); BOARD_SIZE],
            check_possibilities: CheckPossibilities::new(),
        }
    }
    fn player_state_from_str<'a>(
        &mut self,
        color: Color,
        pstr: &'a str,
    ) -> Result<&'a str, &'static str> {
        let mut files = [0u8; ROW_SIZE];
        let bytes = pstr.as_bytes();
        let mut i = 0;

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
                                return Err("each player must have one king");
                            }
                            self.king_positions[usize::from(color)] = Some(loc);
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
        for rook_i in 0..2 {
            if i == bytes.len() {
                return Err("unexpected end of string");
            }
            let b = bytes[i];
            i += 1;
            if b >= 'A'.try_into().unwrap() && b <= 'L'.try_into().unwrap() {
                let field = FieldLocation::from_utf8([
                    b,
                    FieldLocation::from(usize::from(color) * HB_SIZE).rank_char(),
                ]);
                if field.is_some() {
                    self.possible_rooks_for_castling[usize::from(color)][rook_i] = field;
                } else {
                    return Err("invalid castling file");
                }
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
            .find("|")
            .map(|p| Ok(p))
            .unwrap_or(Err("expected en passant location or '|'"))?;
        if pipe_pos == 0 {
            Ok(&pstr[1..])
        } else {
            let (ep_str, end) = pstr.split_at(pipe_pos);
            let ep_square = FieldLocation::from_str(ep_str)
                .map(|l| Ok(l))
                .unwrap_or(Err("invalid en passant square"))?;
            self.possible_en_passant[usize::from(color)] = Some(ep_square);
            Result::Ok(end)
        }
    }
    pub fn from_str(pstr: &str) -> Result<ThreePlayerChess, &'static str> {
        let mut tpc = ThreePlayerChess::new();
        let mut pstr_it = pstr;
        for c in u8::from(Color::C0)..u8::from(Color::C2) + 1 {
            pstr_it = tpc.player_state_from_str(Color::from(c), pstr_it)?;
        }
        let pipe_pos = pstr_it
            .find("|")
            .map(|x| Ok(x))
            .unwrap_or(Err("expected capture/pawn move index followed by '|'"))?;
        let (cpi, mi) = pstr_it.split_at(pipe_pos);
        tpc.last_capture_or_pawn_move_index = cpi
            .parse()
            .or_else(|_| Err("capture/pawn move index is not a valid integer"))?;
        tpc.move_index = mi[1..]
            .parse()
            .or_else(|_| Err("move index is not a valid integer"))?;
        tpc.turn = Color::from((tpc.move_index % HB_COUNT as u16) as u8);
        Ok(tpc)
    }
    pub fn write_state_str<'a, W: std::fmt::Write>(
        &self,
        writer: W,
    ) -> Result<(), std::fmt::Error> {
        for c in u8::from(Color::C0)..u8::from(Color::C2) + 1 {
            for piece_type in PieceType::iter() {
                for hb in u8::from(Color::C0)..u8::from(Color::C2) + 1 {
                    for rank in 1..HB_ROW_COUNT as i8 + 1 {
                        let mut piece_found = false;
                        for file in 1..ROW_SIZE as i8 + 1 {
                            let loc = FieldLocation::new(Color::from(hb), file, rank);
                            if let FieldValue(Some((col, piece))) =
                                self.board[usize::from(loc)].into()
                            {
                                if col == c.into() && piece == *piece_type {
                                    writer
                                        .write_fmt(format_args!("{}", loc.file_char() as char))?;
                                    piece_found = true;
                                }
                            }
                        }
                        if piece_found {
                            writer.write_char(
                                FieldLocation::new(Color::from(hb), 1, rank).rank_char() as char,
                            )?;
                        }
                    }
                }
                writer.write_char('/')?;
            }
            for i in 0..2 {
                if let Some(loc) = self.possible_rooks_for_castling[c as usize][i] {
                    writer.write_char(loc.file_char() as char)?;
                }
            }
            writer.write_char('/')?;
            if let Some(loc) = self.possible_en_passant[c as usize] {
                writer.write_fmt(format_args!("{}", loc))?;
            }
            writer.write_char('|')?;
        }
        writer.write_fmt(format_args!(
            "{}|{}",
            self.last_capture_or_pawn_move_index, self.move_index
        ))?;
        Ok(())
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

impl std::fmt::Display for PieceType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                PieceType::Pawn => "P",
                PieceType::Knight => "N",
                PieceType::Bishop => "B",
                PieceType::Rook => "R",
                PieceType::Queen => "Q",
                PieceType::King => "K",
            }
        )
    }
}
impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", ToPrimitive::to_i8(self).unwrap() + 1)
    }
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
impl std::fmt::Display for FieldLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", <&str>::from(*self))
    }
}
impl std::default::Default for FieldLocation {
    fn default() -> FieldLocation {
        FieldLocation::from(0 as u8)
    }
}
impl std::convert::From<u8> for FieldLocation {
    fn from(v: u8) -> FieldLocation {
        FieldLocation(std::num::NonZeroU8::new(v + 1).unwrap())
    }
}
impl std::convert::From<FieldLocation> for u8 {
    fn from(v: FieldLocation) -> u8 {
        v.0.get() - 1
    }
}
impl std::convert::From<Color> for u8 {
    fn from(v: Color) -> u8 {
        ToPrimitive::to_u8(&v).unwrap()
    }
}
impl std::convert::From<u8> for Color {
    fn from(v: u8) -> Color {
        Color::from_u8(v).unwrap()
    }
}
impl std::convert::From<Color> for usize {
    fn from(v: Color) -> usize {
        u8::from(v) as usize
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
impl std::convert::From<FieldLocation> for &str {
    fn from(v: FieldLocation) -> &'static str {
        unsafe {
            return std::str::from_utf8_unchecked(&BOARD_NOTATION[usize::from(v)]);
        }
    }
}
impl FieldLocation {
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
    pub fn file_char(&self) -> u8 {
        <[u8; 2]>::from(*self)[0]
    }
    pub fn rank_char(&self) -> u8 {
        <[u8; 2]>::from(*self)[1]
    }
}
impl std::convert::From<FieldValue> for PackedFieldValue {
    fn from(v: FieldValue) -> PackedFieldValue {
        match v {
            FieldValue(Some((color, piece_type))) => {
                ToPrimitive::to_u8(&piece_type).unwrap() << 2
                    | (ToPrimitive::to_u8(&color).unwrap() + 1)
            }
            FieldValue(None) => 0 as PackedFieldValue,
        }
    }
}
impl std::convert::From<PackedFieldValue> for FieldValue {
    fn from(v: PackedFieldValue) -> FieldValue {
        let color_val = v & 0x3;
        if color_val != 0 {
            if let Some(color) = Color::from_u8(color_val - 1) {
                if let Some(piece_type) = PieceType::from_u8(v >> 2) {
                    return FieldValue(Some((color, piece_type)));
                }
            }
        }
        FieldValue(None)
    }
}
