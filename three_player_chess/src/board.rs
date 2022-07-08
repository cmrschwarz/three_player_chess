use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use crate::movegen::*;
use arrayvec::ArrayString;
use std::char;
use std::collections::HashMap;
use std::io::Write;
use PieceType::*;

pub const ROW_SIZE: usize = 8; // row == rank
pub const HB_ROW_COUNT: usize = 4; // each halfboard has 4 rows
pub const HB_COUNT: usize = 3; // number of half boards
pub const COLOR_COUNT: u8 = HB_COUNT as u8;
pub const PIECE_COUNT: usize = 6;
pub const HB_SIZE: usize = ROW_SIZE * HB_ROW_COUNT;
pub const BOARD_SIZE: usize = HB_SIZE * HB_COUNT; // 96

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

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive, Debug)]
pub enum Color {
    // these are assigned clockwise, with player 1 at the bottom
    C0 = 0,
    C1 = 1,
    C2 = 2,
}

impl Color {
    pub fn iter() -> std::slice::Iter<'static, Color> {
        static COLORS: [Color; 3] = [Color::C0, Color::C1, Color::C2];
        COLORS.iter()
    }
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
    InsufficientMaterial, //TODO: implement this
    FiftyMoveRule,
    ThreefoldRepetition,
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
    Capture(PackedFieldValue),
    EnPassant(PackedFieldValue, FieldLocation),
    Castle(bool),
    Promotion(PieceType),
    CapturePromotion(PackedFieldValue, PieceType),
    ClaimDraw,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Move {
    pub move_type: MoveType,
    pub source: FieldLocation,
    pub target: FieldLocation,
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
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FieldLocation(std::num::NonZeroU8);

#[derive(Clone, PartialEq, Eq)]
pub struct ThreePlayerChess {
    pub turn: Color,
    pub possible_en_passant: [Option<FieldLocation>; HB_COUNT],
    pub possible_rooks_for_castling: [[Option<FieldLocation>; 2]; HB_COUNT],
    pub king_positions: [FieldLocation; HB_COUNT],
    pub move_index: u16,
    pub last_capture_or_pawn_move_index: u16,
    pub game_status: GameStatus,
    pub resigned_player: Option<Color>,
    pub board: [PackedFieldValue; BOARD_SIZE],
    pub check_possibilities: [CheckPossibilities; HB_COUNT],
}

impl ThreePlayerChess {
    pub fn new() -> ThreePlayerChess {
        ThreePlayerChess {
            turn: Color::C1,
            possible_en_passant: [None; HB_COUNT],
            possible_rooks_for_castling: Default::default(),
            king_positions: Default::default(),
            move_index: 0,
            last_capture_or_pawn_move_index: 0,
            game_status: GameStatus::Ongoing,
            resigned_player: None,
            board: [FieldValue(None).into(); BOARD_SIZE],
            check_possibilities: Default::default(),
        }
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
                            self.king_positions[ci] = loc;
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
        for _ in 0..2 {
            if i == bytes.len() {
                return Err("unexpected end of string");
            }
            let b = bytes[i];
            i += 1;
            if b >= 'A'.try_into().unwrap() && b <= 'L'.try_into().unwrap() {
                let field =
                    FieldLocation::from_utf8([b, FieldLocation::from(ci * HB_SIZE).rank_char()])
                        .ok_or("invalid castling file")?;
                let short = get_raw_file(field) > get_raw_file(self.king_positions[ci]);
                if self.possible_rooks_for_castling[ci][short as usize].is_some() {
                    return Err("conflicting castling files");
                }
                self.possible_rooks_for_castling[ci][short as usize] = Some(field);
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
        tpc.turn = Color::from((tpc.move_index % HB_COUNT as u16) as u8);
        for c in Color::iter() {
            tpc.check_possibilities[usize::from(*c)] =
                CheckPossibilities::from_king_pos(tpc.king_positions[usize::from(*c)])
        }
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
impl std::convert::From<PieceType> for u8 {
    fn from(v: PieceType) -> u8 {
        ToPrimitive::to_u8(&v).unwrap()
    }
}
impl std::convert::TryFrom<u8> for PieceType {
    type Error = ();
    fn try_from(v: u8) -> Result<PieceType, ()> {
        PieceType::from_u8(v).ok_or(())
    }
}
impl std::convert::From<PieceType> for usize {
    fn from(v: PieceType) -> usize {
        ToPrimitive::to_usize(&v).unwrap()
    }
}
impl std::fmt::Display for PieceType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_ascii() as char)
    }
}

impl FieldLocation {
    pub fn from_checked(v: u8) -> Result<FieldLocation, ()> {
        if v >= BOARD_SIZE as u8 {
            return Err(());
        }
        Ok(FieldLocation(std::num::NonZeroU8::new(v + 1).unwrap()))
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
    pub fn file_char(&self) -> u8 {
        <[u8; 2]>::from(*self)[0]
    }
    pub fn file_char_fancy(&self) -> u8 {
        self.file_char().to_ascii_lowercase()
    }
    pub fn rank_char(&self) -> u8 {
        <[u8; 2]>::from(*self)[1]
    }
    pub fn rank_char_fancy(&self) -> ArrayString<2> {
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
    pub fn to_str(&self) -> &'static str {
        unsafe {
            return std::str::from_utf8_unchecked(&BOARD_NOTATION[usize::from(*self)]);
        }
    }
    pub fn to_str_fancy(&self) -> ArrayString<3> {
        let mut res = ArrayString::new();
        res.push(self.file_char_fancy() as char);
        res.push_str(&self.rank_char_fancy().as_str());
        res
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
                ToPrimitive::to_u8(&piece_type).unwrap() << 2
                    | (ToPrimitive::to_u8(&color).unwrap() + 1)
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
                Color::from_u8((v & 0x3) - 1).unwrap(),
                PieceType::from_u8(v >> 2).unwrap(),
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
            Color::from_u8(v & 0x3 - 1).ok_or(())?;
            PieceType::from_u8(v >> 2).ok_or(())?;
            Ok(PackedFieldValue(v))
        }
    }
}

impl Move {
    pub fn from_str(game: &mut ThreePlayerChess, string: &str) -> Option<Move> {
        if string == "O-O" {
            return game.gen_move_castling(true);
        }
        if string == "O-O-O" {
            return game.gen_move_castling(false);
        }
        if string == "draw" {
            return Some(Move {
                source: Default::default(),
                target: Default::default(),
                move_type: MoveType::ClaimDraw,
            });
        }

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

        if string.len() > 4 {
            let promotion: [u8; 2] = string[4..].as_bytes().try_into().ok()?;
            if promotion[0] != '='.try_into().unwrap() {
                return None;
            }
            let piece_type = PieceType::from_ascii(promotion[1])?;
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
                move_type: MoveType::EnPassant(
                    game.board[usize::from(ep_square.loc)],
                    ep_square.loc,
                ),
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
    pub fn get_source_string(&self, game: &mut ThreePlayerChess) -> ArrayString<4> {
        let field_val = game.get_field_value(self.source);
        let (_, piece) = field_val.unwrap();
        let mut mov = *self;
        let mut cp = CheckPossibilities::new();
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
            cp.add_knight_moves(self.target.into());
            for nm in &cp.knight_moves {
                check_move_ambiguities(*nm);
            }
        }
        if piece == Bishop || piece == Queen {
            cp.add_diagonal_directions(self.target.into());
            for i in 0..5 {
                let begin = if i > 0 {
                    cp.diagonal_line_ends[i - 1]
                } else {
                    0
                };
                for j in begin..cp.diagonal_line_ends[i] {
                    check_move_ambiguities(cp.diagonal_lines[j]);
                }
            }
        }
        if piece == Rook || piece == Queen {
            cp.add_cardinal_directions(self.target.into());
            for i in 0..ROW_SIZE {
                check_move_ambiguities(cp.file[i]);
                check_move_ambiguities(cp.rank[i]);
            }
        }
        let mut res = ArrayString::new();
        if piece == Pawn {
            if let MoveType::Capture(_) = self.move_type {
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
            MoveType::Castle(_) => {
                if AnnotatedFieldLocation::from(self.source).file
                    < AnnotatedFieldLocation::from(self.target).file
                {
                    writer.write_str("O-O")?
                } else {
                    writer.write_str("O-O-O")?
                }
            }
            MoveType::Slide => writer.write_fmt(format_args!(
                "{}{}",
                self.get_source_string(game),
                self.target.to_str_fancy().as_str()
            ))?,
            MoveType::Capture(_) => writer.write_fmt(format_args!(
                "{}x{}",
                self.get_source_string(game),
                self.target.to_str_fancy().as_str()
            ))?,
            MoveType::EnPassant(_, _) => writer.write_fmt(format_args!(
                "{}x{} e.p.",
                self.get_source_string(game),
                self.target.to_str_fancy().as_str()
            ))?,
            MoveType::Promotion(piece_type) => writer.write_fmt(format_args!(
                "{}={}",
                self.get_source_string(game),
                piece_type.to_ascii() as char,
            ))?,
            MoveType::CapturePromotion(_, piece_type) => writer.write_fmt(format_args!(
                "{}x{}={}",
                self.get_source_string(game),
                self.target.to_str_fancy().as_str(),
                piece_type.to_ascii() as char,
            ))?,
            MoveType::ClaimDraw => writer.write_str("draw")?,
        }
        let turn = game.turn;
        game.make_move(*self);
        game.turn = get_next_hb(game.turn, true);
        if game.is_king_in_checkmate() {
            if game.is_king_capturable(Some(turn)) {
                writer.write_char('#')?;
            } else {
                writer.write_str("(#)")?;
            }
        } else if game.is_king_capturable(None) {
            if game.is_king_capturable(Some(turn)) {
                writer.write_char('+')?;
            } else {
                writer.write_str("(+)")?;
            }
        } else {
            game.turn = get_next_hb(game.turn, true);
            if game.is_king_capturable(Some(turn)) {
                writer.write_char('+')?;
            }
        }
        game.turn = turn;
        game.undo_move(*self);
        Ok(())
    }
    // 11 charactesr, for moves like 'exf10 e.p.(#)'
    pub fn to_string(&self, game: &mut ThreePlayerChess) -> ArrayString<11> {
        let mut res = ArrayString::new();
        self.write_as_str(game, &mut res).unwrap();
        res
    }
}
