use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use std::char;
use std::collections::HashMap;
use std::io::Write;

pub const ROW_SIZE: usize = 8; // row == rank
pub const HB_ROW_COUNT: usize = 4; // each halfboard has 4 rows
pub const HB_COUNT: usize = 3; // number of half boards
pub const COLOR_COUNT: u8 = HB_COUNT as u8;
pub const HB_SIZE: usize =  ROW_SIZE * HB_ROW_COUNT;
pub const BOARD_SIZE: usize = HB_SIZE * HB_COUNT; // 96

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive, Debug)]
pub enum PieceType {
    Pawn = 1,
    Knight = 2,
    Bishop = 3,
    Rook = 4,
    Queen = 5,
    King = 6,
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive, Debug)]
pub enum Color { // these are assigned clockwise, with player 1 at the bottom
    C1 = 1,
    C2 = 2,
    C3 = 3,
}

// this is not used for board storage because it's sadly
// two bytes large. it can be converted from / into PackedFieldValue alias u8,
// which is actually used for storage
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FieldValue(pub Option<(Color, PieceType)>);

pub type PackedFieldValue = u8;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FieldLocation(pub std::num::NonZeroU8);

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ThreePlayerChess {
    pub turn: Color,
    pub board: [PackedFieldValue; BOARD_SIZE],
}

impl ThreePlayerChess {
    pub fn new() -> ThreePlayerChess {
        ThreePlayerChess {
            turn: Color::C1,
            board: [FieldValue(Option::None).into(); BOARD_SIZE],
        }
    }
}

const BOARD_STRING: &'static str = include_str!("board.txt");

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
        write!(f, "{}", ToPrimitive::to_i8(self).unwrap())
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
        FieldLocation(std::num::NonZeroU8::new(v+1).unwrap())
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
}
impl std::convert::From<FieldValue> for PackedFieldValue {
    fn from(v: FieldValue) -> PackedFieldValue {
        match v {
            FieldValue(Some((color, piece_type))) => {
                ToPrimitive::to_u8(&color).unwrap() << 3 | ToPrimitive::to_u8(&piece_type).unwrap()
            }
            FieldValue(None) => 0 as PackedFieldValue,
        }
    }
}
impl std::convert::From<PackedFieldValue> for FieldValue {
    fn from(v: PackedFieldValue) -> FieldValue {
        if let Some(color) = Color::from_u8(v >> 3) {
            if let Some(piece_type) = PieceType::from_u8(v & 0x7) {
                return FieldValue(Some((color, piece_type)));
            }
        }
        FieldValue(None)
    }
}
