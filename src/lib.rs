#[macro_use]
extern crate lazy_static;

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

use std::char;
use std::collections::HashMap;
use std::io::Write;

const ROW_SIZE: usize = 8; // in standard chess rows would be called ranks
const HB_ROW_COUNT: usize = 4; // each halfboard has 4 rows
const HB_COUNT: usize = 3; // number of half boards
const BOARD_SIZE: usize = ROW_SIZE * HB_ROW_COUNT * HB_COUNT; // 96

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive)]
enum PieceType {
    Pawn = 1,
    Knight = 2,
    Bishop = 3,
    Rook = 4,
    Queen = 5,
    King = 6,
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive)]
enum Color {
    C1 = 1,
    C2 = 2,
    C3 = 3,
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct FieldValue(Option<(Color, PieceType)>);

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
struct FieldLocation(u8);

#[derive(Copy, Clone, PartialEq, Eq)]
struct ThreePlayerChess {
    turn: Color,
    board: [FieldValue; BOARD_SIZE],
}

impl ThreePlayerChess {
    pub fn new() -> ThreePlayerChess {
        ThreePlayerChess {
            turn: Color::C1,
            board: [FieldValue(None); BOARD_SIZE],
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
            nm.insert(BOARD_NOTATION[i], FieldLocation(i as u8));
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
            write!(&mut board_str[loc..loc + 2], "{}", value).unwrap();
        }
        write!(f, "{}", String::from_utf8_lossy(&board_str))
    }
}
impl std::fmt::Display for FieldLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            String::from_utf8_lossy(&BOARD_NOTATION[self.to_usize()])
        )
    }
}

impl FieldLocation {
    fn to_usize(&self) -> usize {
        self.0 as usize
    }
    fn to_u8(&self) -> u8 {
        self.0
    }
    fn to_notation(&self) -> [u8; 2] {
        return BOARD_NOTATION[self.to_usize()];
    }
    fn to_notation_str(&self) -> String {
        String::from_utf8_lossy(&BOARD_NOTATION[self.to_usize()])
            .to_owned()
            .to_string()
    }
    fn from_str(field_name: &str) -> Option<Self> {
        let board_str: [u8; 2] = field_name.as_bytes()[0..2].try_into().unwrap();
        BOARD_NOTATION_MAP
            .get(&board_str)
            .map(|field_not| *field_not)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn print_board() {
        let mut tpc = ThreePlayerChess::new();
        tpc.board[FieldLocation::from_str("A3").unwrap().to_usize()] =
            FieldValue(Some((Color::C1, PieceType::Rook)));
        println!("{}", tpc);
    }
}
