const BOARD_SIZE: usize = 96;
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
enum PieceType {
    Pawn = 0,
    Knight = 1,
    Bishop = 2,
    Rook = 3,
    Queen = 4,
    King = 5,
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct ThreePlayerChess {
    turn: u8, // either 1, 2 or 3
    board: [PieceType; BOARD_SIZE],
}

impl ThreePlayerChess {}

impl std::fmt::Debug for ThreePlayerChess {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ThreePlayerChess");
    }
}
