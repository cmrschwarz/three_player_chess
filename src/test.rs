use crate::{board::*, movegen::*};

#[test]
pub fn print_board() {
    let mut tpc = ThreePlayerChess::new();
    tpc.board[usize::from(FieldLocation::from_str("A3").unwrap())] =
        FieldValue(Some((Color::C1, PieceType::Rook))).into();
    println!("{}", tpc);
}

#[test]
pub fn gen_moves() {
    let mut tpc = ThreePlayerChess::new();
    tpc.board[usize::from(FieldLocation::from_str("I9").unwrap())] =
        FieldValue(Some((Color::C1, PieceType::Rook))).into();

    let moves = tpc.gen_moves();
    for m in moves {
        tpc.board[usize::from(m.target)] = FieldValue(Some((Color::C2, PieceType::King))).into();
    }
    println!("{}", tpc);
}
