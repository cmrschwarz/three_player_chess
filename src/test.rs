use crate::board::*;

#[test]
pub fn print_board() {
    let mut tpc = ThreePlayerChess::new();
    tpc.board[usize::from(FieldLocation::from_str("A3").unwrap())] =
        FieldValue(Some((Color::C1, PieceType::Rook))).into();
    println!("{}", tpc);
}
