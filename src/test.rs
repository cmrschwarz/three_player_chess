use crate::{board::*, movegen::*};

#[test]
pub fn print_board() {
    let tpc = ThreePlayerChess::default();
    println!("{}", tpc);
    let mut board_str_buffer = [0u8; MAX_POSITION_STRING_SIZE];
    let board_str = tpc.to_string(&mut board_str_buffer);
    println!("{}", board_str);
    assert_eq!(board_str, START_POSITION_STRING);
}

#[test]
pub fn gen_moves() {
    let mut tpc = ThreePlayerChess::new();
    tpc.board[usize::from(FieldLocation::from_str("I9").unwrap())] =
        FieldValue(Some((Color::C1, PieceType::Pawn))).into();
    tpc.board[usize::from(FieldLocation::from_str("Ea").unwrap())] =
        FieldValue(Some((Color::C2, PieceType::Pawn))).into();

    let moves = tpc.gen_moves();
    for m in moves {
        tpc.board[usize::from(m.target)] = FieldValue(Some((Color::C2, PieceType::King))).into();
    }
    println!("{}", tpc);
}
