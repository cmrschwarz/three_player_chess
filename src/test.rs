use crate::board::*;

#[test]
pub fn print_board() {
    let tpc = ThreePlayerChess::default();
    println!("{}", tpc);
    let mut board_str = String::new();
    tpc.write_state_str(&mut board_str);
    println!("{}", board_str);
    assert_eq!(board_str, START_POSITION_STRING);
}

#[test]
pub fn gen_moves() {
    let mut tpc = ThreePlayerChess::default();

    let moves = tpc.gen_moves();
    for m in moves {
        tpc.board[usize::from(m.target)] = FieldValue(Some((Color::C2, PieceType::King))).into();
    }
    println!("{}", tpc);
}
