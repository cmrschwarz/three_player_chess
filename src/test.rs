use crate::board::*;

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
    let mut tpc = ThreePlayerChess::default();

    let moves = tpc.gen_moves();
    for m in moves {
        tpc.board[usize::from(m.target)] = FieldValue(Some((Color::C2, PieceType::King))).into();
    }
    println!("{}", tpc);
}
