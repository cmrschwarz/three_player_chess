use crate::board::*;
use crate::wrapper::*;

#[test]
pub fn state_str() {
    let tpc = ThreePlayerChess::default();
    println!("{}", tpc);
    let board_str = tpc.state_string();
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
#[test]
pub fn make_move() {
    let mut tpc = ThreePlayerChess::default();
    let mov = parse_move_string(&mut tpc, "A2A3").unwrap();
    assert!(check_move_valid(&mut tpc, mov));
    tpc.make_move(mov);
    const STR_AFTER_MOVE: &'static str = concat!(
        "BCDEFGH2A3/BG1/CF1/AH1/D1/E1/AH/|",
        "LKJIDCBA7/KB8/JC8/LA8/I8/D8/LA/|",
        "HGFEIJKLb/GKc/FJc/HLc/Ec/Ic/HL/|",
        "0|0"
    );
    assert_eq!(tpc.state_string(), STR_AFTER_MOVE);
}
