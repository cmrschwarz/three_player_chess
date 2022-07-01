use crate::board::*;
use crate::game_wrapper::*;
use crate::movegen::*;

#[test]
pub fn state_str() {
    let tpc = ThreePlayerChess::default();
    println!("{}", tpc);
    let board_str = tpc.state_string();
    println!("{}", board_str);
    assert_eq!(board_str, START_POSITION_STRING);
}

#[test]
pub fn en_passent() {
    let mut tpc = ThreePlayerChess::from_str(
        "ABCDFGH2E4/BG1/CF1/AH1/H9/E1/AH/:A4LKJIDCB7/KB8/JC8/LA8/I8/D8/LA/:HGFEILbJaK9/GKc/FJc/HLc/Ec/Ic/HL/Ka:6:6"
    ).unwrap();

    let moves = tpc.gen_moves();
    for m in moves {
        tpc.board[usize::from(m.target)] = FieldValue(Some((Color::C2, PieceType::King))).into();
    }
    println!("{}", tpc);
}

#[test]
pub fn checkmate() {
    let mut tpc = ThreePlayerChess::from_str(
        "BCDEFGH2A5/BG1/CF1/AH1/D1/E1/AH/:LKIDCBA7J6/KB8/JC8/LA8/L5/D8/LA/:HGFEILbJaK9/GKc/FJc/HLc/Ec/Ic/HL/Ka:7:7"
    ).unwrap();
    let mov = Move::from_str(&mut tpc, "L5L9").unwrap();
    assert!(tpc.is_valid_move(mov));
    tpc.make_move(mov);
    tpc.apply_move_sideeffects(mov);
    assert!(tpc.game_status == GameStatus::Win(Color::C1, WinReason::Checkmate(Color::C2)));
}
#[test]
pub fn make_move() {
    let mut tpc = ThreePlayerChess::default();
    let mov = Move::from_str(&mut tpc, "E2E4").unwrap();
    let movcode = u64::from(mov);
    let mov_reenc = Move::try_from(movcode).unwrap();
    assert_eq!(mov, mov_reenc);
    assert!(tpc.is_valid_move(mov_reenc));
    tpc.make_move(mov);
    const STR_AFTER_MOVE: &'static str = concat!(
        "ABCDFGH2E4/BG1/CF1/AH1/D1/E1/AH/:",
        "LKJIDCBA7/KB8/JC8/LA8/I8/D8/LA/:",
        "HGFEIJKLb/GKc/FJc/HLc/Ec/Ic/HL/:",
        "0:0"
    );
    assert_eq!(tpc.state_string(), STR_AFTER_MOVE);
}
