use crate::board::*;

pub struct Move(pub FieldLocation, pub FieldLocation);

impl ThreePlayerChess {
    pub fn gen_moves(&self) -> Vec<Move> {
        Vec::new()
    }
}
