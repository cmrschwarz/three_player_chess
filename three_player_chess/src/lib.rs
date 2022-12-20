#[macro_use]
extern crate lazy_static;

pub mod board;
pub mod movegen;
pub mod zobrist;

#[cfg(test)]
mod test;
