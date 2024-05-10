use core::fmt;
use std::fmt::{Display, Formatter};

mod codegen;
mod evaluator;
pub mod parser;

#[derive(Debug)]
pub enum Instruction {
    Char(char),
    Match,
    Jump(usize),
    Split(usize, usize),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Instruction::Char(c) => write!(f, "char {c}"),
            Instruction::Match => write!(f, "match"),
            Instruction::Jump(addr) => write!(f, "jump {addr:>04}"),
            Instruction::Split(addr1, addr2) => write!(f, "split {addr1:>04} {addr2:>04}"),
        }
    }
}
