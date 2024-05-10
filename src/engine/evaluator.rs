use std::{
    collections::VecDeque,
    fmt::{self, Display, Formatter},
};

use crate::helper::safe_add;

use super::Instruction;

#[derive(Debug)]
pub enum EvalError {
    PCOverFlow,
    SPOverFlow,
    InvalidPC,
    InvalidContext,
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "CodeGenError: {self:?}")
    }
}

pub fn eval(
    instructions: &[Instruction],
    line: &[char],
    is_depth: bool,
) -> Result<bool, Box<EvalError>> {
    if is_depth {
        eval_depth(instructions, line, 0, 0)
    } else {
        eval_width(instructions, line)
    }
}

fn eval_depth(
    instructions: &[Instruction],
    line: &[char],
    mut pc: usize,
    mut sp: usize,
) -> Result<bool, Box<EvalError>> {
    loop {
        let Some(next) = instructions.get(pc) else {
            return Err(Box::new(EvalError::InvalidPC));
        };
        match next {
            Instruction::Char(c) => {
                if line.get(sp).map_or(false, |sp_c| c == sp_c) {
                    safe_add(&mut pc, &1, || Box::new(EvalError::PCOverFlow))?;
                    safe_add(&mut sp, &1, || Box::new(EvalError::SPOverFlow))?;
                } else {
                    return Ok(false);
                }
            }
            Instruction::Match => {
                return Ok(true);
            }
            Instruction::Jump(addr) => {
                pc = *addr;
            }
            Instruction::Split(addr1, addr2) => {
                return Ok(eval_depth(instructions, line, *addr1, sp)?
                    || eval_depth(instructions, line, *addr2, sp)?);
            }
        }
    }
}

fn pop_context(
    pc: &mut usize,
    sp: &mut usize,
    context: &mut VecDeque<(usize, usize)>,
) -> Result<(), Box<EvalError>> {
    let Some((p, s)) = context.pop_back() else {
        return Err(Box::new(EvalError::InvalidContext));
    };
    *pc = p;
    *sp = s;
    Ok(())
}

fn eval_width(instructions: &[Instruction], line: &[char]) -> Result<bool, Box<EvalError>> {
    let mut context: VecDeque<(usize, usize)> = VecDeque::new();
    let mut pc = 0;
    let mut sp = 0;
    loop {
        let Some(next) = instructions.get(pc) else {
            return Err(Box::new(EvalError::InvalidPC));
        };
        match next {
            Instruction::Char(c) => {
                if line.get(sp).map_or(false, |sp_c| c == sp_c) {
                    safe_add(&mut pc, &1, || Box::new(EvalError::PCOverFlow))?;
                    safe_add(&mut sp, &1, || Box::new(EvalError::SPOverFlow))?;
                } else {
                    if context.is_empty() {
                        return Ok(false);
                    } else {
                        pop_context(&mut pc, &mut sp, &mut context)?;
                    }
                }
            }
            Instruction::Match => {
                return Ok(true);
            }
            Instruction::Jump(addr) => {
                pc = *addr;
            }
            Instruction::Split(addr1, addr2) => {
                pc = *addr1;
                context.push_back((*addr2, sp));
                continue;
            }
        }
        if !context.is_empty() {
            context.push_back((pc, sp));
            pop_context(&mut pc, &mut sp, &mut context)?;
        }
    }
}
