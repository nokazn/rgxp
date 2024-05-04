use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

use crate::helper::safe_add;

use super::{parser::AST, Instruction};

#[derive(Debug)]
pub enum CodeGenError {
    PCOverflow,
    FailStar,
    FailOr,
    FailQuestion,
}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "CodeGenError: {self:?}")
    }
}

impl Error for CodeGenError {}

#[derive(Debug, Default)]
struct Generator {
    pc: usize,
    instructions: Vec<Instruction>,
}

impl Generator {
    const PROVISIONAL_ADDR: usize = 0;

    // fn increment_pc(&mut self) -> Result<(), CodeGenError> {
    //     safe_add(&mut self.pc, &1, || CodeGenError::PCOverflow)
    // }

    fn push_instruction(&mut self, instruction: Instruction) -> Result<(), Box<CodeGenError>> {
        safe_add(&mut self.pc, &1, || CodeGenError::PCOverflow)?;
        // self.increment_pc()?;
        self.instructions.push(instruction);
        Ok(())
    }

    fn gen_code(&mut self, ast: &AST) -> Result<(), Box<CodeGenError>> {
        self.gen_expression(ast)?;
        self.push_instruction(Instruction::Match)?;
        Ok(())
    }

    fn gen_expression(&mut self, ast: &AST) -> Result<(), Box<CodeGenError>> {
        match ast {
            AST::Char(c) => self.gen_char(*c)?,
            AST::Or(e1, e2) => self.gen_or(e1, e2)?,
            AST::Plus(e) => self.gen_plus(e)?,
            AST::Question(e) => self.gen_question(e)?,
            AST::Seq(v) => self.gen_seq(v)?,
            AST::Star(e) => self.gen_star(e)?,
        };
        Ok(())
    }

    fn gen_char(&mut self, c: char) -> Result<(), Box<CodeGenError>> {
        self.push_instruction(Instruction::Char(c))
    }

    fn gen_or(&mut self, e1: &AST, e2: &AST) -> Result<(), Box<CodeGenError>> {
        let split_addr = self.pc;
        self.push_instruction(Instruction::Split(split_addr + 1, Self::PROVISIONAL_ADDR))?;

        self.gen_expression(e1)?;

        let jump_addr = self.pc;
        self.push_instruction(Instruction::Jump(Self::PROVISIONAL_ADDR))?;

        if let Some(Instruction::Split(_, l)) = self.instructions.get_mut(split_addr) {
            *l = self.pc;
        } else {
            return Err(Box::new(CodeGenError::FailOr));
        }

        self.gen_expression(e2)?;

        if let Some(Instruction::Jump(l)) = self.instructions.get_mut(jump_addr) {
            *l = self.pc;
        } else {
            return Err(Box::new(CodeGenError::FailOr));
        }

        Ok(())
    }

    fn gen_plus(&mut self, e: &AST) -> Result<(), Box<CodeGenError>> {
        let addr = self.pc;
        self.gen_expression(e)?;

        self.push_instruction(Instruction::Split(addr, self.pc + 1))?;
        Ok(())
    }

    fn gen_question(&mut self, e: &AST) -> Result<(), Box<CodeGenError>> {
        let split_addr = self.pc;
        self.push_instruction(Instruction::Split(split_addr + 1, Self::PROVISIONAL_ADDR))?;

        self.gen_expression(e)?;

        if let Some(Instruction::Split(_, l)) = self.instructions.get_mut(split_addr) {
            *l = self.pc;
        } else {
            return Err(Box::new(CodeGenError::FailQuestion));
        }
        Ok(())
    }

    fn gen_seq(&mut self, v: &[AST]) -> Result<(), Box<CodeGenError>> {
        for e in v {
            self.gen_expression(e)?;
        }
        Ok(())
    }

    fn gen_star(&mut self, e: &AST) -> Result<(), Box<CodeGenError>> {
        let split_addr = self.pc;
        self.push_instruction(Instruction::Split(split_addr + 1, Self::PROVISIONAL_ADDR))?;

        self.gen_expression(e)?;

        self.push_instruction(Instruction::Jump(split_addr))?;

        if let Some(Instruction::Split(_, l)) = self.instructions.get_mut(split_addr) {
            *l = self.pc;
        } else {
            return Err(Box::new(CodeGenError::FailStar));
        }
        Ok(())
    }
}

pub fn get_code(ast: &AST) -> Result<Vec<Instruction>, Box<CodeGenError>> {
    let mut generator = Generator::default();
    generator.gen_code(ast)?;
    Ok(generator.instructions)
}
