use core::fmt;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Char(char),
    Plus(Box<AST>),
    Star(Box<AST>),
    Question(Box<AST>),
    Or(Box<AST>, Box<AST>),
    Seq(Vec<AST>),
}

enum PSQ {
    Plus,
    Star,
    Question,
}

#[derive(Debug)]
pub enum ParseError {
    InvalidEscape(usize, char),
    InvalidRightParen(usize),
    NoPrev(usize),
    NoRightParen,
    Empty,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const PREFIX: &str = "ParseError: ";
        match self {
            ParseError::InvalidEscape(pos, c) => {
                write!(f, "{PREFIX}invalid escape: pos = {pos}, char = {c}")
            }
            ParseError::InvalidRightParen(pos) => {
                write!(f, "{PREFIX}invalid right parenthesis: pos = {pos}")
            }
            ParseError::NoPrev(pos) => {
                write!(f, "{PREFIX}no previous expressions: pos = {pos}")
            }
            ParseError::NoRightParen => {
                write!(f, "{PREFIX}no right parenthesis")
            }
            ParseError::Empty => {
                write!(f, "{PREFIX}empty expression")
            }
        }
    }
}

pub fn parse(expr: &str) -> Result<AST, ParseError> {
    unimplemented!("TODO: implement parse");
}

/// escape characters
fn parse_escape(pos: usize, c: char) -> Result<AST, ParseError> {
    match c {
        '\\' | '(' | ')' | '|' | '+' | '*' | '?' => Ok(AST::Char(c)),
        _ => {
            let error = ParseError::InvalidEscape(pos, c);
            Err(error)
        }
    }
}

/// convert  `+`, `*` or `?` to AST
fn parse_plus_star_question(
    mut seq: Vec<AST>,
    ast_type: PSQ,
    pos: usize,
) -> Result<(), ParseError> {
    if let Some(prev) = seq.last_mut() {
        let ast = match ast_type {
            PSQ::Plus => AST::Plus(Box::new(prev.clone())),
            PSQ::Star => AST::Star(Box::new(prev.clone())),
            PSQ::Question => AST::Question(Box::new(prev.clone())),
        };
        *prev = ast;
        Ok(())
    } else {
        Err(ParseError::NoPrev(pos))
    }
}

/// convert multiple concatenate expressions to a single AST
///
/// ### Example
/// ```rust
/// let ast = fold_or(vec![AST::Char('a'), AST::Char('b'), AST::Char('c')]);
/// assert_eq!(
///     ast.unwrap(),
///     AST::Or(
///         Box::new(AST::Char('a')),
///         Box::new(AST::Or(
///             Box::new(AST::Char('b')),
///             Box::new(AST::Char('c')),
///         )),
///     )
/// );
/// ```
fn fold_or(mut seq_or: Vec<AST>) -> Option<AST> {
    if seq_or.len() > 1 {
        seq_or
            .into_iter()
            .rev()
            .reduce(|sum, s| AST::Or(Box::new(s), Box::new(sum)))
    } else {
        seq_or.pop()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fold_or() {
        let ast = fold_or(vec![AST::Char('a'), AST::Char('b'), AST::Char('c')]);
        assert_eq!(
            ast,
            Some(AST::Or(
                Box::new(AST::Char('a')),
                Box::new(AST::Or(Box::new(AST::Char('b')), Box::new(AST::Char('c')),)),
            ))
        );
    }
}
