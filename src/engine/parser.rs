use core::fmt;
use std::{fmt::Display, mem::take, vec};

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Char(char),
    Plus(Box<AST>),
    Star(Box<AST>),
    Question(Box<AST>),
    Or(Box<AST>, Box<AST>),
    Seq(Vec<AST>),
}

#[repr(u8)]
enum Identifier {
    Plus = b'+',
    Star = b'*',
    Question = b'?',
    Or = b'|',
    LeftParen = b'(',
    RightParen = b')',
    Escape = b'\\',
}

impl Into<char> for Identifier {
    fn into(self) -> char {
        self as u8 as char
    }
}

impl Identifier {
    fn from(c: char) -> Option<Self> {
        match c {
            '+' => Some(Identifier::Plus),
            '*' => Some(Identifier::Star),
            '?' => Some(Identifier::Question),
            '|' => Some(Identifier::Or),
            '(' => Some(Identifier::LeftParen),
            ')' => Some(Identifier::RightParen),
            '\\' => Some(Identifier::Escape),
            _ => None,
        }
    }
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

pub fn parse(expr: &str) -> Result<AST, Box<ParseError>> {
    enum ParseState {
        Char,
        Escape,
    }

    let mut seq = vec![];
    let mut seq_or = vec![];
    let mut stack = vec![];
    let mut state = ParseState::Char;

    for (pos, c) in expr.chars().enumerate() {
        match &state {
            ParseState::Char => match Identifier::from(c) {
                Some(Identifier::Plus) => parse_plus_star_question(&mut seq, PSQ::Plus, pos)?,
                Some(Identifier::Star) => parse_plus_star_question(&mut seq, PSQ::Star, pos)?,
                Some(Identifier::Question) => {
                    parse_plus_star_question(&mut seq, PSQ::Question, pos)?
                }
                Some(Identifier::LeftParen) => {
                    let prev = take(&mut seq);
                    let prev_or = take(&mut seq_or);
                    stack.push((prev, prev_or));
                }
                Some(Identifier::RightParen) => {
                    eat_right_paren(&mut stack, &mut seq, &mut seq_or, pos)?;
                }
                Some(Identifier::Or) => {
                    eat_or(&mut seq, &mut seq_or, pos)?;
                }
                Some(Identifier::Escape) => {
                    state = ParseState::Escape;
                }
                None => seq.push(AST::Char(c)),
            },
            ParseState::Escape => {
                let ast = parse_escape(pos, c)?;
                seq.push(ast);
                state = ParseState::Char;
            }
        };
    }

    if !stack.is_empty() {
        return Err(Box::new(ParseError::NoRightParen));
    }
    // store the current sequence regardless of preceded `or` identifier
    if !seq.is_empty() {
        seq_or.push(AST::Seq(seq));
    }

    fold_or(seq_or).ok_or(Box::new(ParseError::Empty))
}

fn eat_right_paren(
    stack: &mut Vec<(Vec<AST>, Vec<AST>)>,
    seq: &mut Vec<AST>,
    seq_or: &mut Vec<AST>,
    pos: usize,
) -> Result<(), Box<ParseError>> {
    if let Some((mut prev, prev_or)) = stack.pop() {
        // store the current sequence regardless of preceded `or` identifier
        if !seq.is_empty() {
            seq_or.push(AST::Seq(seq.clone()));
        }
        if let Some(ast_or) = fold_or(seq_or.clone()) {
            prev.push(ast_or);
        }
        *seq = prev;
        *seq_or = prev_or;
        Ok(())
    } else {
        return Err(Box::new(ParseError::InvalidRightParen(pos)));
    }
}

/// consume the last sequence and `or` identifier
/// ie. consume `abc|`
fn eat_or(seq: &mut Vec<AST>, seq_or: &mut Vec<AST>, pos: usize) -> Result<(), Box<ParseError>> {
    if seq.is_empty() {
        return Err(Box::new(ParseError::NoPrev(pos)));
    }
    let prev = take(seq);
    seq_or.push(AST::Seq(prev));
    Ok(())
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
    seq: &mut Vec<AST>,
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
    fn test_fold_or_1() {
        let ast = fold_or(vec![AST::Char('a'), AST::Char('b'), AST::Char('c')]);
        assert_eq!(
            ast,
            Some(AST::Or(
                Box::new(AST::Char('a')),
                Box::new(AST::Or(Box::new(AST::Char('b')), Box::new(AST::Char('c')),)),
            ))
        );
    }
    #[test]
    fn test_fold_or_2() {
        let ast = fold_or(vec![AST::Char('a')]);
        assert_eq!(ast, Some(AST::Char('a')));
    }
    #[test]
    fn test_fold_or_3() {
        let ast = fold_or(vec![]);
        assert_eq!(ast, None);
    }
}
