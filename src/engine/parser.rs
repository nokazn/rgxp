use strum_macros::EnumIter;

use core::fmt;
use std::{error::Error, fmt::Display, mem::take, vec};

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Char(char),
    Plus(Box<AST>),
    Star(Box<AST>),
    Question(Box<AST>),
    Or(Box<AST>, Box<AST>),
    Seq(Vec<AST>),
}

#[derive(EnumIter)]
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

struct StackLayer {
    seq: Vec<AST>,
    seq_or: Vec<AST>,
}

#[derive(Debug, PartialEq)]
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

impl Error for ParseError {}

pub fn parse(expr: &str) -> Result<AST, Box<ParseError>> {
    enum ParseState {
        Char,
        Escape,
    }

    let mut seq = vec![];
    let mut seq_or = vec![];
    let mut stack: Vec<StackLayer> = vec![];
    let mut state = ParseState::Char;

    for (pos, c) in expr.chars().enumerate() {
        match &state {
            ParseState::Char => match Identifier::from(c) {
                Some(Identifier::Plus) => eat_plus_star_question(&mut seq, PSQ::Plus, pos)?,
                Some(Identifier::Star) => eat_plus_star_question(&mut seq, PSQ::Star, pos)?,
                Some(Identifier::Question) => eat_plus_star_question(&mut seq, PSQ::Question, pos)?,
                Some(Identifier::LeftParen) => eat_left_paren(&mut seq, &mut seq_or, &mut stack),
                Some(Identifier::RightParen) => {
                    eat_right_paren(&mut seq, &mut seq_or, &mut stack, pos)?;
                }
                Some(Identifier::Or) => {
                    eat_or(&mut seq, &mut seq_or);
                }
                Some(Identifier::Escape) => {
                    state = ParseState::Escape;
                }
                None => seq.push(AST::Char(c)),
            },
            ParseState::Escape => {
                seq.push(parse_escape(pos, c)?);
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

/// - convert  `+`, `*` or `?` to AST that wraos the previous sequence
/// - return `Err` if no previous sequences in `seq`
fn eat_plus_star_question(seq: &mut Vec<AST>, ast_type: PSQ, pos: usize) -> Result<(), ParseError> {
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

fn eat_left_paren(seq: &mut Vec<AST>, seq_or: &mut Vec<AST>, stack: &mut Vec<StackLayer>) {
    let prev = take(seq);
    let prev_or = take(seq_or);
    stack.push(StackLayer {
        seq: prev,
        seq_or: prev_or,
    });
}

fn eat_right_paren(
    seq: &mut Vec<AST>,
    seq_or: &mut Vec<AST>,
    stack: &mut Vec<StackLayer>,
    pos: usize,
) -> Result<(), Box<ParseError>> {
    if let Some(StackLayer {
        seq: mut prev,
        seq_or: prev_or,
    }) = stack.pop()
    {
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

/// - consume the last sequence and `or` identifier
///   - ie. consume `abc|`
/// - return `Err` if no sequences in `seq`
fn eat_or(seq: &mut Vec<AST>, seq_or: &mut Vec<AST>) {
    if seq.is_empty() {
        // do nothing
    } else {
        let prev = take(seq);
        seq_or.push(AST::Seq(prev));
    }
}

/// escape characters
fn parse_escape(pos: usize, c: char) -> Result<AST, ParseError> {
    match Identifier::from(c) {
        Some(_) => Ok(AST::Char(c)),
        None => Err(ParseError::InvalidEscape(pos, c)),
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
    use strum::IntoEnumIterator;

    use crate::test_each;

    use super::*;

    struct TestFoldOrCase {
        input: Vec<AST>,
        expected: Option<AST>,
    }

    test_each!(
        test_fold_or,
        |case: TestFoldOrCase| {
            let ast = fold_or(case.input);
            assert_eq!(ast, case.expected);
        },
        "1" => TestFoldOrCase {
            input: vec![AST::Char('a'), AST::Char('b')],
            expected: Some(AST::Or(
                Box::new(AST::Char('a')),
                Box::new(AST::Char('b')),
            )),
        },
        "2" => TestFoldOrCase {
            input: vec![AST::Char('a'), AST::Char('b'), AST::Char('c'), AST::Char('d')],
            expected: Some(AST::Or(
                Box::new(AST::Char('a')),
                Box::new(AST::Or(
                    Box::new(AST::Char('b')),
                    Box::new(AST::Or(
                        Box::new(AST::Char('c')),
                        Box::new(AST::Char('d')),
                    )),
                )),
            )),
        },
        "3" => TestFoldOrCase {
            input: vec![AST::Char('a')],
            expected: Some(AST::Char('a')),
        },
        "4" => TestFoldOrCase {
            input: vec![],
            expected: None,
        },
    );

    #[test]
    fn test_parse_escape_valid() {
        for i in Identifier::iter() {
            let i = i.into();
            let ast = parse_escape(0, i);
            assert_eq!(ast, Ok(AST::Char(i)));
        }
    }

    #[test]
    fn test_parse_escape_invalid() {
        let ast = parse_escape(0, 'a');
        assert_eq!(ast, Err(ParseError::InvalidEscape(0, 'a')));
    }

    struct TestParseCase {
        input: &'static str,
        expected: Result<AST, Box<ParseError>>,
    }

    test_each!(
        test_parse,
        |case: TestParseCase| {
            let ast = parse(case.input);
            assert_eq!(ast, case.expected);
        },
        "plus_1" => TestParseCase {
            input: "a+",
            expected: Ok(AST::Seq(vec![
                AST::Plus(Box::new(AST::Char('a')))
            ])),
        },
        "plus_2" => TestParseCase {
            input: "+",
            expected: Err(Box::new(ParseError::NoPrev(0))),
        },
        "plus_3" => TestParseCase {
            input: "abc+",
            expected: Ok(AST::Seq(vec![
                AST::Char('a'),
                AST::Char('b'),
                AST::Plus(Box::new(AST::Char('c')))
            ])),
        },
        "star_1" => TestParseCase {
            input: "a*",
            expected: Ok(AST::Seq(vec![
                AST::Star(Box::new(AST::Char('a')))
            ])),
        },
        "star_2" => TestParseCase {
            input: "abc*",
            expected: Ok(AST::Seq(vec![
                AST::Char('a'),
                AST::Char('b'),
                AST::Star(Box::new(AST::Char('c')))
            ])),
        },
        "star_3" => TestParseCase {
            input: "a**",
            expected: Ok(AST::Seq(vec![
                AST::Star(Box::new(
                    AST::Star(Box::new(AST::Char('a')))
                ))
            ])),
        },
        "question_1" => TestParseCase {
            input: "a?",
            expected: Ok(AST::Seq(vec![
                AST::Question(Box::new(AST::Char('a')))
            ])),
        },
        "question_2" => TestParseCase {
            input: "abc?",
            expected: Ok(AST::Seq(vec![
                AST::Char('a'),
                AST::Char('b'),
                AST::Question(Box::new(AST::Char('c')))
            ])),
        },
        "question_3" => TestParseCase {
            input: "a??",
            expected: Ok(AST::Seq(vec![
                AST::Question(Box::new(
                    AST::Question(Box::new(AST::Char('a')))
                ))
            ])),
        },
        "or_1" => TestParseCase {
            input: "a|b",
            expected: Ok(AST::Or(
                Box::new(AST::Seq(vec![AST::Char('a')])),
                Box::new(AST::Seq(vec![AST::Char('b')])),
            )),
        },
        "or_2" => TestParseCase {
            input: "\\|\\*",
            expected: Ok(AST::Seq(vec![
                AST::Char('|'),
                AST::Char('*'),
            ])),
        },
        "or_3" => TestParseCase {
            input: "\\\\|\\*",
            expected: Ok(AST::Or(
                Box::new(AST::Seq(vec![AST::Char('\\')])),
                Box::new(AST::Seq(vec![AST::Char('*')])),
            )),
        },
        "or_empty_single_1" => TestParseCase {
            input: "a|",
            expected: Ok(AST::Seq(vec![
                AST::Char('a'),
            ])),
        },
        "or_empty_single_2" => TestParseCase {
            input: "|a|",
            expected: Ok(AST::Seq(vec![
                AST::Char('a'),
            ])),
        },
        "or_empty_single_3" => TestParseCase {
            input: "|a",
            expected: Ok(AST::Seq(vec![
                AST::Char('a'),
            ])),
        },
        "or_empty_single_4" => TestParseCase {
            input: "||a|||",
            expected: Ok(AST::Seq(vec![
                AST::Char('a'),
            ])),
        },
        "or_empty_multiple_1" => TestParseCase {
            input: "||a|b||",
            expected: Ok(AST::Or(
                Box::new(AST::Seq(vec![AST::Char('a')])),
                Box::new(AST::Seq(vec![AST::Char('b')])),
            )),
        },
        "or_empty_1" => TestParseCase {
            input: "|",
            expected: Err(Box::new(ParseError::Empty)),
        },
        "or_empty_2" => TestParseCase {
            input: "|||||||||",
            expected: Err(Box::new(ParseError::Empty)),
        },
        "or_empty_3" => TestParseCase {
            input: "\\\\|||||",
            expected: Ok(AST::Seq(vec![
                AST::Char('\\'),
            ])),
        },
        "or_empty_7" => TestParseCase {
            input: "|a|||b|||c|  ||",
            expected: Ok(AST::Or(
                Box::new(AST::Seq(vec![AST::Char('a')])),
                Box::new(AST::Or(
                    Box::new(AST::Seq(vec![AST::Char('b')])),
                    Box::new(AST::Or(
                        Box::new(AST::Seq(vec![AST::Char('c')])),
                        Box::new(AST::Seq(vec![
                            AST::Char(' '),
                            AST::Char(' '),
                        ])),
                    )),
                )),
            )),
        },
        "escape_plus_1" => TestParseCase {
            input: "\\+",
            expected: Ok(AST::Seq(vec![
                AST::Char('+')
            ])),
        },
        "escape_plus_2" => TestParseCase {
            input: "\\\\+",
            expected: Ok(AST::Seq(vec![
                AST::Plus(Box::new(AST::Char('\\'))),
            ])),
        },
        "escape_plus_3" => TestParseCase {
            input: "\\\\\\+",
            expected: Ok(AST::Seq(vec![
                AST::Char('\\'),
                AST::Char('+'),
            ])),
        },
        "escape_star_1" => TestParseCase {
            input: "\\*",
            expected: Ok(AST::Seq(vec![
                AST::Char('*')
            ])),
        },
        "escape_star_2" => TestParseCase {
            input: "\\\\*",
            expected: Ok(AST::Seq(vec![
                AST::Star(Box::new(AST::Char('\\'))),
            ])),
        },
        "escape_star_3" => TestParseCase {
            input: "\\\\\\*",
            expected: Ok(AST::Seq(vec![
                AST::Char('\\'),
                AST::Char('*'),
            ])),
        },
        "escape_question_1" => TestParseCase {
            input: "\\?",
            expected: Ok(AST::Seq(vec![
                AST::Char('?'),
            ])),
        },
        "escape_question_2" => TestParseCase {
            input: "\\\\?",
            expected: Ok(AST::Seq(vec![
                AST::Question(Box::new(AST::Char('\\'))),
            ])),
        },
        "escape_question_3" => TestParseCase {
            input: "\\\\\\?",
            expected: Ok(AST::Seq(vec![
                AST::Char('\\'),
                AST::Char('?'),
            ])),
        },
        "escape_or_1" => TestParseCase {
            input: "\\|",
            expected: Ok(AST::Seq(vec![
                AST::Char('|'),
            ])),
        },
        "escape_or_2" => TestParseCase {
            input: "\\\\|",
            expected: Ok(AST::Seq(vec![
                AST::Char('\\'),
            ])),
        },
        "escape_left_paren_1" => TestParseCase {
            input: "\\(",
            expected: Ok(AST::Seq(vec![
                AST::Char('('),
            ])),
        },
        "escape_right_paren_1" => TestParseCase {
            input: "\\)",
            expected: Ok(AST::Seq(vec![
                AST::Char(')'),
            ])),
        },
        "escape_escape_1" => TestParseCase {
            input: "\\",
            expected: Err(Box::new(ParseError::Empty)),
        },
        "escape_escape_2" => TestParseCase {
            input: "\\\\",
            expected: Ok(AST::Seq(vec![
                AST::Char('\\'),
            ])),
        },
        "escape_escape_3" => TestParseCase {
            input: "\\\\\\",
            expected: Ok(AST::Seq(vec![
                AST::Char('\\'),
            ])),
        },
        "escape_escape_4" => TestParseCase {
            input: "\\\\\\\\",
            expected: Ok(AST::Seq(vec![
                AST::Char('\\'),
                AST::Char('\\'),
            ])),
        },
        "escape_escape_5" => TestParseCase {
            input: "\\a",
            expected: Err(Box::new(ParseError::InvalidEscape(1, 'a'))),
        },
        "paren_1" => TestParseCase {
            input: "()",
            expected: Err(Box::new(ParseError::Empty)),
        },
        "paren_2" => TestParseCase {
            input: "(abc)",
            expected: Ok(
                AST::Seq(vec![
                    AST::Seq(vec![
                        AST::Char('a'),
                        AST::Char('b'),
                        AST::Char('c'),
                    ]),
                ]),
            ),
        },
        "paren_or_1" => TestParseCase {
            input: "(a|b|c)",
            expected: Ok(
                AST::Seq(vec![
                    AST::Or(
                        Box::new(AST::Seq(vec![AST::Char('a')])),
                        Box::new(AST::Or(
                            Box::new(AST::Seq(vec![AST::Char('b')])),
                            Box::new(AST::Seq(vec![AST::Char('c')])),
                        )),
                    ),
                ]),
            ),
        },
        "no_closing_paren_1" => TestParseCase {
            input: "(",
            expected: Err(Box::new(ParseError::NoRightParen)),
        },
        "no_closing_paren_2" => TestParseCase {
            input: "(a|b|c",
            expected: Err(Box::new(ParseError::NoRightParen)),
        },
        "no_opening_paren_1" => TestParseCase {
            input: "a|b|c)",
            expected: Err(Box::new(ParseError::InvalidRightParen(5))),
        },
        "no_opening_paren_2" => TestParseCase {
            input: ")",
            expected: Err(Box::new(ParseError::InvalidRightParen(0))),
        },
    );
}
