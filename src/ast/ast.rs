use std::{
    collections::VecDeque,
    fmt::{Display, Formatter},
};

use crate::cons;
use crate::tokenizer::{SourceLoc, Token};

#[derive(Debug)]
pub enum Error {
    UnclosedParentheses,
    UnopenedParentheses,
    UnexpectedToken,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnclosedParentheses => write!(f, "Unclosed parentheses"),
            Error::UnopenedParentheses => write!(f, "Unopened parentheses"),
            Error::UnexpectedToken => write!(f, "Unexpected token"),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Atom(String),
    StringLiteral(String),
    Integer(i64),
    Float(f64),
    Nil,
    Bool(bool),
    Cons(Box<Value>, Box<Value>),
    Lambda,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Atom(v) => write!(f, "{}", v),
            Value::StringLiteral(v) => write!(f, "{}", v),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "NIL"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Cons(car, cdr) => write!(f, "({}, {})", car, cdr),
            Value::Lambda => write!(f, "λ"),
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

impl From<isize> for Value {
    fn from(value: isize) -> Self {
        Value::Integer(value as i64)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Integer(value as i64)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Atom(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Atom(value.to_string())
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Nil
    }
}

struct AST {}

impl AST {
    pub fn new() -> Self {
        Self {}
    }

    pub fn build(tokens: Vec<Token>) -> Result<Value, Error> {
        let mut iter = tokens.iter();
        let mut parens_stack = Vec::new();
        let r = Self::parse_group_expression(&mut iter, &mut parens_stack);

        if parens_stack.is_empty() {
            return r;
        }

        Err(Error::UnclosedParentheses)
    }

    pub fn parse_group_expression<'a>(
        iter: &mut impl Iterator<Item = &'a Token>,
        parens_stack: &mut Vec<SourceLoc>,
    ) -> Result<Value, Error> {
        let mut stack = Vec::new();
        while let Some(token) = iter.next() {
            match token {
                Token::LeftParen(loc) => {
                    parens_stack.push(loc.clone());
                    let value = Self::parse_group_expression(iter, parens_stack)?;
                    stack.push(value);
                }
                Token::RightParen(_) => {
                    parens_stack.pop();

                    let mut cons = Value::Nil;
                    for v in stack.iter().rev() {
                        cons = cons!(v.clone(), cons);
                    }

                    stack.clear();
                    stack.push(cons);
                }
                Token::Atom(_, atom) => {
                    let v = match atom.as_str() {
                        "t" => Value::Bool(true),
                        "f" => Value::Bool(false),
                        "lambda" => Value::Lambda,
                        _ => Value::Atom(atom.to_string()),
                    };

                    stack.push(v);
                }
                Token::Int(_, value) => stack.push(Value::from(*value)),
                Token::Float(_, value) => stack.push(Value::from(*value)),
                Token::StringLiteral(_, value) => {
                    stack.push(Value::StringLiteral(value.to_string()))
                }
                Token::EOF => break,
            }
        }

        let mut cons = Value::Nil;

        if stack.len() == 1 {
            return Ok(stack.pop().unwrap());
        }

        for v in stack.into_iter().rev() {
            cons = cons!(v, cons);
        }

        Ok(cons)
    }
}

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr) => {
        Value::Cons(Box::new($car.into()), Box::new($cdr.into()))
    };
}

#[macro_export]
macro_rules! s {
    ($s:expr) => {
        Value::StringLiteral($s.to_string())
    };
}

#[macro_export]
macro_rules! v {
    () => {
        Value::Nil
    };

    ($v:expr) => {
        Value::from($v)
    };
}

#[macro_export]
macro_rules! nil {
    () => {
        Value::Nil
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_addition_fn() {
        let tokens = tokenize("(+ 1 2)").unwrap();
        let tree = AST::build(tokens).unwrap();
        assert_eq!(tree, cons!("+", cons!(1, cons!(2, nil![]))));
    }

    #[test]
    fn test_call_fn() {
        let tokens = tokenize("(say \"hello\")").unwrap();
        let tree = AST::build(tokens).unwrap();
        assert_eq!(tree, cons!("say", cons!(s!("hello"), nil![])));
    }

    #[test]
    fn test_multiple_sexpr() {
        let i = r#"
(+ 1 3)
(say "hello")
"#;
        let tokens = tokenize(i).unwrap();
        let tree = AST::build(tokens).unwrap();

        assert_eq!(
            tree,
            cons!(
                cons!("+", cons!(1, cons!(3, nil![]))),
                cons!(cons!("say", cons!(s!("hello"), nil![])), nil![])
            )
        );
    }

    #[test]
    #[should_panic]
    fn test_unclosed_parentheses() {
        let tokens = tokenize("(+ 1 2").unwrap();
        let tree = AST::build(tokens).unwrap();
    }
}
