use std::{fmt::Display, iter::Peekable, str::Chars};

use self::source::Source;

pub mod source;

#[derive(Debug)]
pub enum Error {
    StringLiteralNotClosed(SourceLoc),
    InvalidFloat(SourceLoc, String),
    InvalidInt(SourceLoc, String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::StringLiteralNotClosed(_) => write!(f, "string literal not closed"),
            Error::InvalidFloat(_, n) => {
                write!(f, "parsing error, cannot parse {} into a float", n)
            }
            Error::InvalidInt(_, n) => {
                write!(f, "parsing error, cannot parse {} into an integer", n)
            }
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub struct AggregatedError<T> {
    errors: Vec<T>,
}

impl<T: std::error::Error + Display> Display for AggregatedError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Aggregated error:\n")?;

        for error in &self.errors {
            write!(f, "\t{}\n", error)?;
        }

        Ok(())
    }
}

impl<T: std::error::Error> std::error::Error for AggregatedError<T> {}

impl<T: Display + std::fmt::Debug + std::error::Error> AggregatedError<T> {
    fn new(errors: Vec<T>) -> Self {
        Self { errors }
    }
}

/// Token types
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    StringLiteral(SourceLoc, String),
    LeftParen(SourceLoc),
    RightParen(SourceLoc),
    Int(SourceLoc, i64),
    Float(SourceLoc, f64),
    Atom(SourceLoc, String),
    EOF,
}

impl Token {
    pub fn len(&self) -> usize {
        match self {
            Self::StringLiteral(_, s) => s.len(),
            Self::LeftParen(_) => 1,
            Self::RightParen(_) => 1,
            Self::Int(_, i) => i.to_string().len(),
            Self::Float(_, n) => n.to_string().len(),
            Self::Atom(_, s) => s.len(),
            Self::EOF => 0,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StringLiteral(_, s) => write!(f, "\"{}\"", s),
            Self::LeftParen(_) => write!(f, "("),
            Self::RightParen(_) => write!(f, ")"),
            Self::Int(_, i) => write!(f, "{}", i),
            Self::Float(_, n) => write!(f, "{}", n),
            Self::Atom(_, s) => write!(f, "{}", s),
            Self::EOF => write!(f, "EOF"),
        }
    }
}

/// SourceLoc is a struct that represents the location of a token in the original source code.
#[derive(Debug, PartialEq, Clone)]
pub struct SourceLoc {
    pub line: u32,
    pub column: u32,
    pub origin: String,
}

impl Display for SourceLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{},{}", self.origin, self.line, self.column)
    }
}

type Tokens = Vec<Token>;

/// Tokenizer implements a simple tokenizer that takes the lisp source code and return a list of tokens.
pub struct Tokenizer<'a> {
    source: &'a Source,
}

impl<'a> Tokenizer<'a> {
    /// Creates a new Tokenizer instance.
    pub fn new(source: &'a Source) -> Self {
        Self { source }
    }

    fn parse_iter(&self) -> TokenizerIter {
        TokenizerIter::new(
            self.source.content().chars().peekable(),
            self.source.origin(),
        )
    }

    pub fn parse(&self) -> Result<Tokens, AggregatedError<Error>> {
        let (tokens, errors): (Vec<_>, Vec<_>) = self.parse_iter().partition(Result::is_ok);

        if errors.is_empty() {
            Ok(tokens.into_iter().map(Result::unwrap).collect())
        } else {
            Err(AggregatedError::new(
                errors.into_iter().map(Result::unwrap_err).collect(),
            ))
        }
    }
}

struct TokenizerIter<'a> {
    iter: Peekable<Chars<'a>>,
    origin: &'a str,
    lines: u32,
    columns: u32,
}

impl<'a> TokenizerIter<'a> {
    fn new(iter: Peekable<Chars<'a>>, origin: &'a str) -> Self {
        Self {
            iter,
            origin,
            lines: 0,
            columns: 0,
        }
    }

    // TODO: add source loc
    fn parse_expression(&mut self) -> Result<Token, Error> {
        while let Some(c) = self.iter.peek() {
            match c {
                '(' => {
                    self.iter.next();
                    let t = Token::LeftParen(self.loc());
                    self.advance_column(t.len() as u32);
                    return Ok(t);
                }
                ')' => {
                    self.iter.next();
                    let t = Token::RightParen(self.loc());
                    self.advance_column(t.len() as u32);
                    return Ok(t);
                }
                '\n' => {
                    self.new_line();
                    self.iter.next();
                }
                ' ' => {
                    self.iter.next();
                    self.advance_column(1);
                }
                '"' => return self.consume_string_literal(),
                '0'..='9' => return self.consume_number(),
                _ => return self.consume_atom(),
            }
        }

        Ok(Token::EOF)
    }

    fn consume_atom(&mut self) -> Result<Token, Error> {
        let s = std::iter::from_fn(|| {
            self.iter.next_if(|c| {
                // NOTE: Everything is a possible atom, except a whitespace.
                // we might want to restrict this in the future.
                if c.is_whitespace() {
                    return false;
                }

                match c {
                    '(' | ')' => false,
                    _ => true,
                }
            })
        })
        .collect::<String>();

        let loc = self.loc();
        self.advance_column(s.len() as u32);
        Ok(Token::Atom(loc, s))
    }

    fn consume_string_literal(&mut self) -> Result<Token, Error> {
        // consume the opening quote
        self.iter.next();

        let mut s = String::new();
        let mut escaped = false;
        let loc = self.loc();

        while let Some(c) = self.iter.next() {
            match c {
                '"' if !escaped => {
                    let t = Token::StringLiteral(loc, s);
                    self.advance_column((t.len() + 2) as u32);
                    return Ok(t);
                }
                '"' if escaped => {
                    escaped = false;
                    s.push(c);
                }
                '\\' if !escaped => {
                    escaped = true;
                    // We still need to advance the column even if we are escaping the character,
                    // so the error reporting fit the actual position in the source code.
                    self.advance_column(1);
                }
                '\\' if escaped => {
                    escaped = false;
                    s.push(c);
                }
                _ => s.push(c),
            }
        }

        Err(Error::StringLiteralNotClosed(loc))
    }

    fn consume_number(&mut self) -> Result<Token, Error> {
        let loc = self.loc();

        let n = std::iter::from_fn(|| self.iter.next_if(|c| c.is_digit(10) || *c == '.'))
            .collect::<String>();

        self.advance_column(n.len() as u32);

        if n.contains('.') {
            Ok(Token::Float(
                loc.clone(),
                n.parse().map_err(|_| Error::InvalidFloat(loc, n))?,
            ))
        } else {
            Ok(Token::Int(
                loc.clone(),
                n.parse().map_err(|_| Error::InvalidInt(loc, n))?,
            ))
        }
    }

    fn new_line(&mut self) {
        self.lines += 1;
        self.columns = 0;
    }

    fn advance_column(&mut self, n: u32) {
        self.columns += n;
    }

    fn loc(&self) -> SourceLoc {
        SourceLoc {
            line: self.lines,
            column: self.columns,
            origin: self.origin.to_string(), // TODO: remove to_string so we do not allocate a string for each source-location.
        }
    }
}

impl Iterator for TokenizerIter<'_> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.parse_expression() {
            Ok(Token::EOF) => None,
            t => Some(t),
        }
    }
}

/// Tokenize a string
pub fn tokenize(src: impl Into<String>) -> Result<Tokens, AggregatedError<Error>> {
    let mut source = Source::from(src.into());
    tracing::info!("tokenize!");
    Tokenizer::new(&mut source).parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn loc() -> SourceLoc {
        SourceLoc {
            line: 0,
            column: 0,
            origin: String::from("__inline__"),
        }
    }

    #[test]
    fn test_tokenize_atom() {
        let tokens = tokenize("hello").unwrap();
        assert_eq!(tokens, vec![Token::Atom(loc(), String::from("hello"))]);
    }

    #[test]
    fn test_single_character() {
        let tokens = tokenize("+").unwrap();
        assert_eq!(tokens, vec![Token::Atom(loc(), String::from("+"))]);
    }

    #[test]
    fn test_closed_string_literal() {
        let tokens = tokenize("\"hello world\"").unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral(loc(), String::from("hello world"))]
        );
    }

    #[test]
    fn test_closed_string_literal_with_escaped_quote() {
        let tokens = tokenize("\"hello \\\"world\\\"\"").unwrap();

        assert_eq!(
            tokens,
            vec![Token::StringLiteral(loc(), String::from("hello \"world\""))]
        );
    }

    #[test]
    fn test_closed_string_literal_with_escaped_backslash() {
        let tokens = tokenize("\"hello \\\\world\\\"\"").unwrap();

        assert_eq!(
            tokens,
            vec![Token::StringLiteral(loc(), String::from("hello \\world\""))]
        );
    }

    #[test]
    fn test_parse_parentheses() {
        let tokens = tokenize("()").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LeftParen(loc()),
                Token::RightParen(SourceLoc {
                    line: 0,
                    column: 1,
                    origin: String::from("__inline__")
                })
            ]
        );
    }

    #[test]
    fn test_parse_number() {
        let tokens = tokenize("123").unwrap();
        assert_eq!(tokens, vec![Token::Int(loc(), 123)]);
    }

    #[test]
    fn test_tokenize_string_literal() {
        let tokens = tokenize("\"hello world\"").unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral(loc(), String::from("hello world")),]
        );
    }

    #[test]
    fn test_tokenize_parentheses_with_integer() {
        let tokens = tokenize("(123)").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LeftParen(loc()),
                Token::Int(
                    SourceLoc {
                        line: 0,
                        column: 1,
                        origin: String::from("__inline__")
                    },
                    123
                ),
                Token::RightParen(SourceLoc {
                    line: 0,
                    column: 4,
                    origin: String::from("__inline__")
                }),
            ]
        );
    }

    #[test]
    fn test_tokenize_parentheses_with_string_literal() {
        let tokens = tokenize("(\"hello world\")").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LeftParen(loc()),
                Token::StringLiteral(
                    SourceLoc {
                        line: 0,
                        column: 1,
                        origin: String::from("__inline__")
                    },
                    String::from("hello world")
                ),
                Token::RightParen(SourceLoc {
                    line: 0,
                    column: 14,
                    origin: String::from("__inline__")
                }),
            ]
        );
    }

    #[test]
    fn test_tokenize_parentheses_with_atom() {
        let tokens = tokenize("(hello)").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LeftParen(loc()),
                Token::Atom(
                    SourceLoc {
                        line: 0,
                        column: 1,
                        origin: String::from("__inline__")
                    },
                    String::from("hello")
                ),
                Token::RightParen(SourceLoc {
                    line: 0,
                    column: 6,
                    origin: String::from("__inline__")
                }),
            ]
        );
    }

    #[test]
    fn test_tokenize_equation() {
        let tokens = tokenize("(+ 1 3)").unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::LeftParen(loc()),
                Token::Atom(
                    SourceLoc {
                        line: 0,
                        column: 1,
                        origin: String::from("__inline__")
                    },
                    String::from("+")
                ),
                Token::Int(
                    SourceLoc {
                        line: 0,
                        column: 3,
                        origin: String::from("__inline__")
                    },
                    1
                ),
                Token::Int(
                    SourceLoc {
                        line: 0,
                        column: 5,
                        origin: String::from("__inline__")
                    },
                    3
                ),
                Token::RightParen(SourceLoc {
                    line: 0,
                    column: 6,
                    origin: String::from("__inline__")
                }),
            ]
        );
    }

    #[test]
    #[should_panic]
    // TODO: need better assert, need to test with the inline reporter.
    fn test_unclosed_string_literal() {
        let _ = tokenize("\"hello world").unwrap();
    }

    #[test]
    #[should_panic]
    // TODO: need better assert, need to test with the inline reporter.
    fn test_malformed_float() {
        let _ = tokenize("1.9.9").unwrap();
    }

    #[test]
    fn test_simple_equation() {
        let tokens = tokenize("(+ 1 3)").unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::LeftParen(SourceLoc {
                    line: 0,
                    column: 0,
                    origin: String::from("__inline__")
                }),
                Token::Atom(
                    SourceLoc {
                        line: 0,
                        column: 1,
                        origin: String::from("__inline__"),
                    },
                    String::from("+")
                ),
                Token::Int(
                    SourceLoc {
                        line: 0,
                        column: 3,
                        origin: String::from("__inline__"),
                    },
                    1
                ),
                Token::Int(
                    SourceLoc {
                        line: 0,
                        column: 5,
                        origin: String::from("__inline__"),
                    },
                    3
                ),
                Token::RightParen(SourceLoc {
                    line: 0,
                    column: 6,
                    origin: String::from("__inline__")
                }),
            ]
        )
    }

    #[test]
    fn test_parse_quote_as_atom() {
        let tokens = tokenize("'(1 3)").unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Atom(
                    SourceLoc {
                        line: 0,
                        column: 0,
                        origin: String::from("__inline__")
                    },
                    String::from("'")
                ),
                Token::LeftParen(SourceLoc {
                    line: 0,
                    column: 1,
                    origin: String::from("__inline__")
                }),
                Token::Int(
                    SourceLoc {
                        line: 0,
                        column: 2,
                        origin: String::from("__inline__"),
                    },
                    1
                ),
                Token::Int(
                    SourceLoc {
                        line: 0,
                        column: 4,
                        origin: String::from("__inline__"),
                    },
                    3
                ),
                Token::RightParen(SourceLoc {
                    line: 0,
                    column: 5,
                    origin: String::from("__inline__")
                }),
            ]
        )
    }

    #[test]
    fn test_multi_line_sexpression() {
        let sexp = r#"
(define (add a b) (+ a b))
(add 1 2)
"#;

        let tokens = tokenize(sexp).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LeftParen(SourceLoc {
                    line: 1,
                    column: 0,
                    origin: String::from("__inline__"),
                }),
                Token::Atom(
                    SourceLoc {
                        line: 1,
                        column: 1,
                        origin: String::from("__inline__"),
                    },
                    String::from("define"),
                ),
                Token::LeftParen(SourceLoc {
                    line: 1,
                    column: 8,
                    origin: String::from("__inline__"),
                }),
                Token::Atom(
                    SourceLoc {
                        line: 1,
                        column: 9,
                        origin: String::from("__inline__"),
                    },
                    String::from("add"),
                ),
                Token::Atom(
                    SourceLoc {
                        line: 1,
                        column: 13,
                        origin: String::from("__inline__"),
                    },
                    String::from("a"),
                ),
                Token::Atom(
                    SourceLoc {
                        line: 1,
                        column: 15,
                        origin: String::from("__inline__"),
                    },
                    String::from("b"),
                ),
                Token::RightParen(SourceLoc {
                    line: 1,
                    column: 16,
                    origin: String::from("__inline__"),
                }),
                Token::LeftParen(SourceLoc {
                    line: 1,
                    column: 18,
                    origin: String::from("__inline__"),
                }),
                Token::Atom(
                    SourceLoc {
                        line: 1,
                        column: 19,
                        origin: String::from("__inline__"),
                    },
                    String::from("+"),
                ),
                Token::Atom(
                    SourceLoc {
                        line: 1,
                        column: 21,
                        origin: String::from("__inline__"),
                    },
                    String::from("a"),
                ),
                Token::Atom(
                    SourceLoc {
                        line: 1,
                        column: 23,
                        origin: String::from("__inline__"),
                    },
                    String::from("b"),
                ),
                Token::RightParen(SourceLoc {
                    line: 1,
                    column: 24,
                    origin: String::from("__inline__"),
                }),
                Token::RightParen(SourceLoc {
                    line: 1,
                    column: 25,
                    origin: String::from("__inline__"),
                }),
                Token::LeftParen(SourceLoc {
                    line: 2,
                    column: 0,
                    origin: String::from("__inline__"),
                }),
                Token::Atom(
                    SourceLoc {
                        line: 2,
                        column: 1,
                        origin: String::from("__inline__"),
                    },
                    String::from("add"),
                ),
                Token::Int(
                    SourceLoc {
                        line: 2,
                        column: 5,
                        origin: String::from("__inline__"),
                    },
                    1,
                ),
                Token::Int(
                    SourceLoc {
                        line: 2,
                        column: 7,
                        origin: String::from("__inline__"),
                    },
                    2,
                ),
                Token::RightParen(SourceLoc {
                    line: 2,
                    column: 8,
                    origin: String::from("__inline__"),
                }),
            ],
        )
    }
}
