#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    column: usize,
    line: usize,
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}:{})", self.line, self.column)
    }
}

impl Source {
    fn new(column: usize, line: usize) -> Self {
        Source { column, line }
    }
}

#[derive(Debug, Clone)]
pub enum LexerErr {
    UnterminatedStringLiteral(Source),
}

impl std::error::Error for LexerErr {}

impl std::fmt::Display for LexerErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerErr::UnterminatedStringLiteral(source) => {
                write!(f, "unterminated string literal beginning at `{}`", source)
            }
        }
    }
}

#[allow(unused)]
#[derive(Debug, PartialEq)]
pub enum Token<'s> {
    Identifier(Source, &'s str),
    LeftParen(Source),
    RightParen(Source),
    LeftBracket(Source),
    RightBracket(Source),
    String(Source, &'s str),
    Number(Source, Number),
}

#[allow(unused)]
#[derive(Debug, PartialEq)]
pub enum Number {
    Integer(usize),
    Float(f64),
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(value, ..) => write!(f, "Atom({})", value),
            Token::LeftParen(..) => write!(f, "("),
            Token::RightParen(..) => write!(f, ")"),
            Token::LeftBracket(..) => write!(f, "["),
            Token::RightBracket(..) => write!(f, "]"),
            Token::String(value, ..) => write!(f, "String(\"{}\")", value),
            Token::Number(value, ..) => write!(f, "Numer({})", value),
        }
    }
}

pub struct Lexer<'s> {
    source: &'s str,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Lexer { source }
    }
}

impl<'s> IntoIterator for Lexer<'s> {
    type Item = Result<Token<'s>, LexerErr>;
    type IntoIter = LexerIntoIterator<'s>;

    fn into_iter(self) -> Self::IntoIter {
        LexerIntoIterator {
            remainder: self.source,
            ..Default::default()
        }
    }
}

#[derive(Default)]
pub struct LexerIntoIterator<'s> {
    remainder: &'s str,
    column: usize,
    line: usize,
}

impl<'s> LexerIntoIterator<'s> {
    fn src(&self) -> Source {
        Source::new(self.column, self.line)
    }
}

impl<'s> Iterator for LexerIntoIterator<'s> {
    type Item = Result<Token<'s>, LexerErr>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let cur_str = self.remainder;
            let mut iter = self.remainder.chars();
            let c = match iter.next() {
                Some(c) => {
                    self.remainder = iter.as_str();
                    c
                }
                None => return None,
            };

            let emit = move |tok| Some(Ok(tok));

            enum State {
                BeginIdentifier,
                BeginString,
                BeginMaybeNumber,
            }

            let s = match c {
                '(' => {
                    let tok = Token::LeftParen(self.src());
                    self.column += 1;
                    return emit(tok);
                }
                ')' => {
                    let tok = Token::RightParen(self.src());
                    self.column += 1;
                    return emit(tok);
                }
                '[' => {
                    let tok = Token::LeftBracket(self.src());
                    self.column += 1;
                    return emit(tok);
                }
                ']' => {
                    let tok = Token::RightBracket(self.src());
                    self.column += 1;
                    return emit(tok);
                }
                // partial match
                '"' => State::BeginString,
                ' ' => {
                    self.column += 1;
                    continue;
                }
                c if c.is_numeric() => State::BeginMaybeNumber,
                // ignore
                c if c.is_whitespace() => {
                    self.column = 0;
                    self.line += 1;
                    continue; // find next token
                }
                _ => State::BeginIdentifier,
            };

            break match s {
                State::BeginIdentifier => {
                    let ending = cur_str
                        .find(|c: char| is_end_of_identifier(c))
                        .unwrap_or_else(|| cur_str.len());

                    let v = &cur_str[..ending];
                    self.remainder = &cur_str[ending..];

                    let tok = Token::Identifier(self.src(), v);
                    self.column += v.len_utf8();
                    emit(tok)
                }
                State::BeginString => {
                    let mut ending = 0;
                    let mut escaped = false;

                    loop {
                        let c = iter.next();
                        ending += 1;

                        match c {
                            Some('"') => {
                                if escaped {
                                    escaped = false;
                                    continue;
                                }

                                let v = &self.remainder[..ending - 1];

                                if ending + 1 >= self.remainder.len() {
                                    self.remainder = "";
                                } else {
                                    self.remainder = &self.remainder[ending + 1..];
                                }

                                let tok = Token::String(self.src(), v);
                                self.column += v.len();
                                return emit(tok);
                            }
                            Some('\\') => {
                                if escaped {
                                    escaped = false;
                                } else {
                                    escaped = true
                                }
                            }
                            Some('\n') | Some('\r') => {
                                self.column = 0;
                                self.line += 1;
                            }
                            Some(_) => escaped = false,
                            None => {
                                return Some(Err(LexerErr::UnterminatedStringLiteral(self.src())))
                            }
                        }
                    }
                }
                State::BeginMaybeNumber => {
                    let ending = cur_str
                        .find(|c: char| is_end_of_identifier(c))
                        .unwrap_or_else(|| cur_str.len());

                    let v = &cur_str[..ending];
                    self.remainder = &cur_str[ending..];

                    let tok = if v.contains(".") {
                        match v.parse::<f64>() {
                            Ok(f) => Token::Number(self.src(), Number::Float(f)),
                            Err(_) => Token::Identifier(self.src(), v),
                        }
                    } else {
                        match v.parse::<usize>() {
                            Ok(i) => Token::Number(self.src(), Number::Integer(i)),
                            Err(_) => Token::Identifier(self.src(), v),
                        }
                    };

                    self.column += v.len();
                    emit(tok)
                }
            };
        }
    }
}

fn is_end_of_identifier(c: char) -> bool {
    match c {
        c if c.is_whitespace() => true,
        ')' | '(' | '[' | ']' => true,
        _ => false,
    }
}

#[allow(unused)]
fn tokenize<'s>(source: &'s str) -> Result<Vec<Token<'s>>, LexerErr> {
    let mut tokens = Vec::new();
    let lexer = Lexer::new(source);
    for token in lexer.into_iter() {
        match token {
            Ok(token) => tokens.push(token),
            Err(err) => return Err(err),
        }
    }
    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_identifier() {
        let tokens = tokenize("+").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(vec![Token::Identifier(Source::new(0, 0), "+")], tokens);
    }

    #[test]
    fn parse_two_identifiers() {
        let tokens = tokenize("+ -").unwrap();
        assert_eq!(2, tokens.len());
        assert_eq!(
            vec![
                Token::Identifier(Source::new(0, 0), "+"),
                Token::Identifier(Source::new(2, 0), "-")
            ],
            tokens
        );
    }

    #[test]
    fn parse_integer() {
        let tokens = tokenize("10").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(
            vec![Token::Number(Source::new(0, 0), Number::Integer(10)),],
            tokens
        );
    }

    #[test]
    fn parse_float() {
        let tokens = tokenize("10.1").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(
            vec![Token::Number(Source::new(0, 0), Number::Float(10.1)),],
            tokens
        );
    }

    #[test]
    fn parse_maybe_float_but_return_identifier() {
        let tokens = tokenize("10.1a").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(vec![Token::Identifier(Source::new(0, 0), "10.1a"),], tokens);
    }

    #[test]
    fn parse_maybe_integer_but_return_identifier() {
        let tokens = tokenize("12f").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(vec![Token::Identifier(Source::new(0, 0), "12f"),], tokens);
    }

    #[test]
    fn parse_string() {
        let tokens = tokenize("\"hello world\"").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(
            vec![Token::String(Source::new(0, 0), "hello world"),],
            tokens
        );
    }

    #[test]
    fn parse_left_paren() {
        let tokens = tokenize("(").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(vec![Token::LeftParen(Source::new(0, 0))], tokens);
    }

    #[test]
    fn parse_right_paren() {
        let tokens = tokenize(")").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(vec![Token::RightParen(Source::new(0, 0))], tokens);
    }

    #[test]
    fn parse_left_bracket() {
        let tokens = tokenize("[").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(vec![Token::LeftBracket(Source::new(0, 0))], tokens);
    }

    #[test]
    fn parse_right_bracket() {
        let tokens = tokenize("]").unwrap();
        assert_eq!(1, tokens.len());
        assert_eq!(vec![Token::RightBracket(Source::new(0, 0))], tokens);
    }

    #[test]
    fn parse_simple_expression() {
        let tokens = tokenize("(+ 1 2.0)").unwrap();
        assert_eq!(5, tokens.len());
        assert_eq!(
            vec![
                Token::LeftParen(Source::new(0, 0)),
                Token::Identifier(Source::new(1, 0), "+"),
                Token::Number(Source::new(3, 0), Number::Integer(1)),
                Token::Number(Source::new(5, 0), Number::Float(2.0)),
                Token::RightParen(Source::new(8, 0)),
            ],
            tokens
        );
    }

    #[test]
    fn parse_embedded_expression() {
        let tokens = tokenize("(+ 1 (- 6 3))").unwrap();
        assert_eq!(9, tokens.len());
        assert_eq!(
            vec![
                Token::LeftParen(Source::new(0, 0)),
                Token::Identifier(Source::new(1, 0), "+"),
                Token::Number(Source::new(3, 0), Number::Integer(1)),
                Token::LeftParen(Source::new(5, 0)),
                Token::Identifier(Source::new(6, 0), "-"),
                Token::Number(Source::new(8, 0), Number::Integer(6)),
                Token::Number(Source::new(10, 0), Number::Integer(3)),
                Token::RightParen(Source::new(11, 0)),
                Token::RightParen(Source::new(12, 0)),
            ],
            tokens
        );
    }

    #[test]
    // (list "a" "b" "c")
    fn parse_list_of_strings() {
        let tokens = tokenize("(list \"a\" \"b\" \"c\")").unwrap();
        // assert_eq!(6, tokens.len());
        assert_eq!(
            vec![
                Token::LeftParen(Source::new(0, 0)),
                Token::Identifier(Source::new(1, 0), "list"),
                Token::String(Source::new(6, 0), "a"),
                Token::String(Source::new(10, 0), "b"),
                Token::String(Source::new(14, 0), "c"),
                Token::RightParen(Source::new(12, 0)),
            ],
            tokens
        );
    }
}
