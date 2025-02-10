use std::{
    fmt::{self, Display, Formatter},
    io::{BufRead, Read},
};

use self::circular_buffer::CircularBuffer;
mod circular_buffer;
mod output;
mod terminal;

#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Io(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

/// Context holder the context where an error or a message should be applied to the source code.
#[derive(Debug)]
pub struct Context {
    pub line: usize,
    pub column: usize,
    pub message: String,
    pub before_context: String,
}

pub struct Reporter {
    before_context: usize,
}

impl Reporter {
    pub fn new(before_context: usize) -> Self {
        Self { before_context }
    }

    pub fn extract_context<S: Read + BufRead>(
        &self,
        source: &mut S,
        inline_messages: &[impl InlineMessage],
    ) -> Result<Vec<Context>, Error> {
        let mut contexts = Vec::new();

        let mut circular_buffer: CircularBuffer<String> = CircularBuffer::new(self.before_context);
        let mut inline_messages_idx = 0;

        for (line_number, result) in source.lines().enumerate() {
            let line = result?;

            // We have done all the work we need to do
            if inline_messages_idx >= inline_messages.len() {
                break;
            }

            let current_message = &inline_messages[inline_messages_idx];

            circular_buffer.push(line.clone());

            if current_message.line() == line_number {
                let before_context = circular_buffer
                    .iter()
                    .map(|s| s.clone())
                    .collect::<Vec<_>>()
                    .join("\n");

                contexts.push(Context {
                    line: current_message.line(),
                    column: current_message.column(),
                    message: current_message.message(),
                    before_context,
                });

                inline_messages_idx += 1;
            }
        }

        if inline_messages_idx < inline_messages.len() {
            panic!("Error message line is bigger than the number of lines in current source codesource");
        }

        Ok(contexts)
    }
}

pub trait InlineMessage: std::fmt::Debug {
    fn line(&self) -> usize;
    fn column(&self) -> usize;
    fn message(&self) -> String;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct M {
        line: usize,
        column: usize,
        message: String,
    }

    impl M {
        fn new(line: usize, column: usize, message: String) -> Self {
            Self {
                line,
                column,
                message,
            }
        }
    }

    impl InlineMessage for M {
        fn line(&self) -> usize {
            self.line
        }

        fn column(&self) -> usize {
            self.column
        }

        fn message(&self) -> String {
            self.message.clone()
        }
    }

    fn extract_from(src: &str, inline_messages: &[impl InlineMessage]) -> Vec<Context> {
        let mut src = str::trim(src).as_bytes();
        let reporter = Reporter::new(3);
        reporter.extract_context(&mut src, inline_messages).unwrap()
    }

    fn src_code() -> String {
        r"
    (define fname (x)
       (cond (
	 (eq x 1) (x + 20)
	 (eq x 3) (x + 30)
	 (t x + 100))))"
            .to_string()
    }

    fn setup_two_context() -> (Vec<Context>, Vec<impl InlineMessage>) {
        let m1 = M::new(2, 5, "missing closing parenthesis".to_string());
        let m2 = M::new(4, 9, "again missing closing parenthesis".to_string());

        let inline_messages = vec![m1, m2];

        (extract_from(&src_code(), &inline_messages), inline_messages)
    }

    #[test]
    fn test_extract_multiple_context_from_source() {
        let (extracted, inlines_messages) = setup_two_context();
        assert_eq!(extracted.len(), inlines_messages.len());
    }

    #[test]
    fn test_lines_should_match_supplied_inline_messages() {
        let (extracted, inlines_messages) = setup_two_context();

        extracted
            .iter()
            .zip(inlines_messages.iter())
            .for_each(|(e, i)| assert_eq!(e.line, i.line()));
    }

    #[test]
    fn test_column_should_match_supplied_inline_messages() {
        let (extracted, inlines_messages) = setup_two_context();

        extracted
            .iter()
            .zip(inlines_messages.iter())
            .for_each(|(e, i)| assert_eq!(e.column, i.column()));
    }

    #[test]
    fn test_message_should_match_supplied_inline_messages() {
        let (extracted, inlines_messages) = setup_two_context();

        extracted
            .iter()
            .zip(inlines_messages.iter())
            .for_each(|(e, i)| assert_eq!(e.message, i.message()));
    }

    #[test]
    fn test_extract_first_around_context_from_source() {
        let (extracted, _) = setup_two_context();
        assert_eq!(
            extracted[0].before_context,
            str::trim(
                r"
    (define fname (x)
       (cond (
	 (eq x 1) (x + 20)"
            )
        );
    }

    #[test]
    fn test_extract_second_around_context_from_source() {
        let (extracted, _) = setup_two_context();
        assert_eq!(
            str::trim(&extracted[1].before_context),
            str::trim(
                r"
	 (eq x 1) (x + 20)
	 (eq x 3) (x + 30)
	 (t x + 100))))"
            )
        );
    }

    #[test]
    #[should_panic]
    fn test_should_panic_when_error_line_is_bigger_than_source_lines() {
        let m1 = M::new(100, 5, "missing closing parenthesis".to_string());
        let inline_messages = vec![m1];

        let src = src_code();
        let mut src = src.as_bytes();

        let reporter = Reporter::new(3);
        reporter
            .extract_context(&mut src, &inline_messages)
            .unwrap();
    }
}
