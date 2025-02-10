use std::io::Write;

use super::{terminal, Context, Error};

/// Output is a trait that defines the interface for printing a Context.
#[allow(dead_code)]
pub trait Output {
    fn print(&mut self, context: &Context) -> Result<(), Error>;
    fn print_all(&mut self, contexts: &[Context]) -> Result<(), Error> {
        for context in contexts {
            self.print(context)?;
        }
        Ok(())
    }
}

/// PlainConsole is a simple output implementation that prints the context to a Write without any color.
pub struct PlainConsole<W: Write> {
    out: W,
}

/// Implement the Output trait for PlainConsole.
#[allow(dead_code)]
impl<W: Write> PlainConsole<W> {
    pub fn new(out: W) -> Self {
        Self { out }
    }
}

/// Implement the Output trait for PlainConsole.
impl<W: Write> Output for PlainConsole<W> {
    /// Print the context to the provider object implementing Write without any color.
    fn print(&mut self, context: &Context) -> Result<(), Error> {
        writeln!(self.out, "{}", context.before_context)?;
        writeln!(
            self.out,
            "{: >width$}^- {}",
            "",
            context.message,
            width = context.column - 1
        )?;
        Ok(())
    }
}

pub struct ColorConsole<W: Write> {
    out: W,
}

#[allow(dead_code)]
impl<W: Write> ColorConsole<W> {
    pub fn new(out: W) -> Self {
        Self { out }
    }
}

impl<W: Write> Output for ColorConsole<W> {
    fn print(&mut self, context: &Context) -> Result<(), Error> {
        writeln!(self.out, "{}", terminal::cyan(&context.before_context))?;
        writeln!(
            self.out,
            "{: >width$}^- {}",
            "",
            terminal::red(&context.message),
            width = context.column - 1
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plain_console() {
        let context = Context {
            line: 1,
            column: 3,
            message: "this should work".to_string(),
            before_context: "Aaaaaa\nBbbbbbbb\nCcccccc".to_string(),
        };

        let mut out = Vec::new();
        let mut console = PlainConsole::new(&mut out);
        console.print(&context).unwrap();

        assert_eq!(
            String::from_utf8(out).unwrap(),
            "Aaaaaa\nBbbbbbbb\nCcccccc\n  ^- this should work\n"
        );
    }

    #[test]
    fn test_multiple_context_plain_console() {
        let contexts = vec![
            Context {
                line: 1,
                column: 3,
                message: "this should work".to_string(),
                before_context: "A".to_string(),
            },
            Context {
                line: 2,
                column: 3,
                message: "this should work".to_string(),
                before_context: "B".to_string(),
            },
        ];

        let mut out = Vec::new();
        let mut console = PlainConsole::new(&mut out);
        console.print_all(&contexts).unwrap();

        assert_eq!(
            String::from_utf8(out).unwrap(),
            "A\n  ^- this should work\nB\n  ^- this should work\n"
        );
    }

    #[test]
    fn test_color_console() {
        let context = Context {
            line: 1,
            column: 3,
            message: "this should work".to_string(),
            before_context: "Aaaaaa\nBbbbbbbb\nCcccccc".to_string(),
        };

        let mut out = Vec::new();
        let mut console = ColorConsole::new(&mut out);
        console.print(&context).unwrap();

        assert_eq!(
	    String::from_utf8(out).unwrap(),
	    "\u{1b}[36mAaaaaa\nBbbbbbbb\nCcccccc\u{1b}[36m\n  ^- \u{1b}[31mthis should work\u{1b}[36m\n"
	);
    }
}
