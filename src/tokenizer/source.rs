use std::fmt::Display;

/// "__inline__" is a default source name for inline sources, mostly used with the REPL or evaluating expressions.
const DEFAULT_SOURCE_NAME: &str = "__inline__";

/// Source represents a source of code, which can be a file, a string, or a REPL input.
#[derive(Clone, Debug)]
pub struct Source {
    origin: String,
    content: String,
}

impl Source {
    /// Create a new source with an origin and content.
    pub fn new(origin: impl Into<String>, content: impl Into<String>) -> Self {
        Source {
            origin: origin.into(),
            content: content.into(),
        }
    }

    /// Get the origin of the source, this is usually a file path or "__inline__" for inline sources.
    pub fn origin(&self) -> &str {
        &self.origin
    }

    /// Get the content of the source.
    pub fn content(&self) -> &str {
        &self.content
    }
}

impl Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const LIMIT: usize = 15;

        let mut s = f.debug_struct("source");
        s.field("origin", &self.origin);

        if self.content.len() > LIMIT {
            s.field("content", &self.content.get(0..LIMIT));
        } else {
            s.field("content", &self.content);
        }

        s.finish()
    }
}

/// Implementations for converting from a string or a string slice to a source.
impl From<String> for Source {
    fn from(content: String) -> Self {
        Source::new(DEFAULT_SOURCE_NAME, content)
    }
}

/// Implementations for converting from a &&str to a source
impl From<&str> for Source {
    fn from(content: &str) -> Self {
        Source::new(DEFAULT_SOURCE_NAME, content.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_a_normal_source_with_an_origin() {
        let src = Source::new("test", "hello");
        assert_eq!(src.origin(), "test");
        assert_eq!(src.content(), "hello");
    }

    #[test]
    fn test_convert_from_string() {
        let src = Source::from(String::from("hello"));
        assert_eq!(src.origin(), DEFAULT_SOURCE_NAME);
        assert_eq!(src.content(), "hello");
    }

    #[test]
    fn test_convert_from_str() {
        let src = Source::from("hello");
        assert_eq!(src.origin(), DEFAULT_SOURCE_NAME);
        assert_eq!(src.content(), "hello");
    }
}
