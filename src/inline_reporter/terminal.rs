use std::fmt::Display;

#[derive(Debug)]
#[allow(dead_code)]
pub enum Color {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
    Reset,
}

impl Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let color = match self {
            Color::Black => "\x1b[30m",
            Color::Red => "\x1b[31m",
            Color::Green => "\x1b[32m",
            Color::Yellow => "\x1b[33m",
            Color::Blue => "\x1b[34m",
            Color::Magenta => "\x1b[35m",
            Color::Cyan => "\x1b[36m",
            Color::White => "\x1b[37m",
            Color::BrightBlack => "\x1b[90m",
            Color::BrightRed => "\x1b[91m",
            Color::BrightGreen => "\x1b[92m",
            Color::BrightYellow => "\x1b[93m",
            Color::BrightBlue => "\x1b[94m",
            Color::BrightMagenta => "\x1b[95m",
            Color::BrightCyan => "\x1b[96m",
            Color::BrightWhite => "\x1b[97m",
            Color::Reset => "\x1b[0m",
        };
        write!(f, "{}", color)
    }
}

pub fn t(color: Color, i: impl Display) -> String {
    format!("{}{}{}", color, i, Color::Cyan)
}

macro_rules! color_fn {
    ($name:ident, $color:ident) => {
        #[allow(dead_code)]
        pub fn $name(i: impl Display) -> String {
            t(Color::$color, i)
        }
    };
}

color_fn!(black, Black);
color_fn!(red, Red);
color_fn!(green, Green);
color_fn!(yellow, Yellow);
color_fn!(blue, Blue);
color_fn!(magenta, Magenta);
color_fn!(cyan, Cyan);
color_fn!(white, White);
color_fn!(bright_black, BrightBlack);
color_fn!(bright_red, BrightRed);
color_fn!(bright_green, BrightGreen);
color_fn!(bright_yellow, BrightYellow);
color_fn!(bright_blue, BrightBlue);
color_fn!(bright_magenta, BrightMagenta);
color_fn!(bright_cyan, BrightCyan);
color_fn!(bright_white, BrightWhite);
color_fn!(reset, Reset);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_t() {
        assert_eq!(t(Color::Red, "test"), "\x1b[31mtest\x1b[36m");
        assert_eq!(t(Color::Cyan, "test"), "\x1b[36mtest\x1b[36m");
    }

    #[test]
    fn test_red() {
        assert_eq!(red("test"), "\x1b[31mtest\x1b[36m");
    }

    #[test]
    fn test_cyan() {
        assert_eq!(cyan("test"), "\x1b[36mtest\x1b[36m");
    }
}
