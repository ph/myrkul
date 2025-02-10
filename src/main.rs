use std::process;

use tracing::Level;

use crate::tokenizer::tokenize;

mod ast;
mod cli;
mod default_runtime;
mod function;
mod inline_reporter;
mod interpreter;
mod runtime;
mod tokenizer;

fn setup_logging() {
    tracing_subscriber::fmt().with_max_level(Level::INFO).init();
}

fn main() {
    match cli::execute() {
        Ok(_) => process::exit(exitcode::OK),
        Err(e) => {
            println!("{}", e);
            process::exit(exitcode::USAGE);
        }
    }
}
