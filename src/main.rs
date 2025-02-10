use std::process;

use tracing::Level;

mod ast;
mod cli;
mod default_runtime;
mod function;
mod inline_reporter;
mod runtime;
mod tokenizer;

fn setup_logging() {
    tracing_subscriber::fmt().with_max_level(Level::INFO).init();
}

fn main() {
    setup_logging();

    match cli::execute() {
        Ok(_) => process::exit(exitcode::OK),
        Err(e) => {
            println!("{}", e);
            process::exit(exitcode::USAGE);
        }
    }
}
