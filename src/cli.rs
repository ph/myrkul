use std::{fs, io, io::Write, path::PathBuf};

use clap::{arg, Command};
use rustyline::{error::ReadlineError, DefaultEditor};

use crate::tokenizer::{source::Source, Tokenizer};

fn cli() -> Command {
    Command::new("myrkul")
        .about("Myrkul language")
        .subcommand(Command::new("repl").about("interractive repl"))
        .subcommand(
            Command::new("run")
                .flatten_help(true)
                .about("run script")
                .arg(arg!(-e --stdin <INPUT> "read code from stdin and execute"))
                .arg(arg!([file] "file to execute")),
        )
        .subcommand(Command::new("version").about("return version"))
}

pub fn execute() -> Result<(), Box<dyn std::error::Error>> {
    let _cmd = cli();
    let matches = cli().get_matches();

    match matches.subcommand() {
        Some(("repl", _sub_matches)) => execute_repl(ReplArgs),
        Some(("version", _sub_matches)) => execute_version(VersionArgs)?,
        Some(("run", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").map(PathBuf::from);
            let stdin_input = sub_matches.get_one::<String>("stdin").cloned();

            execute_run(RunArgs { file, stdin_input })?
        }
        _ => {}
    }

    Ok(())
}

struct ReplArgs;

fn execute_repl(_args: ReplArgs) {
    let mut rl = DefaultEditor::new().unwrap();
    let mut counter = 1;

    let _ = rl.load_history(&history_path());

    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                println!("#{} {}", counter, line);
                counter += 1;

                let source = Source::from(line);

                execute_code(source).unwrap();
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => println!("Error: {}", err),
        }
    }
}

fn history_path() -> String {
    "history.txt".to_string()
}

struct RunArgs {
    file: Option<PathBuf>,
    stdin_input: Option<String>,
}

fn execute_run(args: RunArgs) -> Result<(), Box<dyn std::error::Error>> {
    let mut sb = String::new();
    let mut file_path = String::new();

    if let Some(std_input) = args.stdin_input {
        sb.push_str(&std_input);
    }

    if let Some(path) = args.file {
        let content = fs::read_to_string(&path)?;
        file_path = path.to_string_lossy().to_string();
        sb.push_str(&content);
    }

    let source = Source::new(file_path, sb);
    execute_code(source)?;

    Ok(())
}

fn execute_code(source: Source) -> Result<(), Box<dyn std::error::Error>> {
    let tokens = Tokenizer::new(&source).parse().unwrap();
    println!("{:?}", tokens);

    Ok(())
}

struct VersionArgs;

fn execute_version(_args: VersionArgs) -> Result<(), Box<dyn std::error::Error>> {
    let mut stdout = io::stdout();
    writeln!(stdout, "version: 0.1")?;
    Ok(())
}
