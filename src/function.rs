use crate::{ast::Value, runtime::LexicalScope};

type Atom = String;

#[allow(dead_code)]
#[derive(Debug, Eq, PartialOrd, Ord)]
enum ArgumentType {
    Single(String),
    Splatted(String),
}

impl PartialEq for ArgumentType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Single(l0), Self::Single(r0)) => l0 == r0,
            (Self::Splatted(l0), Self::Splatted(r0)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum FunctionError {
    InvalidArity(Arity, usize),
}

impl std::fmt::Display for FunctionError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            FunctionError::InvalidArity(expected_arity, got) => {
                write!(
                    f,
                    "Invalid arity: expected {} arguments got {}",
                    expected_arity, got
                )
            }
        }
    }
}

impl std::error::Error for FunctionError {}

#[derive(Debug, Clone, PartialEq)]
enum Arity {
    Finite(usize),
    Variadic(usize),
}

#[allow(dead_code)]
struct Function {
    name: Atom,
    args: Vec<ArgumentType>,
}

impl std::fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Arity::Finite(count) => write!(f, "{}", count),
            Arity::Variadic(count) => write!(f, "{}+", count),
        }
    }
}

#[allow(dead_code)]
impl Function {
    fn new(name: Atom, args: Vec<ArgumentType>) -> Function {
        Function { name, args }
    }

    fn eval(&self, _scope: LexicalScope, args: Vec<Value>) -> Result<Value, FunctionError> {
        self.match_arity(&args)?;
        Ok(Value::Bool(true))
    }

    fn match_arity(&self, args: &[Value]) -> Result<(), FunctionError> {
        let arity = self.arity();

        match arity {
            Arity::Finite(count) => {
                if args.len() == count {
                    Ok(())
                } else {
                    Err(FunctionError::InvalidArity(arity, args.len()))
                }
            }
            Arity::Variadic(count) => {
                if args.len() >= count {
                    Ok(())
                } else {
                    Err(FunctionError::InvalidArity(arity, args.len()))
                }
            }
        }
    }

    fn arity(&self) -> Arity {
        let count = self.args.len();
        if count > 0 {
            if let Some(ArgumentType::Splatted(_)) = &self.args.get(count - 1) {
                return Arity::Variadic(count);
            }
        }

        Arity::Finite(count)
    }
}

// mod test {
//     use super::*;

// fn test_create_function() {
//     let _f = Function {
//         name: "foo".to_string(),
//         args: Vec::new(),
//     };
// }

// fn test_eval_function_no_arguments() {
//     let f = Function {
//         name: "foo".to_string(),
//         args: Vec::new(),
//     };

//     let scope = LexicalScope::new();
//     assert_eq!(f.eval(scope, Vec::new()), Ok(Value::Bool(true)));
// }
// }
