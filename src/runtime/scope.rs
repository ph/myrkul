use std::{
    collections::{BTreeMap, VecDeque},
    fmt::Display,
};

#[derive(Debug)]
pub enum LexicalScopeError {
    SymbolNotFound(Atom),
}

impl Display for LexicalScopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexicalScopeError::SymbolNotFound(s) => {
                write!(f, "symbol {} not found in current scope", s)
            }
        }
    }
}

impl std::error::Error for LexicalScopeError {}

#[derive(Ord, PartialEq, Eq, PartialOrd, Debug, Clone)]
pub struct Atom(String);

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for Atom {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for Atom {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Integer(isize),
}

impl From<Value> for isize {
    fn from(value: Value) -> Self {
        let Value::Integer(v) = value;
        v
    }
}

pub type Function = fn(&LexicalScope, &BTreeMap<Atom, Value>) -> Value;
pub type Scope = BTreeMap<Atom, Function>;

pub struct LexicalScope {
    scopes: VecDeque<Scope>,
}

#[allow(dead_code)]
impl LexicalScope {
    pub fn new() -> Self {
        let global_scope = Scope::new();

        Self {
            scopes: VecDeque::from([global_scope]),
        }
    }

    pub fn register(&mut self, atom: Atom, f: Function) {
        self.scopes
            .get_mut(self.scopes.len() - 1)
            .unwrap()
            .insert(atom, f);
    }

    pub fn lookup(&mut self, atom: &Atom) -> Result<&Function, LexicalScopeError> {
        self.scopes
            .iter()
            .find_map(|s| s.get(atom))
            .ok_or(LexicalScopeError::SymbolNotFound(atom.clone()))
    }

    pub fn new_scope(&mut self) {
        self.scopes.push_front(Scope::new());
    }

    pub fn drop_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("INVARIANT: LexicalScope must have at least a scope.")
        }
        self.scopes.pop_front();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn calc(_lexical_scope: &LexicalScope, _args: &BTreeMap<Atom, Value>) -> Value {
        Value::Integer(5)
    }

    fn calc_two(_lexical_scope: &LexicalScope, _args: &BTreeMap<Atom, Value>) -> Value {
        Value::Integer(6)
    }

    #[test]
    fn test_register_function() {
        let mut lexical_scope = LexicalScope::new();
        let atom = Atom::from("hey");

        lexical_scope.register(atom, calc);
    }

    #[test]
    fn test_lookup_function() {
        let mut lexical_scope = LexicalScope::new();
        let atom = Atom::from("hey");
        lexical_scope.register(atom, calc);
        let f = lexical_scope.lookup(&Atom::from("hey")).unwrap();

        assert_eq!(f(&lexical_scope, &BTreeMap::new()), Value::Integer(5));
    }

    #[test]
    #[should_panic]
    fn test_panic_on_dropping_when_no_scope_is_associated() {
        let mut lexical_scope = LexicalScope::new();
        lexical_scope.drop_scope();
    }

    #[test]
    fn test_override_value_in_sub_scope() {
        let mut lexical_scope = LexicalScope::new();
        let atom = Atom::from("hey");

        lexical_scope.register(atom.clone(), calc);
        let f = lexical_scope.lookup(&atom).unwrap();
        assert_eq!(f(&lexical_scope, &BTreeMap::new()), Value::Integer(5));

        lexical_scope.new_scope();
        lexical_scope.register(atom.clone(), calc_two);
        let f = lexical_scope.lookup(&atom).unwrap();
        assert_eq!(f(&lexical_scope, &BTreeMap::new()), Value::Integer(6));
    }
}
