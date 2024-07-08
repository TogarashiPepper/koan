use crate::value::Value;
use std::collections::HashMap;
use std::f64;

#[derive(Debug)]
pub struct State {
    // Array of environments, index = depth (i.e. `0` = global scope)
    pub variables: Vec<HashMap<String, Value>>,
}

impl State {
    pub fn new() -> Self {
        let mut variables = HashMap::new();
        variables.insert("π".to_owned(), Value::Num(f64::consts::PI));
        variables.insert("e".to_owned(), Value::Num(f64::consts::E));

        State {
            variables: vec![variables],
        }
    }
    
    pub fn empty() -> Self {
        State {
            variables: vec![HashMap::new()]
        }
    }

    pub(crate) fn set(&mut self, k: String, v: Value) {
        // Unwrap is okay bc we don't let length fall <1
        let x = self.variables.last_mut().unwrap();
        x.insert(k, v);
    }

    pub(crate) fn get(&self, k: &str) -> Option<Value> {
        for scope in self.variables.iter().rev() {
            if let Some(x) = scope.get(k) {
                return Some(x.clone());
            }
        }

        None
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}
