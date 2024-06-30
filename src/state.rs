use crate::value::Value;
use std::collections::HashMap;

#[derive(Debug)]
pub struct State {
    // Array of environments, index = depth (i.e. `0` = global scope)
    pub variables: Vec<HashMap<String, Value>>,
}

impl State {
    pub fn new() -> Self {
        State {
            variables: vec![HashMap::new()],
        }
    }

    pub fn set(&mut self, k: String, v: Value) {
        // Unwrap is okay bc we don't let length fall <1
        let x = self.variables.last_mut().unwrap();
        x.insert(k, v);
    }

    pub fn get(&self, k: &str) -> Option<Value> {
        for scope in self.variables.iter().rev() {
            if let Some(x) = scope.get(k) {
                return Some(x.clone());
            }
        }

        None
    }
}
