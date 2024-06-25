use crate::value::Value;
use std::collections::HashMap;

#[derive(Debug)]
pub struct State {
    pub variables: HashMap<String, Value>,
    pub child: Option<Box<State>>,
}

impl State {
    pub fn new() -> Self {
        State {
            variables: HashMap::new(),
            child: None,
        }
    }

    pub fn set(&mut self, k: String, v: Value) {
        self.variables.insert(k, v);
    }

    pub fn get(&self, k: &str) -> Option<Value> {
        match &self.child.as_ref().and_then(|c| c.get(k)) {
            Some(cv) => Some(cv.clone()),
            None => self.variables.get(k).cloned(),
        }
    }
}
