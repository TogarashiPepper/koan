use crate::value::Value;
use std::collections::HashMap;

#[derive(Debug)]
pub struct State {
    pub variables: HashMap<String, Value>,
}

impl State {
    pub fn new() -> Self {
        State {
            variables: HashMap::new(),
        }
    }
}
