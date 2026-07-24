use std::collections::HashMap;

use crate::values::Value;

pub type Env = HashMap<String, Value>;

pub trait EnvExt {
    fn lookup(&self, name: &str) -> Value;
}

impl EnvExt for Env {
    fn lookup(&self, name: &str) -> Value {
        match self.get(name) {
            Some(value) => value.clone(),
            None => Value::err("unbound symbol", Value::symbol(name)),
        }
    }
}
