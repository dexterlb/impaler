use std::collections::HashMap;

use crate::value_list::ValueList;
use crate::values::Value;

pub type Env = HashMap<String, Value>;

pub trait EnvExt {
    fn lookup(&self, name: &str) -> Value;
    fn from_val(value: &Value) -> Option<Self>
    where
        Self: Sized;
}

impl EnvExt for Env {
    fn lookup(&self, name: &str) -> Value {
        match self.get(name) {
            Some(value) => value.clone(),
            None => Value::err("unbound symbol", Value::symbol(name)),
        }
    }

    fn from_val(value: &Value) -> Option<Self> {
        let mut env = Env::new();
        for pair in ValueList::from_val(value)?.to_vec() {
            match pair {
                Value::Pair(cell) => match &cell.0 {
                    Value::Symbol(name) => {
                        env.insert(name.clone(), cell.1.clone());
                    }
                    _ => return None,
                },
                _ => return None,
            }
        }
        Some(env)
    }
}
