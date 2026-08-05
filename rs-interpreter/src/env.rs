use std::collections::HashMap;

use crate::value_list::ValueList;
use crate::values::{Value, ValueItem};

pub type Env = HashMap<String, Value>;

pub trait EnvExt {
    fn from_val(value: &Value) -> Option<Self>
    where
        Self: Sized;
    fn to_val(&self) -> Value;
}

impl EnvExt for Env {
    fn to_val(&self) -> Value {
        let pairs: Vec<Value> = self
            .iter()
            .map(|(name, value)| Value::pair(Value::symbol(name.clone()), value.clone()))
            .collect();
        Value::list(pairs)
    }

    fn from_val(value: &Value) -> Option<Self> {
        let mut env = Env::new();
        for pair in ValueList::from_val(value)?.to_vec() {
            match pair.get() {
                ValueItem::Pair(car, cdr) => match car.get() {
                    ValueItem::Symbol(name) => {
                        env.insert(name.clone(), cdr.clone());
                    }
                    _ => return None,
                },
                _ => return None,
            }
        }
        Some(env)
    }
}
