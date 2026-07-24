use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Symbol(String),
    Number(f64),
    String(String),
    Pair(Rc<(Value, Value)>),
    Null,
}

impl Value {
    pub fn symbol(name: impl Into<String>) -> Value {
        Value::Symbol(name.into())
    }

    pub fn number(x: f64) -> Value {
        Value::Number(x)
    }

    pub fn string(s: impl Into<String>) -> Value {
        Value::String(s.into())
    }

    pub fn pair(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new((car, cdr)))
    }

    pub fn list<I>(items: I) -> Value
    where
        I: IntoIterator<Item = Value>,
        I::IntoIter: DoubleEndedIterator,
    {
        items
            .into_iter()
            .rev()
            .fold(Value::Null, |cdr, car| Value::pair(car, cdr))
    }
}
