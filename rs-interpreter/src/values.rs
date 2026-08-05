use std::fmt;
use std::rc::Rc;

use crate::special_form::SpecialForm;
use crate::value_list::ValueList;

pub type Cont = Rc<dyn Fn(Value)>;

pub trait External: fmt::Debug + 'static {
    fn apply(&self, cont: Cont, args: ValueList);
    fn show(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct DebugInfo {
    pub filename: Option<String>,
    pub line_no: usize,
    pub char_offset: usize,
}

impl DebugInfo {
    pub fn show(&self) -> String {
        let file = self.filename.as_deref().unwrap_or("<unknown>");
        format!("{}:{}:{}", file, self.line_no, self.char_offset)
    }
}

#[derive(Debug, Clone)]
pub enum ValueItem {
    Symbol(String),
    Number(f64),
    String(String),
    Bool(bool),
    Pair(Rc<(Value, Value)>),
    Null,

    SpecialForm(SpecialForm),

    ExternalVal(Rc<dyn External>),
}

#[derive(Debug, Clone)]
pub struct Value {
    pub item: ValueItem,
    pub debug: Option<Rc<DebugInfo>>,
}

impl Value {
    fn bare(item: ValueItem) -> Value {
        Value { item, debug: None }
    }

    pub fn with_debug(mut self, info: DebugInfo) -> Value {
        self.debug = Some(Rc::new(info));
        self
    }

    pub fn symbol(name: impl Into<String>) -> Value {
        Value::bare(ValueItem::Symbol(name.into()))
    }

    pub fn number(x: f64) -> Value {
        Value::bare(ValueItem::Number(x))
    }

    pub fn string(s: impl Into<String>) -> Value {
        Value::bare(ValueItem::String(s.into()))
    }

    pub fn boolean(value: bool) -> Value {
        Value::bare(ValueItem::Bool(value))
    }

    pub fn pair(car: Value, cdr: Value) -> Value {
        Value::bare(ValueItem::Pair(Rc::new((car, cdr))))
    }

    pub fn null() -> Value {
        Value::bare(ValueItem::Null)
    }

    pub fn special_form(form: SpecialForm) -> Value {
        Value::bare(ValueItem::SpecialForm(form))
    }

    pub fn external(external: impl External) -> Value {
        Value::bare(ValueItem::ExternalVal(Rc::new(external)))
    }

    pub fn err(message: impl Into<String>, value: Value) -> Value {
        panic!("{}: {}", message.into(), value.show())
    }

    pub fn list<I>(items: I) -> Value
    where
        I: IntoIterator<Item = Value>,
        I::IntoIter: DoubleEndedIterator,
    {
        items
            .into_iter()
            .rev()
            .fold(Value::null(), |cdr, car| Value::pair(car, cdr))
    }

    pub fn show(&self) -> String {
        match &self.item {
            ValueItem::Symbol(name) => name.clone(),
            ValueItem::Number(x) => format!("{}", x),
            ValueItem::String(s) => format!("{:?}", s),
            ValueItem::Bool(value) => if *value { "#t" } else { "#f" }.to_string(),
            ValueItem::SpecialForm(form) => form.show(),
            ValueItem::ExternalVal(external) => external.show(),
            ValueItem::Null => "()".to_string(),
            ValueItem::Pair(_) => {
                let mut out = String::from("(");
                let mut current = self;
                let mut first = true;
                loop {
                    match &current.item {
                        ValueItem::Pair(cell) => {
                            if !first {
                                out.push(' ');
                            }
                            first = false;
                            out.push_str(&cell.0.show());
                            current = &cell.1;
                        }
                        ValueItem::Null => break,
                        _ => {
                            out.push_str(" . ");
                            out.push_str(&current.show());
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
        }
    }
}

#[cfg(test)]
impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (&self.item, &other.item) {
            (ValueItem::Symbol(a), ValueItem::Symbol(b)) => a == b,
            (ValueItem::Number(a), ValueItem::Number(b)) => a == b,
            (ValueItem::String(a), ValueItem::String(b)) => a == b,
            (ValueItem::Bool(a), ValueItem::Bool(b)) => a == b,
            (ValueItem::Pair(a), ValueItem::Pair(b)) => a == b,
            (ValueItem::Null, ValueItem::Null) => true,
            (ValueItem::ExternalVal(a), ValueItem::ExternalVal(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct Prim(&'static str);

    impl External for Prim {
        fn apply(&self, _cont: Cont, _args: ValueList) {}

        fn show(&self) -> String {
            format!("#<primitive {}>", self.0)
        }
    }

    #[test]
    fn displays_atoms() {
        assert_eq!(Value::symbol("foo").show(), "foo");
        assert_eq!(Value::number(42.0).show(), "42");
        assert_eq!(Value::number(3.5).show(), "3.5");
        assert_eq!(Value::string("hi").show(), "\"hi\"");
        assert_eq!(Value::null().show(), "()");
    }

    #[test]
    fn displays_list() {
        let value = Value::list([Value::symbol("add"), Value::number(1.0), Value::number(2.0)]);
        assert_eq!(value.show(), "(add 1 2)");
    }

    #[test]
    fn displays_dotted_pair() {
        let value = Value::pair(Value::number(1.0), Value::number(2.0));
        assert_eq!(value.show(), "(1 . 2)");
    }

    #[test]
    fn displays_improper_list() {
        let value = Value::pair(
            Value::number(1.0),
            Value::pair(Value::number(2.0), Value::number(3.0)),
        );
        assert_eq!(value.show(), "(1 2 . 3)");
    }

    #[test]
    fn displays_external() {
        let value = Value::list([Value::symbol("call"), Value::external(Prim("add"))]);
        assert_eq!(value.show(), "(call #<primitive add>)");
    }
}
