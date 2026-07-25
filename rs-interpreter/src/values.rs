use std::fmt;
use std::rc::Rc;

use crate::value_list::ValueList;

pub type Cont = Rc<dyn Fn(Value)>;

pub trait External: fmt::Debug + 'static {
    fn apply(&self, cont: Cont, args: ValueList);
    fn show(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Value {
    Symbol(String),
    Number(f64),
    String(String),
    Bool(bool),
    Pair(Rc<(Value, Value)>),
    Null,

    ExternalVal(Rc<dyn External>),
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

    pub fn boolean(value: bool) -> Value {
        Value::Bool(value)
    }

    pub fn pair(car: Value, cdr: Value) -> Value {
        Value::Pair(Rc::new((car, cdr)))
    }

    pub fn external(external: impl External) -> Value {
        Value::ExternalVal(Rc::new(external))
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
            .fold(Value::Null, |cdr, car| Value::pair(car, cdr))
    }

    pub fn show(&self) -> String {
        match self {
            Value::Symbol(name) => name.clone(),
            Value::Number(x) => format!("{}", x),
            Value::String(s) => format!("{:?}", s),
            Value::Bool(value) => if *value { "#t" } else { "#f" }.to_string(),
            Value::ExternalVal(external) => external.show(),
            Value::Null => "()".to_string(),
            Value::Pair(_) => {
                let mut out = String::from("(");
                let mut current = self;
                let mut first = true;
                loop {
                    match current {
                        Value::Pair(cell) => {
                            if !first {
                                out.push(' ');
                            }
                            first = false;
                            out.push_str(&cell.0.show());
                            current = &cell.1;
                        }
                        Value::Null => break,
                        other => {
                            out.push_str(" . ");
                            out.push_str(&other.show());
                            break;
                        }
                    }
                }
                out.push(')');
                out
            }
        }
    }

    pub fn apply(&self, cont: Cont, args: ValueList) {
        if let Value::ExternalVal(external) = self {
            external.apply(cont, args);
        }
    }

    pub fn func_cps_nary(
        name: impl Into<String>,
        f: impl Fn(&dyn Fn(Value), ValueList) + 'static,
    ) -> Value {
        Value::external(Func {
            name: name.into(),
            f: Box::new(move |cont: Cont, args: ValueList| f(&*cont, args)),
        })
    }

    pub fn func_nary(name: impl Into<String>, f: impl Fn(ValueList) -> Value + 'static) -> Value {
        Value::func_cps_nary(name, move |ret, args| ret(f(args)))
    }

    pub fn func_cps(name: impl Into<String>, f: impl Fn(&dyn Fn(Value), Value) + 'static) -> Value {
        Value::func_cps_nary(name, move |ret, args| match args.to_array::<1>() {
            Some([a]) => f(ret, a),
            None => ret(Value::Null),
        })
    }

    pub fn func(name: impl Into<String>, f: impl Fn(Value) -> Value + 'static) -> Value {
        Value::func_cps(name, move |ret, arg| ret(f(arg)))
    }

    pub fn func_unary(name: impl Into<String>, f: impl Fn(Value) -> Value + 'static) -> Value {
        Value::func_nary(name, move |args| match args.to_array::<1>() {
            Some([a]) => f(a),
            None => Value::Null,
        })
    }

    pub fn func_binary(
        name: impl Into<String>,
        f: impl Fn(Value, Value) -> Value + 'static,
    ) -> Value {
        Value::func_nary(name, move |args| match args.to_array::<2>() {
            Some([a, b]) => f(a, b),
            None => Value::Null,
        })
    }

    pub fn func_ternary(
        name: impl Into<String>,
        f: impl Fn(Value, Value, Value) -> Value + 'static,
    ) -> Value {
        Value::func_nary(name, move |args| match args.to_array::<3>() {
            Some([a, b, c]) => f(a, b, c),
            None => Value::Null,
        })
    }
}

struct Func {
    name: String,
    f: Box<dyn Fn(Cont, ValueList)>,
}

impl fmt::Debug for Func {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "#<function {}>", self.name)
    }
}

impl External for Func {
    fn apply(&self, cont: Cont, args: ValueList) {
        (self.f)(cont, args)
    }

    fn show(&self) -> String {
        format!("#<function {}>", self.name)
    }
}

#[cfg(test)]
impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Pair(a), Value::Pair(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::ExternalVal(a), Value::ExternalVal(b)) => Rc::ptr_eq(a, b),
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
        assert_eq!(Value::Null.show(), "()");
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
