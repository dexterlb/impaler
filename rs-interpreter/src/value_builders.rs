use std::fmt;

use crate::value_list::ValueList;
use crate::values::{Cont, External, Value};

pub fn func_cps_nary(
    name: impl Into<String>,
    f: impl Fn(&dyn Fn(Value), ValueList) + 'static,
) -> Value {
    Value::external(Func {
        name: name.into(),
        f: Box::new(move |cont: Cont, args: ValueList| f(&*cont, args)),
    })
}

// like func_cps_nary, but converts the Value to a Cont before passing
pub fn func_cont_nary(name: impl Into<String>, f: impl Fn(Cont, ValueList) + 'static) -> Value {
    Value::external(Func {
        name: name.into(),
        f: Box::new(f),
    })
}

pub fn func_cont_binary(
    name: impl Into<String>,
    f: impl Fn(Cont, Value, Value) + 'static,
) -> Value {
    func_cont_nary(name, move |cont, args| match args.to_array::<2>() {
        Some([a, b]) => f(cont, a, b),
        None => (&*cont)(Value::err("expected two arguments", args.to_value())),
    })
}

pub fn func_nary(name: impl Into<String>, f: impl Fn(ValueList) -> Value + 'static) -> Value {
    func_cps_nary(name, move |ret, args| ret(f(args)))
}

pub fn func_cps(name: impl Into<String>, f: impl Fn(&dyn Fn(Value), Value) + 'static) -> Value {
    func_cps_nary(name, move |ret, args| match args.to_array::<1>() {
        Some([a]) => f(ret, a),
        None => ret(Value::err("expected one argument", args.to_value())),
    })
}

pub fn func(name: impl Into<String>, f: impl Fn(Value) -> Value + 'static) -> Value {
    func_cps(name, move |ret, arg| ret(f(arg)))
}

pub fn func_cps_binary(
    name: impl Into<String>,
    f: impl Fn(&dyn Fn(Value), Value, Value) + 'static,
) -> Value {
    func_cps_nary(name, move |ret, args| match args.to_array::<2>() {
        Some([a, b]) => f(ret, a, b),
        None => ret(Value::err("expected two arguments", args.to_value())),
    })
}

pub fn func_unary(name: impl Into<String>, f: impl Fn(Value) -> Value + 'static) -> Value {
    func_nary(name, move |args| match args.to_array::<1>() {
        Some([a]) => f(a),
        None => Value::err("expected one argument", args.to_value()),
    })
}

pub fn func_binary(name: impl Into<String>, f: impl Fn(Value, Value) -> Value + 'static) -> Value {
    func_nary(name, move |args| match args.to_array::<2>() {
        Some([a, b]) => f(a, b),
        None => Value::err("expected two arguments", args.to_value()),
    })
}

pub fn func_ternary(
    name: impl Into<String>,
    f: impl Fn(Value, Value, Value) -> Value + 'static,
) -> Value {
    func_nary(name, move |args| match args.to_array::<3>() {
        Some([a, b, c]) => f(a, b, c),
        None => Value::err("expected three arguments", args.to_value()),
    })
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
