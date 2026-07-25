use crate::env::Env;
use crate::special_form::SpecialForm;
use crate::value_builders::{func_binary, func_nary, func_unary};
use crate::value_list::ValueList;
use crate::values::Value;

pub fn sandbox_env() -> Env {
    let mut env = Env::new();

    env.insert("+".to_string(), func_nary("+", sum));
    env.insert("*".to_string(), func_nary("*", product));
    env.insert(
        "-".to_string(),
        func_binary("-", |a, b| arithmetic("-", a, b, |x, y| x - y)),
    );
    env.insert(
        "/".to_string(),
        func_binary("/", |a, b| arithmetic("/", a, b, |x, y| x / y)),
    );

    env.insert(
        "=".to_string(),
        func_binary("=", |a, b| compare("=", a, b, |x, y| x == y)),
    );
    env.insert(
        "<".to_string(),
        func_binary("<", |a, b| compare("<", a, b, |x, y| x < y)),
    );
    env.insert(
        ">".to_string(),
        func_binary(">", |a, b| compare(">", a, b, |x, y| x > y)),
    );

    env.insert("cons".to_string(), func_binary("cons", Value::pair));
    env.insert("car".to_string(), func_unary("car", car));
    env.insert("cdr".to_string(), func_unary("cdr", cdr));

    env.insert("quote".to_string(), Value::SpecialForm(SpecialForm::Quote));

    env
}

fn sum(args: ValueList) -> Value {
    let mut total = 0.0;
    for arg in args.to_vec() {
        match arg {
            Value::Number(n) => total += n,
            other => return Value::err("+: expected number", other),
        }
    }
    Value::number(total)
}

fn product(args: ValueList) -> Value {
    let mut total = 1.0;
    for arg in args.to_vec() {
        match arg {
            Value::Number(n) => total *= n,
            other => return Value::err("*: expected number", other),
        }
    }
    Value::number(total)
}

fn arithmetic(name: &str, a: Value, b: Value, op: impl Fn(f64, f64) -> f64) -> Value {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => Value::number(op(x, y)),
        (Value::Number(_), other) => Value::err(format!("{}: expected number", name), other),
        (other, _) => Value::err(format!("{}: expected number", name), other),
    }
}

fn compare(name: &str, a: Value, b: Value, op: impl Fn(f64, f64) -> bool) -> Value {
    match (a, b) {
        (Value::Number(x), Value::Number(y)) => Value::boolean(op(x, y)),
        (Value::Number(_), other) => Value::err(format!("{}: expected number", name), other),
        (other, _) => Value::err(format!("{}: expected number", name), other),
    }
}

fn car(value: Value) -> Value {
    match value {
        Value::Pair(cell) => cell.0.clone(),
        other => Value::err("car: expected pair", other),
    }
}

fn cdr(value: Value) -> Value {
    match value {
        Value::Pair(cell) => cell.1.clone(),
        other => Value::err("cdr: expected pair", other),
    }
}
