use std::collections::HashMap;

use crate::async_eval::eval_async;
use crate::env::{Env, EnvExt};
use crate::lambda::mk_lambda;
use crate::parse::parse_value;
use crate::special_form::SpecialForm;
use crate::value_builders::{func_binary, func_cps_binary, func_nary, func_ternary, func_unary};
use crate::value_list::ValueList;
use crate::values::{Value, ValueItem};

// `sources` maps a path to the source text of a file, consulted by
// `read-source` before falling back to the filesystem.
pub fn sandbox_env(sources: HashMap<String, String>) -> Env {
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

    env.insert("quote".to_string(), Value::special_form(SpecialForm::Quote));
    env.insert(
        "macroexpand".to_string(),
        Value::special_form(SpecialForm::MacroExpand),
    );
    env.insert(
        "free-vars".to_string(),
        Value::special_form(SpecialForm::FreeVars),
    );

    env.insert("eval".to_string(), func_cps_binary("eval", do_eval));

    env.insert(
        "mk-lambda".to_string(),
        func_ternary("mk-lambda", mk_lambda),
    );

    env.insert(
        "read-source".to_string(),
        func_unary("read-source", move |path| read_source(&sources, path)),
    );

    env
}

fn sum(args: ValueList) -> Value {
    let mut total = 0.0;
    for arg in args.to_vec() {
        match arg.get() {
            ValueItem::Number(n) => total += *n,
            _ => return Value::err("+: expected number", arg),
        }
    }
    Value::number(total)
}

fn product(args: ValueList) -> Value {
    let mut total = 1.0;
    for arg in args.to_vec() {
        match arg.get() {
            ValueItem::Number(n) => total *= *n,
            _ => return Value::err("*: expected number", arg),
        }
    }
    Value::number(total)
}

fn arithmetic(name: &str, a: Value, b: Value, op: impl Fn(f64, f64) -> f64) -> Value {
    match (a.get(), b.get()) {
        (ValueItem::Number(x), ValueItem::Number(y)) => Value::number(op(*x, *y)),
        (ValueItem::Number(_), _) => Value::err(format!("{}: expected number", name), b),
        _ => Value::err(format!("{}: expected number", name), a),
    }
}

fn compare(name: &str, a: Value, b: Value, op: impl Fn(f64, f64) -> bool) -> Value {
    match (a.get(), b.get()) {
        (ValueItem::Number(x), ValueItem::Number(y)) => Value::boolean(op(*x, *y)),
        (ValueItem::Number(_), _) => Value::err(format!("{}: expected number", name), b),
        _ => Value::err(format!("{}: expected number", name), a),
    }
}

fn car(value: Value) -> Value {
    match value.get() {
        ValueItem::Pair(car, _) => car.clone(),
        _ => Value::err("car: expected pair", value),
    }
}

fn cdr(value: Value) -> Value {
    match value.get() {
        ValueItem::Pair(_, cdr) => cdr.clone(),
        _ => Value::err("cdr: expected pair", value),
    }
}

fn do_eval(ret: &dyn Fn(Value), env_spec: Value, body: Value) {
    match Env::from_val(&env_spec) {
        Some(env) => ret(eval_async(env, body)),
        None => ret(Value::err("eval: invalid environment", env_spec)),
    }
}

fn read_source(sources: &HashMap<String, String>, path: Value) -> Value {
    let path = match path.get() {
        ValueItem::String(path) => path.clone(),
        _ => return Value::err("read-source: expected a string path", path),
    };
    let source = match sources.get(&path) {
        Some(source) => source.clone(),
        None => match std::fs::read_to_string(&path) {
            Ok(source) => source,
            Err(error) => {
                return Value::err(
                    "read-source: cannot read file",
                    Value::string(error.to_string()),
                );
            }
        },
    };
    match parse_value(&source, Some(&path)) {
        Ok(value) => value,
        Err(message) => Value::err("read-source: parse error", Value::string(message)),
    }
}
