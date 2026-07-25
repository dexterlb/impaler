use std::rc::Rc;

use crate::env::{Env, EnvExt};
use crate::evaluator::eval;
use crate::value_list::ValueList;
use crate::values::{Cont, External, Value};

#[derive(Debug)]
struct Lambda {
    closure: Env,
    arg_names: ArgSpec,
    body: Value,
}

#[derive(Debug)]
enum ArgSpec {
    // specifies a list of formal parameters; may not be a well-formed
    // list if capturing the "rest of the arguments" in a nested list
    Cons(Rc<(String, ArgSpec)>),
    Wildcard(String),
    Empty,
}

impl ArgSpec {
    fn from_val(value: &Value) -> Option<ArgSpec> {
        match value {
            Value::Null => Some(ArgSpec::Empty),
            Value::Symbol(name) => Some(ArgSpec::Wildcard(name.clone())),
            Value::Pair(cell) => match &cell.0 {
                Value::Symbol(name) => Some(ArgSpec::Cons(Rc::new((
                    name.clone(),
                    ArgSpec::from_val(&cell.1)?,
                )))),
                _ => None,
            },
            _ => None,
        }
    }

    fn bind(&self, env: &mut Env, args: &[Value]) {
        match self {
            ArgSpec::Empty => {}
            ArgSpec::Wildcard(name) => {
                env.insert(name.clone(), Value::list(args.to_vec()));
            }
            ArgSpec::Cons(cell) => match args.split_first() {
                Some((first, rest)) => {
                    env.insert(cell.0.clone(), first.clone());
                    cell.1.bind(env, rest);
                }
                None => {
                    env.insert(cell.0.clone(), Value::Null);
                    cell.1.bind(env, &[]);
                }
            },
        }
    }

    fn show(&self) -> String {
        let mut out = String::from("(");
        let mut current = self;
        let mut first = true;
        loop {
            match current {
                ArgSpec::Empty => break,
                ArgSpec::Wildcard(name) => {
                    out.push_str(&format!(" . {}", name));
                    break;
                }
                ArgSpec::Cons(cell) => {
                    if !first {
                        out.push(' ');
                    }
                    first = false;
                    out.push_str(&cell.0);
                    current = &cell.1;
                }
            }
        }
        out.push(')');
        out
    }
}

impl External for Lambda {
    fn apply(&self, ret: Cont, args: ValueList) {
        let env = bind_args(&self.closure, &self.arg_names, args);
        eval(env, ret, self.body.clone());
    }

    fn show(&self) -> String {
        format!(
            "#<lambda {}: {} | capturing {}>",
            self.arg_names.show(),
            self.body.show(),
            show_env(&self.closure),
        )
    }
}

fn bind_args(closure: &Env, arg_names: &ArgSpec, args: ValueList) -> Env {
    let mut env = closure.clone();
    arg_names.bind(&mut env, &args.to_vec());
    env
}

fn show_env(env: &Env) -> String {
    let mut entries: Vec<String> = env
        .iter()
        .map(|(name, value)| format!("{}: {}", name, value.show()))
        .collect();
    entries.sort();
    format!("{{{}}}", entries.join(", "))
}

pub fn mk_lambda(closure_v: Value, arg_names_v: Value, body: Value) -> Value {
    let closure = match Env::from_val(&closure_v) {
        Some(env) => env,
        None => return Value::err("mk-lambda: invalid closure", closure_v),
    };
    let arg_names = match ArgSpec::from_val(&arg_names_v) {
        Some(spec) => spec,
        None => return Value::err("mk-lambda: invalid argument list", arg_names_v),
    };
    Value::external(Lambda {
        closure,
        arg_names,
        body,
    })
}
