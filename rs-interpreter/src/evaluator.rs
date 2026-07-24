use std::rc::Rc;

use crate::env::{Env, EnvExt};
use crate::values::{Cont, Value};

pub fn eval(env: Env, ret: Cont, expr: Value) {
    match expr {
        Value::Symbol(name) => resume(ret, env.lookup(&name)),
        Value::Pair(cell) => {
            let operator = cell.0.clone();
            let operand = cell.1.clone();
            let continuation: Cont = Rc::new(move |callable: Value| {
                apply(ret.clone(), callable, operand.clone());
            });
            eval(env, continuation, operator);
        }
        other => resume(ret, other),
    }
}

fn apply(ret: Cont, callable: Value, arg: Value) {
    if let Value::ExternalVal(_) = callable {
        callable.apply(ret, arg);
    } else {
        Value::err("cannot apply", callable);
    }
}

fn resume(ret: Cont, value: Value) {
    (&*ret)(value);
}
