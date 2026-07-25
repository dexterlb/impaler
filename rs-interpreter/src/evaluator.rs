use std::rc::Rc;

use crate::env::{Env, EnvExt};
use crate::value_list::{NonEmptyValueList, ValueList};
use crate::values::{Cont, Value};

pub fn eval(env: Env, ret: Cont, expr: Value) {
    if let Some(items) = NonEmptyValueList::from_val(&expr) {
        eval_combination(env, ret, items);
    } else {
        resume(ret, eval_simple_expr(env, expr));
    }
}

fn eval_combination(env: Env, ret: Cont, items: NonEmptyValueList) {
    eval_all_and_then(
        env,
        items,
        Rc::new(move |results: NonEmptyValueList| {
            apply(ret.clone(), results.head.clone(), results.tail.clone());
        }),
    );
}

fn apply(ret: Cont, callable: Value, args: ValueList) {
    if let Value::ExternalVal(_) = callable {
        callable.apply(ret, args);
    } else {
        resume(ret, Value::err("cannot apply", callable));
    }
}

fn eval_simple_expr(env: Env, expr: Value) -> Value {
    match expr {
        Value::Symbol(name) => env.lookup(&name),
        Value::Null => Value::err(
            "trying to evaluate Null - did you forget to quote it?",
            Value::Null,
        ),
        pair @ Value::Pair(_) => Value::err(
            "refusing to evaluate pair; use apply or quote explicitly",
            pair,
        ),
        other => other, // everything else evaluates as itself
    }
}

fn eval_all_and_then(env: Env, exprs: NonEmptyValueList, cont: Rc<dyn Fn(NonEmptyValueList)>) {
    eval_all_and_then_loop(
        env,
        exprs.into_list(),
        ValueList::empty(),
        Rc::new(move |results: ValueList| {
            let results = NonEmptyValueList::from_list(results)
                .expect("evaluating a non-empty list must yield a non-empty list; this is a bug in eval_all_and_then_loop");
            (&*cont)(results);
        }),
    );
}

fn eval_all_and_then_loop(
    env: Env,
    exprs: ValueList,
    done: ValueList,
    cont: Rc<dyn Fn(ValueList)>,
) {
    match exprs.split() {
        None => (&*cont)(done.reverse()),
        Some((first, rest)) => {
            let first = first.clone();
            let rest = rest.clone();
            let env_for_rest = env.clone();
            eval(
                env,
                Rc::new(move |value: Value| {
                    let done = done.push(value);
                    eval_all_and_then_loop(env_for_rest.clone(), rest.clone(), done, cont.clone());
                }),
                first,
            );
        }
    }
}

fn resume(ret: Cont, value: Value) {
    (&*ret)(value);
}
