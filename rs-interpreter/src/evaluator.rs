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
    let NonEmptyValueList { head, tail } = items;
    let env_for_args = env.clone();
    eval(
        env,
        Rc::new(move |callable: Value| {
            apply_bare(env_for_args.clone(), ret.clone(), callable, tail.clone());
        }),
        head,
    );
}

fn apply_bare(env: Env, ret: Cont, callable: Value, unevaluated_args: ValueList) {
    match callable {
        Value::SpecialForm(form) => resume(ret, form.apply(unevaluated_args)),
        _ => eval_all_and_then(
            env,
            unevaluated_args,
            Rc::new(move |args: ValueList| {
                apply(ret.clone(), callable.clone(), args);
            }),
        ),
    }
}

fn apply(ret: Cont, callable: Value, args: ValueList) {
    match callable {
        Value::ExternalVal(ext) => ext.apply(ret, args),
        other => resume(ret, Value::err("cannot apply", other)),
    }
}

fn eval_simple_expr(env: Env, expr: Value) -> Value {
    match expr {
        Value::Symbol(name) => env.lookup(&name),
        Value::Null => Value::err(
            "trying to evaluate Null - did you forget to quote it?",
            expr,
        ),
        Value::Pair(_) => Value::err(
            "refusing to evaluate pair; use apply or quote explicitly",
            expr,
        ),
        other => other, // everything else evaluates as itself
    }
}

fn eval_all_and_then(env: Env, exprs: ValueList, cont: Rc<dyn Fn(ValueList)>) {
    eval_all_and_then_loop(env, exprs, ValueList::empty(), cont);
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
