use std::rc::Rc;

use crate::env::Env;
use crate::value_list::{NonEmptyValueList, ValueList};
use crate::values::{Cont, Value, ValueItem};

pub fn eval(env: Env, ret: Cont, expr: Value) {
    if let Some(items) = NonEmptyValueList::from_val(&expr) {
        eval_combination(env, ret, items, eval_args_and_apply);
    } else {
        resume(ret, eval_simple_expr(env, expr));
    }
}

pub(crate) fn eval_combination(
    env: Env,
    ret: Cont,
    items: NonEmptyValueList,
    apply_fn: impl Fn(Env, Cont, Value, ValueList) + 'static,
) {
    let NonEmptyValueList { head, tail } = items;
    let env_for_args = env.clone();
    eval(
        env,
        Rc::new(move |callable: Value| {
            apply_fn(env_for_args.clone(), ret.clone(), callable, tail.clone());
        }),
        head,
    );
}

pub(crate) fn eval_args_and_apply(
    env: Env,
    ret: Cont,
    callable: Value,
    unevaluated_args: ValueList,
) {
    match callable.get() {
        ValueItem::SpecialForm(form) => form.apply(env, ret, unevaluated_args),
        _ => eval_all_and_then(
            env,
            unevaluated_args,
            Rc::new(move |args: ValueList| {
                apply(ret.clone(), callable.clone(), args);
            }),
        ),
    }
}

pub(crate) fn apply(ret: Cont, callable: Value, args: ValueList) {
    match callable.get() {
        ValueItem::ExternalVal(ext) => ext.apply(ret, args),
        _ => resume(ret, Value::err("cannot apply", callable)),
    }
}

fn eval_simple_expr(env: Env, expr: Value) -> Value {
    match expr.get() {
        ValueItem::Symbol(name) => match env.get(name) {
            Some(value) => value.clone(),
            None => Value::err("unbound symbol", expr.clone()),
        },
        ValueItem::Null => Value::err(
            "trying to evaluate Null - did you forget to quote it?",
            expr.clone(),
        ),
        ValueItem::Pair(..) => Value::err(
            "refusing to evaluate pair; use apply or quote explicitly",
            expr.clone(),
        ),
        _ => expr, // everything else evaluates as itself
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
