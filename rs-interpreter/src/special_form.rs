use std::rc::Rc;

use crate::env::{Env, EnvExt};
use crate::evaluator::{apply, eval, eval_combination};
use crate::value_list::{NonEmptyValueList, ValueList};
use crate::values::{Cont, Value};

#[derive(Debug, Clone)]
pub enum SpecialForm {
    Quote,
    FreeVars,
    MacroExpand,
}

impl SpecialForm {
    pub fn show(&self) -> String {
        match self {
            SpecialForm::Quote => "#<special-form quote>".to_string(),
            SpecialForm::FreeVars => "#<special-form free-vars>".to_string(),
            SpecialForm::MacroExpand => "#<special-form macroexpand>".to_string(),
        }
    }

    pub fn apply(&self, env: Env, ret: Cont, args: ValueList) {
        match self {
            SpecialForm::Quote => (&*ret)(Self::apply_quote(args)),
            SpecialForm::FreeVars => (&*ret)(Self::apply_free_vars(env)),
            SpecialForm::MacroExpand => Self::apply_macroexpand(env, ret, args),
        }
    }

    fn apply_macroexpand(env: Env, ret: Cont, args: ValueList) {
        let (macro_expr, macro_args) = match args.split() {
            Some((head, tail)) => (head.clone(), tail.clone()),
            None => {
                (&*ret)(Value::err("macroexpand: missing macro", Value::Null));
                return;
            }
        };
        let items = NonEmptyValueList {
            head: macro_expr,
            tail: macro_args,
        };
        let eval_expansion: Cont = {
            let env = env.clone();
            Rc::new(move |expansion: Value| eval(env.clone(), ret.clone(), expansion))
        };
        eval_combination(env, eval_expansion, items, |_env, ret, callable, args| {
            apply(ret, callable, args)
        });
    }

    fn apply_quote(args: ValueList) -> Value {
        match args.to_array::<1>() {
            Some([arg]) => arg,
            None => Value::err(
                "quote expects exactly one argument",
                Value::list(args.to_vec()),
            ),
        }
    }

    fn apply_free_vars(env: Env) -> Value {
        env.to_val()
    }
}
