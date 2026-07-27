use crate::env::{Env, EnvExt};
use crate::value_list::ValueList;
use crate::values::Value;

#[derive(Debug, Clone)]
pub enum SpecialForm {
    Quote,
    FreeVars,
}

impl SpecialForm {
    pub fn show(&self) -> String {
        match self {
            SpecialForm::Quote => "#<special-form quote>".to_string(),
            SpecialForm::FreeVars => "#<special-form free-vars>".to_string(),
        }
    }

    pub fn apply(&self, env: Env, args: ValueList) -> Value {
        match self {
            SpecialForm::Quote => Self::apply_quote(args),
            SpecialForm::FreeVars => Self::apply_free_vars(env),
        }
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
