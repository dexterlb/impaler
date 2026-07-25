use crate::value_list::ValueList;
use crate::values::Value;

#[derive(Debug, Clone)]
pub enum SpecialForm {
    Quote,
}

impl SpecialForm {
    pub fn show(&self) -> String {
        match self {
            SpecialForm::Quote => "#<special-form quote>".to_string(),
        }
    }

    pub fn apply(&self, args: ValueList) -> Value {
        match self {
            SpecialForm::Quote => Self::apply_quote(args),
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
}
