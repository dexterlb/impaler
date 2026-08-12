use std::rc::Rc;

use crate::evaluator::apply;
use crate::value_list::ValueList;
use crate::values::{Cont, External, Value};

pub fn poly_fix_list(funcs: &ValueList) -> ValueList {
    funcs.map(|f| Value::external(tie(f.clone(), funcs.clone())))
}

fn tie(f: Value, funcs: ValueList) -> Rec {
    Rec { f, funcs }
}

#[derive(Debug)]
struct Rec {
    f: Value,
    funcs: ValueList,
}

impl External for Rec {
    fn apply(&self, gret: Cont, garg: ValueList) {
        let gs_list = tie_all(&self.funcs);
        let call_with_args: Cont = Rc::new(move |g: Value| apply(gret.clone(), g, garg.clone()));
        apply(call_with_args, self.f.clone(), gs_list);
    }

    fn show(&self) -> String {
        "#<poly-fix>".to_string()
    }
}
