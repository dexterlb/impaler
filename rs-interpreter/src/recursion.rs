use std::rc::Rc;

use crate::evaluator::apply;
use crate::value_list::ValueList;
use crate::values::{Cont, External, Value};

pub fn poly_fix_list(funcs: &ValueList) -> ValueList {
    Rec::build(funcs)
}

#[derive(Debug)]
struct Rec {
    funcs: ValueList,
}

impl Rec {
    fn build(funcs: &ValueList) -> ValueList {
        Rc::new(Rec {
            funcs: funcs.clone(),
        })
        .rec_refs()
    }

    fn rec_refs(self: &Rc<Self>) -> ValueList {
        let rec = self.clone();
        self.funcs.map(|func| {
            Value::external(RecRef {
                rec: rec.clone(),
                func: func.clone(),
            })
        })
    }
}

#[derive(Debug)]
struct RecRef {
    rec: Rc<Rec>,
    func: Value,
}

impl External for RecRef {
    fn apply(&self, gret: Cont, garg: ValueList) {
        let rec_refs = self.rec.rec_refs();
        apply(
            Rc::new(move |g: Value| apply(gret.clone(), g, garg.clone())),
            self.func.clone(),
            rec_refs,
        );
    }

    fn show(&self) -> String {
        "#<rec-ref>".to_string()
    }
}
