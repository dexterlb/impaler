use std::rc::{Rc, Weak};

use crate::evaluator::apply;
use crate::value_list::ValueList;
use crate::values::{Cont, External, Value};

type ListOfCallables = ValueList; // duckily assume that the user has provided values that are callable

// Constructs a list of first-order functions ("funcs") from a list of
// second-order functions ("operators") that each expect to be passed the funcs
// as arguments and produce their respective result function
pub fn poly_fix_list(operators: &ListOfCallables) -> ListOfCallables {
    let rec_refs = Rc::new_cyclic(|weak: &Weak<ListOfCallables>| {
        operators.map(|operator| RecRef::weak(weak, operator.clone()).val())
    });
    operators.map(|operator| RecRef::strong(&rec_refs, operator.clone()).val())
}

#[derive(Debug, Clone)]
enum Rec {
    Strong(Rc<ListOfCallables>),
    Weak(Weak<ListOfCallables>),
}

impl Rec {
    fn funcs(&self) -> Rc<ListOfCallables> {
        match self {
            Rec::Strong(rec_refs) => rec_refs.clone(),
            Rec::Weak(weak) => match weak.upgrade() {
                Some(rec_refs) => rec_refs,
                None => panic!("Rec used after being dropped, this must be impossible!"),
            },
        }
    }
}

#[derive(Debug)]
struct RecRef {
    rec: Rec,
    operator: Value,
}

impl RecRef {
    fn weak(funcs: &Weak<ListOfCallables>, operator: Value) -> RecRef {
        RecRef {
            rec: Rec::Weak(funcs.clone()),
            operator,
        }
    }

    fn strong(funcs: &Rc<ListOfCallables>, operator: Value) -> RecRef {
        RecRef {
            rec: Rec::Strong(funcs.clone()),
            operator,
        }
    }

    fn val(self) -> Value {
        Value::external(self)
    }
}

impl External for RecRef {
    fn apply(&self, ret: Cont, arg: ValueList) {
        let funcs = self.rec.funcs();
        let operator = self.operator.clone();
        apply(
            // first apply the operator to the funcs to obtain a func `f`,
            // then apply f to the given arg
            Rc::new(move |f: Value| apply(ret.clone(), f, arg.clone())),
            operator,
            (*funcs).clone(),
        );
    }

    fn show(&self) -> String {
        "#<rec-ref>".to_string()
    }
}
