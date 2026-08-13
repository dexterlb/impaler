use std::rc::Rc;

use crate::evaluator::apply;
use crate::value_list::ValueList;
use crate::values::{Cont, External, Value};

type ListOfCallables = ValueList; // duckily assume that the user has provided values that are callable

// Constructs a list of first-order functions ("funcs") from a list of
// second-order functions ("operators") that each expect to be passed the funcs
// as arguments and produce their respective result function
pub fn poly_fix_list(operators: &ListOfCallables) -> ListOfCallables {
    poly_fix_list_rc(&Rc::new(operators.clone()))
}

pub fn poly_fix_list_rc(operators: &Rc<ListOfCallables>) -> ListOfCallables {
    operators.map(|operator| Rec::new(operators, operator).val())
}

#[derive(Debug)]
struct Rec {
    all_operators: Rc<ListOfCallables>,
    operator: Value,
}

impl Rec {
    fn new(all_operators: &Rc<ListOfCallables>, operator: &Value) -> Self {
        Rec {
            all_operators: all_operators.clone(),
            operator: operator.clone(),
        }
    }

    fn val(self) -> Value {
        Value::external(self)
    }
}

impl External for Rec {
    fn apply(&self, ret: Cont, arg: ValueList) {
        let funcs = poly_fix_list_rc(&self.all_operators);
        let operator = self.operator.clone();

        // first apply the operator to the funcs to obtain a func `f`,
        // then give it to call_func who will call it with the given arg
        let call_func = Rc::new(move |f: Value| apply(ret.clone(), f, arg.clone()));
        apply(call_func, operator, funcs);
    }

    fn show(&self) -> String {
        "#<rec>".to_string()
    }
}
