use std::cell::RefCell;
use std::rc::Rc;

use crate::env::Env;
use crate::evaluator::eval;
use crate::values::{Cont, Value};

pub fn eval_to_value(env: Env, expr: Value) -> Option<Value> {
    let slot: Rc<RefCell<Option<Value>>> = Rc::new(RefCell::new(None));
    let cont: Cont = {
        let slot = slot.clone();
        Rc::new(move |value: Value| *slot.borrow_mut() = Some(value))
    };

    eval(env, cont, expr);

    slot.take()
}
