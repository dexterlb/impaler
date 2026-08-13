use std::cell::RefCell;
use std::rc::Rc;

use crate::evaluator::apply;
use crate::value_list::ValueList;
use crate::values::{Cont, External, Value};

type ListOfCallables = ValueList;
type Slot = Rc<RefCell<Option<Value>>>;

// Ties the knot like a Scheme letrec: allocate a blank slot per operator,
// evaluate each operator once against the shared list of slot-backed funcs,
// then mutate the slots to hold the results. The recursive closures capture
// the funcs, whose slots point back at those same closures. THIS IS A MEMORY LEAK.
// a future version needs to have a proper GC or a clever way to drop the slots.
pub fn poly_fix_list(operators: &ListOfCallables) -> ListOfCallables {
    let operators = operators.to_vec();
    let slots: Vec<Slot> = operators.iter().map(|_| Rc::new(RefCell::new(None))).collect();

    let mut funcs = ValueList::empty();
    for slot in slots.iter().rev() {
        funcs = funcs.push(Value::external(Rec { slot: slot.clone() }));
    }

    for (operator, slot) in operators.iter().zip(&slots) {
        let slot = slot.clone();
        apply(
            Rc::new(move |f: Value| *slot.borrow_mut() = Some(f)),
            operator.clone(),
            funcs.clone(),
        );
    }

    funcs
}

#[derive(Debug)]
struct Rec {
    slot: Slot,
}

impl External for Rec {
    fn apply(&self, ret: Cont, arg: ValueList) {
        match self.slot.borrow().clone() {
            Some(f) => apply(ret, f, arg),
            None => (&*ret)(Value::err("infinite-recursion", arg.to_value())),
        }
    }

    fn show(&self) -> String {
        "#<rec>".to_string()
    }
}
