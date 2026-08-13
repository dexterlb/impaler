use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::evaluator::apply;
use crate::value_list::ValueList;
use crate::values::{Cont, External, Value, ValueItem};

pub fn poly_fix_list(funcs: &ValueList) -> ValueList {
    Rec::build(funcs)
}

struct Rec {
    funcs: ValueList
    rec_refs: ValueList
}

impl Rec {
    fn build(funcs: &ValueList) -> &Self {
        // TODO: funcs -> funcs
        // TODO: rec_refs -> build RecRef objects
    }

    fn apply_ith(&self, i: uint, ret: Cont, arg: ValueList) {
        // TODO: apply ith function to the rec_refs and then apply the result to the arg
    }
}

struct RecRef {
    rec: Rc<Rec>
    func_idx: uint
}

impl External for RecRef {
    fn apply(&self, gret: Cont, garg: ValueList) {
        // TODO: call Rec.apply
    }

    fn show(&self) -> String {
        "#<rec-ref>".to_string()
    }
}
