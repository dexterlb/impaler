use std::rc::Rc;

use crate::values::Value;

#[derive(Debug, Clone)]
pub enum ValueList {
    Cons(Rc<(Value, ValueList)>),
    Empty,
}

impl ValueList {
    pub fn empty() -> ValueList {
        ValueList::Empty
    }

    pub fn push(&self, head: Value) -> ValueList {
        ValueList::Cons(Rc::new((head, self.clone())))
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, ValueList::Empty)
    }

    pub fn split(&self) -> Option<(&Value, &ValueList)> {
        match self {
            ValueList::Cons(cell) => Some((&cell.0, &cell.1)),
            ValueList::Empty => None,
        }
    }

    pub fn reverse(&self) -> ValueList {
        let mut out = ValueList::Empty;
        let mut current = self;
        while let ValueList::Cons(cell) = current {
            out = out.push(cell.0.clone());
            current = &cell.1;
        }
        out
    }

    pub fn to_vec(&self) -> Vec<Value> {
        let mut out = Vec::new();
        let mut current = self;
        while let ValueList::Cons(cell) = current {
            out.push(cell.0.clone());
            current = &cell.1;
        }
        out
    }

    pub fn to_array<const N: usize>(&self) -> Option<[Value; N]> {
        <[Value; N]>::try_from(self.to_vec()).ok()
    }

    pub fn from_val(value: &Value) -> Option<ValueList> {
        match value {
            Value::Null => Some(ValueList::Empty),
            Value::Pair(cell) => Some(ValueList::from_val(&cell.1)?.push(cell.0.clone())),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NonEmptyValueList {
    pub head: Value,
    pub tail: ValueList,
}

impl NonEmptyValueList {
    pub fn from_val(value: &Value) -> Option<NonEmptyValueList> {
        match value {
            Value::Pair(cell) => Some(NonEmptyValueList {
                head: cell.0.clone(),
                tail: ValueList::from_val(&cell.1)?,
            }),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_val_collects_proper_list() {
        let value = Value::list([Value::number(1.0), Value::number(2.0)]);
        let items = ValueList::from_val(&value).expect("proper list");
        assert_eq!(items.to_vec(), vec![Value::number(1.0), Value::number(2.0)]);
    }

    #[test]
    fn from_val_empty_is_some() {
        let items = ValueList::from_val(&Value::Null).expect("empty list");
        assert!(items.is_empty());
    }

    #[test]
    fn from_val_rejects_improper() {
        let value = Value::pair(Value::number(1.0), Value::number(2.0));
        assert!(ValueList::from_val(&value).is_none());
    }

    #[test]
    fn from_val_rejects_atom() {
        assert!(ValueList::from_val(&Value::number(1.0)).is_none());
    }

    #[test]
    fn non_empty_from_val_rejects_empty() {
        assert!(NonEmptyValueList::from_val(&Value::Null).is_none());
    }

    #[test]
    fn non_empty_from_val_splits_head_and_tail() {
        let value = Value::list([Value::symbol("f"), Value::number(1.0)]);
        let list = NonEmptyValueList::from_val(&value).expect("non-empty list");
        assert_eq!(list.head, Value::symbol("f"));
        assert_eq!(list.tail.to_vec(), vec![Value::number(1.0)]);
    }
}
