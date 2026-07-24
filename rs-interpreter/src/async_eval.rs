use std::cell::RefCell;
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll, Waker};

use crate::env::Env;
use crate::evaluator::eval;
use crate::values::{Cont, Value};

pub fn eval_async(env: Env, expr: Value) -> Value {
    let slot: Rc<RefCell<Option<Value>>> = Rc::new(RefCell::new(None));
    let cont: Cont = {
        let slot = slot.clone();
        Rc::new(move |value: Value| *slot.borrow_mut() = Some(value))
    };
    block_on(async move {
        eval(env, cont, expr);
        ResultFuture { slot }.await
    })
}

struct ResultFuture {
    slot: Rc<RefCell<Option<Value>>>,
}

impl Future for ResultFuture {
    type Output = Value;

    fn poll(self: Pin<&mut Self>, _context: &mut Context<'_>) -> Poll<Value> {
        match self.slot.borrow_mut().take() {
            Some(value) => Poll::Ready(value),
            None => Poll::Pending,
        }
    }
}

fn block_on<F: Future>(future: F) -> F::Output {
    let mut future = Box::pin(future);
    let mut context = Context::from_waker(Waker::noop());
    loop {
        if let Poll::Ready(output) = future.as_mut().poll(&mut context) {
            return output;
        }
    }
}
