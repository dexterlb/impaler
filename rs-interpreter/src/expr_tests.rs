use crate::async_eval::eval_async;
use crate::parse::parse_value;
use crate::sandbox::sandbox_env;
use crate::value_list::ValueList;
use crate::values::Value;

fn list_items(value: &Value) -> Vec<Value> {
    ValueList::from_val(value)
        .expect("expected a proper list")
        .to_vec()
}

// Look up `(key value)` among a case's `(expr ...) (expected ...)` entries.
fn field(entries: &[Value], key: &str) -> Value {
    for entry in entries {
        if let [Value::Symbol(name), value] = list_items(entry).as_slice() {
            if name == key {
                return value.clone();
            }
        }
    }
    panic!("missing field `{}` in test case", key);
}

// Runs a test file of the form:
//   (tests
//     (case "name" (expr <expr>) (expected <value>))
//     ...)
fn run_test_file(source: &str) {
    let mut items = list_items(&parse_value(source).expect("parse test file"));
    let tag = items.remove(0);
    assert_eq!(tag, Value::symbol("tests"), "file must start with `tests`");

    for case in items {
        let parts = list_items(&case);
        let name = match &parts[1] {
            Value::String(name) => name.clone(),
            other => panic!("case name must be a string, got {}", other.show()),
        };
        let entries = &parts[2..];
        let expr = field(entries, "expr");
        let expected = field(entries, "expected");
        assert_eq!(
            eval_async(sandbox_env(), expr),
            expected,
            "test case `{}` failed",
            name
        );
    }
}

#[test]
fn simple() {
    run_test_file(include_str!("../../ild-code/expr-tests/simple.ild"));
}

#[test]
fn special_form() {
    run_test_file(include_str!("../../ild-code/expr-tests/special-form.ild"));
}

#[test]
fn lambda() {
    run_test_file(include_str!("../../ild-code/expr-tests/lambda.ild"));
}
