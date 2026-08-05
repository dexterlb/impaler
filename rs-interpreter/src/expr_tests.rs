use std::collections::HashMap;

use crate::async_eval::eval_async;
use crate::parse::parse_all;
use crate::sandbox::sandbox_env;
use crate::value_list::ValueList;
use crate::values::{Value, ValueItem};

// see build.rs
include!(concat!(env!("OUT_DIR"), "/ild_generated.rs"));

fn sources() -> HashMap<String, String> {
    ILD_FILES
        .iter()
        .map(|(path, content)| (path.to_string(), content.to_string()))
        .collect()
}

fn file_contents(path: &str) -> &'static str {
    ILD_FILES
        .iter()
        .find(|(p, _)| *p == path)
        .map(|(_, content)| *content)
        .unwrap_or_else(|| panic!("no such ild file: {}", path))
}

fn list_items(value: &Value) -> Vec<Value> {
    ValueList::from_val(value)
        .expect("expected a proper list")
        .to_vec()
}

fn field(entries: &[Value], key: &str) -> Value {
    for entry in entries {
        if let [key_value, value] = list_items(entry).as_slice() {
            if let ValueItem::Symbol(name) = key_value.get() {
                if name == key {
                    return value.clone();
                }
            }
        }
    }
    panic!("missing field `{}` in test case", key);
}

fn run_case(file: &str, case_name: &str) {
    for form in parse_all(file_contents(file)).expect("parse ild file") {
        let parts = list_items(&form);
        if parts.len() < 2 || parts[0] != Value::symbol("case") {
            continue;
        }
        let name = match parts[1].get() {
            ValueItem::String(name) => name,
            _ => continue,
        };
        if name != case_name {
            continue;
        }
        let entries = &parts[2..];
        assert_eq!(
            eval_async(sandbox_env(sources()), field(entries, "expr")),
            field(entries, "expected"),
            "test case `{}` failed",
            case_name
        );
        return;
    }
    panic!("case `{}` not found in {}", case_name, file);
}
