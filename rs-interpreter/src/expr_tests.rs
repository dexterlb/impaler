use crate::async_eval::eval_async;
use crate::parse::parse_value;
use crate::sandbox::sandbox_env;
use crate::values::Value;

fn eval_str(source: &str) -> Value {
    let env = sandbox_env();
    let expr = parse_value(source).expect("parse");
    eval_async(env, expr)
}

macro_rules! eval_tests {
    ($($name:ident: $input:expr => $expected:expr;)*) => {
        $(
            #[test]
            fn $name() {
                assert_eq!(eval_str($input), eval_str($expected));
            }
        )*
    };
}

eval_tests! {
    adds: "(+ 2 3)" => "5";
    subtracts: "(- 10 4)" => "6";
    multiplies: "(* 2 3)" => "6";
    less_than_true: "(< 1 2)" => "#t";
    less_than_false: "(< 2 1)" => "#f";
    greater_than: "(> 2 1)" => "#t";
    equal_true: "(= 2 2)" => "#t";
    equal_false: "(= 2 3)" => "#f";
    nested_arithmetic: "(+ (+ 1 1) 3)" => "5";
    nested_predicate: "(< (- 5 4) 3)" => "#t";
}
