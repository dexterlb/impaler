(quote (
    (
        (test "protolambda works")
        (input ((eval (getenv) (protolambda (getenv) (quote (x y)) (quote (cons (quote foo) (cons x (cons y ()))))))
            (quote bar) (quote baz)))
        (output (foo bar baz)))
    (
        (test "nested protolambda works")
        (input (
            (eval (getenv) (protolambda (getenv) (quote (x y))
                (quote (
                    (eval (getenv) (protolambda (getenv) (quote (z))
                        (quote (cons (quote foo) (cons x (cons y (cons z ())))))
                    ))
                    (quote qux)))
            ))
            (quote bar) (quote baz)))
        (output (foo bar baz qux)))
    (
        (test "protolambda works (capture args as list)")
        (input ((eval (getenv) (protolambda (getenv) (quote args) (quote (cons (quote foo) args))))
            (quote bar) (quote baz)))
        (output (foo bar baz)))
    (
        (test "protolambda works (capture some of args as list)")
        (input ((eval (getenv) (protolambda (getenv) (quote (foo bar . args)) (quote (cons args (cons foo (cons bar ()))))))
                (quote foo) (quote bar) (quote baz) (quote qux)))
        (output ((baz qux) foo bar)))
))
