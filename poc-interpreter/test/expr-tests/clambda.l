(quote (
    (
        (test "lambda-cps works")
        (input ((lambda-cps ret (x y) (ret (cons (quote foo) (cons x (cons y ())))))
            (quote bar) (quote baz)))
        (output (foo bar baz)))
    (
        (test "lambda-cps works (capture args as list)")
        (input ((lambda-cps ret args (ret args)) (quote foo) (quote bar)))
        (output (foo bar)))
    (
        (test "lambda-cps works (capture some of args as list)")
        (input ((lambda-cps ret (foo bar . args) (ret (cons args (cons foo (cons bar ())))))
                (quote foo) (quote bar) (quote baz) (quote qux)))
        (output ((baz qux) foo bar)))
))
