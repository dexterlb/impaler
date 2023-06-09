(quote (
    (
        (test "clambda works")
        (input ((clambda ret (x y) (ret (cons (quote foo) (cons x (cons y ())))))
            (quote bar) (quote baz)))
        (output (foo bar baz)))
    (
        (test "clambda works (capture args as list)")
        (input ((clambda ret args (ret args)) (quote foo) (quote bar)))
        (output (foo bar)))
    (
        (test "clambda works (capture some of args as list)")
        (input ((clambda ret (foo bar . args) (ret (cons args (cons foo (cons bar ())))))
                (quote foo) (quote bar) (quote baz) (quote qux)))
        (output ((baz qux) foo bar)))
))
