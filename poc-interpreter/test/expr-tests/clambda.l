(quote (
    (
        (test "lambda works")
        (input ((!lambda (x y) (cons (quote foo) (cons x (cons y ()))))
            (quote bar) (quote baz)))
        (output (foo bar baz)))
    (
        (test "lambda works (capture args as list)")
        (input ((!lambda args args) (quote foo) (quote bar)))
        (output (foo bar)))
    (
        (test "lambda works (capture some of args as list)")
        (input ((!lambda (foo bar . args) (cons args (cons foo (cons bar ()))))
                (quote foo) (quote bar) (quote baz) (quote qux)))
        (output ((baz qux) foo bar)))
))
