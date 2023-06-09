(quote (
    (
        (test "letrec works (simple)")
        (input
            (letrec (
                (foo 1)
                (bar 2))

                (cons foo bar)))

        (output (1 . 2)))
    (
        (test "letrec works (transitive)")
        (input
            (letrec (
                (foo 1)
                (bar (cons foo 2)))

                (cons foo bar)))

        (output (1 . 2)))
    (
        (test "letrec works (recursive)")
        (input unimplemented)

        (output 120))
))
