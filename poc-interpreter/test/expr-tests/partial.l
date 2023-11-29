(quote (
    (
        (test "known variables get substituted by their values (trivial case)")
        (input
            (peval (quote ((x . 5))) (quote
                x)))
        (output 5))
    (
        (test "unknown variables are untouched (trivial case)")
        (input
            (peval (quote ((x . 5))) (quote
                y)))
        (output y))
    (
        (test "known variables get substituted by their values")
        (input
            (peval (cons (cons 'add add) (quote ((x . 5)))) (quote
                (add x y))))
        (output (add 5 y)))
    (
        (test "constants get folded")
        (input
            (peval (cons (cons 'add add) (quote ())) (quote
                (add (add 2 3) y))))
        (output (add 5 y)))
    (
        (test "lambda substitution")
        (input
            (peval (cons (cons 'macroexpand macroexpand) (cons (cons 'lambda lambda) (cons (cons 'add add) (quote ())))) (quote
                ((!lambda (x) (add x y)) 5))))
        (output (add 5 y)))
    (
        (test "lambda substitution, then constant folding")
        (input
            (peval (cons (cons 'macroexpand macroexpand) (cons (cons 'lambda lambda) (cons (cons 'add add) (quote ())))) (quote
                ((!lambda (x) (add x 2)) 5))))
        (output 7))
    (
        (test "partial lambda substitution")
        (input
            (peval (cons (cons 'macroexpand macroexpand) (cons (cons 'lambda lambda) (cons (cons 'add add) (quote ())))) (quote
                ((!lambda (x y) (add x y)) 5 z))))
        (output ((fixme-lambda (y) (add 5 y)) z)))
))
