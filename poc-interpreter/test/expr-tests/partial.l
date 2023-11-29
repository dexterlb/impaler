(quote (
    (
        (test "known variables get substituted by their values")
        (input
            (peval (cons (cons 'add (cons add ())) (quote ((x 5)))) (quote
                (add x y))))
        (output (add 5 y)))
    (
        (test "constants get folded")
        (input
            (peval (cons (cons 'add (cons add ())) (quote ())) (quote
                (add (add 2 3) y))))
        (output (add 5 y)))
    (
        (test "lambda substitution")
        (input
            (peval (cons (cons 'lambda (cons lambda ())) (cons (cons 'add (cons add ())) (quote ()))) (quote
                ((!lambda (x) (add x y)) 5))))
        (output (add 5 y)))
    (
        (test "lambda substitution, then constant folding")
        (input
            (peval (cons (cons 'lambda (cons lambda ())) (cons (cons 'add (cons add ())) (quote ()))) (quote
                ((!lambda (x) (add x 2)) 5))))
        (output 7))
    (
        (test "partial lambda substitution")
        (input
            (peval (cons (cons 'lambda (cons lambda ())) (cons (cons 'add (cons add ())) (quote ()))) (quote
                ((!lambda (x y) (add x y)) 5 z))))
        (output ((fixme-lambda (y) (add 5 y)) z)))
))
