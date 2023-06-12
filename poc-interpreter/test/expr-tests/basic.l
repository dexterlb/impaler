(quote (
    ; (
    ;     (test "bool prefix atoms don't break parser")
    ;     (input (quote #fuck))
    ;     (output #fuck))
    (
        (test "ints parse correctly")
        (input 42)
        (output 42))
    (
        (test "floats parse correctly")
        (input 42.5)
        (output 42.5))
    (
        (test "car works")
        (input (car (cons 'foo 'bar)))
        (output foo))
    (
        (test "cdr works")
        (input (car (cons (quote foo) (quote bar))))
        (output foo))
))
