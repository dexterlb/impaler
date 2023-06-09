(quote (
    (
        (test "car works")
        (input (car (cons (quote foo) (quote bar))))
        (output foo))
    (
        (test "cdr works")
        (input (car (cons (quote foo) (quote bar))))
        (output foo))
))
