(module
  (doc "this module calculates factorials :)")
  (exports main)
  (imports
    (builtin (lambda macroexpand quote <= mul add))
    ("libs/core.l" (if))
    )
  (defs
    '(main (!lambda () (fact 5)))

    '(fact (!lambda (x)
      (!if (<= x 0)
        1
        (mul x (fact (add x -1))))))
  ))
