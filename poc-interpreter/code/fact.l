(module
  (doc "this module calculates factorials :)")
  (exports fact)
  (imports
    (builtin (car cdr cons lambda macroexpand quote))
    ("libs/core.l" (if))
    )
  (defs
    '(fact (!lambda (x)
      (!if (<= x 0)
        1
        (mul x (fact (add x -1))))))
  ))
