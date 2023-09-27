(module
  (doc "this module calculates factorials :)")
  (exports fact)
  (imports
    (builtin (lambda macroexpand quote mul add))
    ("libs/core.l" (if))
    )
  (defs
    '(fact (!lambda (x)
      (!if (<= x 0)
        1
        (mul x (fact (add x -1))))))
  ))
