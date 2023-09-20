(module
  (doc "this is a test module")
  (exports main)
  (imports
    (builtin (car cdr cons lambda macroexpand quote))
    ("fact.l" (fact))
    )
  (defs
    ; (!const answer (!lambda () 42))
    ; '(const (!lambda (name definition) (list (list quote name) definition)))

    '(main (!lambda () (fact 5)))
  ))
