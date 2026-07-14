(module
  (doc "this module calculates factorials :)")
  (exports main)
  (imports
    (builtin (macroexpand <= mul add))
    ("core/prelude.l" (if))
    ("core/module-utils.l" (fn)))
  (defs
    (!fn main () (fact 5))

    (!fn fact (x)
      (!if (<= x 0)
        1
        (mul x (fact (add x -1)))))))
