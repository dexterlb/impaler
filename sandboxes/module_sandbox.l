(module
  (imports
    (builtin (car cdr cons)     ; inject those builtin functions into the environment
    (module "foo/bar.l" bar)    ; import other module (qualified, creates bar:<def> bindings for
                                ;    every definition in the other module
    (module "foo/pp.l" (baba dyado))   ; import the baba and dyado definitions from this module
    (module "foo/qq.l"))   ; import everything from this module

  (defs     ; this is a list of key-value pairs that get eval'd
    '(answer 42)
    (!const answer 42)  ; better way to say the above

    (!fn (add1 x)       ; expands to ('add1, <lambda>)
        (+ 1 x))

    (!fn-cps (add2 ret x)
        (ret (+ 1 x))))))
