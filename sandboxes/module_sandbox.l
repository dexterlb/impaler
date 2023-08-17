(module
  (imports
    (builtin (car cdr cons))                    ; inject those builtin functions into the environment
    (module "foo/bar.l" bar)                    ; import other module (qualified, creates bar:<def> bindings for
                                                ;    every definition in the other module
    (module "foo/pp.l" (baba dyado))            ; import the baba and dyado definitions from this module
    (module "foo/qq.l")                         ; import everything from this module
    (module "module-utils" (const fn fn-cps)))

  (exports add1 bar)

  (defs     ; this is a list of key-value pairs that get eval'd
    '(answer . 42)

    (!const answer 42)  ; better way to say the above

    '((foo . 1337)    ; nested lists are automatically flattened
      (bar . 1338))   ; so that macros can inject more than one definition easily

    (!fn (add1 x)       ; expands to (cons 'add1 <lambda>)
        (+ 1 x))

    (!fn-cps (add2 ret x)
        (ret (+ 1 x)))))
