((eval (getenv) (protolambda (getenv)
    (quote (lambda)) (quote
        ; now we have 'lambda' and can do more ergonomic stuff

        ((!lambda (letrec)
            (eval (getenv) (read-source "core/bootstrap/define_module_loader.l")))

            ; definition of 'letrec':
            (eval (getenv) (read-source "core/bootstrap/define_letrec.l")))
)))
    ; definition of 'lambda':
    (eval (getenv) (protolambda (getenv) (quote (args body)) (quote
        (cons
            eval
        (cons
            (cons getenv ())
        (cons
            (cons
                protolambda
            (cons
                (cons getenv ())
            (cons
                (cons quote (cons args ()))
            (cons
                (cons quote (cons body ()))
                ()))))
            ()))))))
)
