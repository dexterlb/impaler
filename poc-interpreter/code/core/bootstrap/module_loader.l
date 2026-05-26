((!lambda (letrec)
    (eval (get-env) (read-source "core/bootstrap/define_module_loader.l")))

    ; definition of 'letrec':
    (eval (get-env) (read-source "core/bootstrap/define_letrec.l")))
