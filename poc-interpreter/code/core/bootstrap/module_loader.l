((!lambda (letrec)
    (expand (read-source "core/bootstrap/define_module_loader.l")))

    ; definition of 'letrec':
    (expand (read-source "core/bootstrap/define_letrec.l")))
