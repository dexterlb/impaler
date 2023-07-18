((clambda ret (lambda) (ret

  ((!lambda (Y list cadr if)
    ((!lambda (map)
      ((!lambda (let)

        (!let ((foo 1)
               (bar 2))
            (add foo bar)))


        ; definition of "let"
        (!lambda (letlist body)
          (cons
            (lambda (map car letlist) body)
            (map cadr letlist)))))

      ; definition of "map"
      (Y (!lambda (map)
          (!lambda (f l)
            (!if (null? l)
              l
              (cons (f (car l)) (map f (cdr l)))))))))

    ; definition of "Y"
    (!lambda (f)
      ((!lambda (x)
        (f (!lambda args (apply (x x) args))))
      (!lambda (x)
        (f (!lambda args (apply (x x) args))))))

    ; definition of "list"
    (!lambda args args)

    ; definition of "cadr"
    (!lambda (p) (car (cdr p)))

    ; definition of "if"
    ; (if cond foo bar) -> (((bool-to-k cond) (!lambda () foo) (!lambda () bar)))
    (!lambda (cond a b)
      (cons
        (cons (cons 'bool-to-k (cons cond ()))
        (cons (lambda '() a)
        (cons (lambda '() b)
        ())))

        ()))

      )))

; definition of "lambda"
(clambda ret (args body)
         (ret
           (cons 'clambda (cons 'the-ret-symb
                                (cons args (cons (cons 'the-ret-symb (cons body ())) ())))))))
