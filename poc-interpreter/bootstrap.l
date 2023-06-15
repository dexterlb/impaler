; ((clambda ret (lambda)  ((!lambda (x y) (add x y)) 1 2))

; lambda -> clambda transformer test
; ((clambda ret (args body)
;     (ret
;         (cons 'clambda (cons 'the-ret-symb
;             (cons args (cons (cons 'the-ret-symb (cons body ())) ()))))))
;  '(x y) '(add x y))

; (macroexpand foo bar baz) -> (expand (foo (quote bar) (quote baz)))
((clambda ret (lambda)
    (ret

        ; ((!lambda (let)
        ;
        ;           (!let ((foo 1)
        ;                  (bar 2))
        ;             (add foo bar)))
        ;
        ;  (!lambda (items body) (cons 'quote (cons (cons items (cons body ())) ()))))))

        ((!lambda (apply if)
          ((!lambda (Y)
            ((!lambda (map)
              (map car '((foo bar) (baz qux))))

              ; definition of "map"
              (Y (!lambda (map)
                  (!lambda (f l)
                    (if (null? l)
                      l
                      (cons (f (car l) (map f (cdr l))))))))))

              ; definition of "Y"
              (!lambda (f)
                ((!lambda (x)
                  (f (!lambda args (apply (x x) args))))
                (!lambda (x)
                  (f (!lambda args (apply (x x) args))))))))

          ; definition of "apply"
          (!lambda (f args)
            (expand (cons f args)))

          ; definition of "if"
          ; (if cond foo bar) -> (((bool-to-k cond) (!lambda () foo) (!lambda () bar)))
          (!lambda (cond a b)
            (cons
              (cons (cons 'bool-to-k (cons cond ()))
              (cons (cons 'macroexpand (cons 'lambda (cons () (cons a ()))))
              (cons (cons 'macroexpand (cons 'lambda (cons () (cons b ()))))
              ())))

              ()))

           )))

(clambda ret (args body)
         (ret
           (cons 'clambda (cons 'the-ret-symb
                                (cons args (cons (cons 'the-ret-symb (cons body ())) ())))))))
