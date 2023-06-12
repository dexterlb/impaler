; ((clambda ret (lambda)  ((!lambda (x y) (add x y)) 1 2))

; lambda -> clambda transformer test
; ((clambda ret (args body)
;     (ret
;         (cons 'clambda (cons 'the-ret-symb
;             (cons args (cons (cons 'the-ret-symb (cons body ())) ()))))))
;  '(x y) '(add x y))

((clambda ret (lambda)
    (ret

        ; ((!lambda (let)
        ;
        ;           (!let ((foo 1)
        ;                  (bar 2))
        ;             (add foo bar)))
        ;
        ;  (!lambda (items body) (cons 'quote (cons (cons items (cons body ())) ()))))))

        ((!lambda (Y if)
          (!if (null? ('foo))
            'yay
            'nay))

          ; definition of "Y"
          (lambda (f)
            ((lambda (x)
              (f (lambda (y) ((x x) y))))
            (lambda (x)
              (f (lambda (y) ((x x) y))))))

          ; definition of "if"
          ; (if cond foo bar) -> (((bool-to-k cond) (!lambda () foo) (!lambda () bar)))
          (!lambda (cond a b)
            (cons
              (cons (cons 'bool-to-k (cons cond ()))
              (cons (cons 'macroexpand (cons 'lambda (cons () (cons a ()))))
              (cons (cons 'macroexpand (cons 'lambda (cons () (cons b ()))))
              ())))

              ())))))

(clambda ret (args body)
         (ret
           (cons 'clambda (cons 'the-ret-symb
                                (cons args (cons (cons 'the-ret-symb (cons body ())) ())))))))
