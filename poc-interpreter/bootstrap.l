; ((clambda ret (lambda)  ((!lambda (x y) (add x y)) 1 2))

; lambda -> clambda transformer test
; ((clambda ret (args body) 
;     (ret 
;         (cons 'clambda (cons 'the-ret-symb 
;             (cons args (cons (cons 'the-ret-symb (cons body ())) ()))))))
;  '(x y) '(add x y))

((clambda ret (lambda)
    (ret ((macroexpand lambda (x y) (add x y)) 1 2)))

(clambda ret (args body) 
         (ret 
           (cons 'clambda (cons 'the-ret-symb 
                                (cons args (cons (cons 'the-ret-symb (cons body ())) ())))))))
