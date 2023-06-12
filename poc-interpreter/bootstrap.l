; ((clambda ret (lambda)  ((!lambda (x y) (add x y)) 1 2))

((clambda ret (args body) (ret (cons (quote clambda) (cons (quote the-ret-symb) (cons args (cons body ()))))))
 (quote (x y)) (quote (add x y)))
