((clambda ret (lambda) (ret

  ((!lambda (Y list cadr if)
    ((!lambda (map)
      ((!lambda (let)

        (!let
          ((le-arglist
            '((is-even? (!lambda (x) (if (= x 0) #t (is-odd?  (- x 1)))))
              (is-odd? (!lambda (x) (if (= x 0) #f (is-even? (- x 1)))))))
            (le-body
              '(is-even? 42))

           (letrec (!lambda (defs body)
             (!let
               ((args (map car defs))
                (item-bodies (map cadr defs))
                (generate-element-getters (Y (!lambda (rec) (!lambda (list-name l)
                  (!if (null? l)
                    l
                    (!if (pair? l)
                      (cons (list 'car list-name)
                            (map (!lambda (subl) (list (car subl) (cons 'cdr (cdr subl))))
                                 (rec list-name (cdr l))))
                      (make-fail (list 'list-of-letrec-elements-not-a-list l)))))))))

                (!let
                  ((arg-bodies (map (!lambda (def-body) (lambda args def-body)) item-bodies))
                   (flist 'the-flist-symb))

                  (!let
                    ((result-body (cons (lambda args body)
                                        (generate-element-getters flist args))))

                    (list (lambda (list flist) result-body) (cons 'Y arg-bodies)))))))
          )

          (letrec le-arglist le-body)))


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
              (!if (pair? l)
                (cons (f (car l)) (map f (cdr l)))
                (make-fail (list 'not-a-list l)))))))))

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
