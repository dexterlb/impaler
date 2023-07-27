; between occurances of "BEGIN-DRAGONS" and "END-DRAGONS" shall be dragons
; BEGIN-DRAGONS
((clambda ret (lambda) (ret
  ((!lambda (Y list cadr if)
    ((!lambda (map)
      ((!lambda (let)
        (!let
          ; helpers that we'll need to define letrec
          ((generate-element-getters (Y (!lambda (rec) (!lambda (list-name l)
            (!if (null? l)
              l
              (!if (pair? l)
                (cons (list 'car list-name)
                      (map (!lambda (subl) (list (car subl) (cons 'cdr (cdr subl))))
                          (rec list-name (cdr l))))
                (make-fail (list 'list-of-letrec-elements-not-a-list l))))))))
            ; polyvariate Y combinator (operates on a list of functions instead of a single function)
            (polyY
              (!lambda l
                ((!lambda (x) (x x))
                (!lambda (p)
                  (map (!lambda (li) (!lambda args (apply (apply li (p p)) args))) l))))))

          (!let
            ; definition of "letrec"
            ((letrec (!lambda (defs body)
              (!let
                ((args (map car defs))
                  (item-bodies (map cadr defs)))
                  (!let (
                    (arg-bodies (map (!lambda (def-body) (lambda args def-body)) item-bodies))
                    (flist (gensym "flist")))

                    (!let (
                      (result-body (cons
                        (lambda args body)
                        (generate-element-getters flist args))))

                      (list (lambda (list flist) result-body) (cons polyY arg-bodies))))))))
            ; END-DRAGONS
            ; we now have letrec and can write sensible code
            (!letrec (
                (or (!lambda items
                  (!if (null? items)
                    #f
                    (!if (pair? items)
                      (if (car items)
                        #t
                        (apply or (cdr items)))
                      (make-fail (list 'unexpected-arg-to-or items))))))

                (and (!lambda items
                  (!if (null? items)
                    #t
                    (!if (pair? items)
                      (if (car items)
                        (apply and (cdr items))
                        #f)
                      (make-fail (list 'unexpected-arg-to-and items))))))

                ; some functions for dealing with key-value lists
                ; (only supporting symbols as keys)
                (kv-find (!lambda (k kvl empty f)
                  (!if (null? kvl)
                    empty
                    (!if (pair? kvl)
                      (!let ((kv (car kvl)))
                        (!if (is-kv? kv)
                          (!if (sym-eq? (car kv) k)
                            (f kv)
                            (kv-has? k (cdr kvl)))
                          (make-fail (list 'not-a-kv-pair kv))))
                      (make-fail (list 'not-a-list kvl))))))

                (kv-has? (!lambda (k kvl)
                  (kv-find k kvl #f
                    (!lambda (_kv) #t))))

                (kv-valid? (!lambda (kvl)
                  (!or
                    (null? kvl)
                    (!and
                      (pair? kvl)
                      (pair? (car kvl))
                      (symbol? (car (car kvl)))
                      (kv-valid? (cdr kvl))))))

                (is-kv? (!lambda (item)
                  (!if (pair? item)
                    (!if (symbol? (car item))
                      #t
                      #f)
                    #f)))


                (is-even? (!lambda (x) (!if (<= x 0) #t (is-odd?  (add x -1)))))
                (is-odd?  (!lambda (x) (!if (<= x 0) #f (is-even? (add x -1)))))

                (baba (!lambda () 42))
                (dyado (!lambda () (!or #f #f)))

                (fact (!lambda (x)
                  (!if (<= x 0)
                    1
                    (mul x (fact (add x -1)))))))

              ; (list 'factorial-of-5-is (fact 5) 'number-42-is-odd? (is-odd? 42))))))
              (list 'baba (baba) 'dyado (dyado))))))

        ; BEGIN-DRAGONS

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
    ((clambda ret (the-ret-symb)
      (ret
        ; now define "lambda" as "clambda" with a fresh "return" symbol
        ; and a body that just executes (return <body>)
        (cons 'clambda (cons the-ret-symb
          (cons args (cons (cons the-ret-symb (cons body ())) ()))))))
      ; definition of the-ret-symb: a symbol that will be used as the
      ; continuation argument:
      (gensym "return")))))

; END-DRAGONS

; TODO: there's a conceptual problem with "gensym".
; Since our macros have "runtime" semantics, given the current
; version of gensym (which is impure and returns a new value each time)
; the result of the macro *depends* on the context in which it is called
; Thus, we can't expect something like (!lambda (x) (foo (!my-macro baba)))
; to be partially evaluated to (!lambda (x) (the-result-of-applying-my-macro-to-baba)),
; even though baba (the symbol 'baba) is a constant with respect to my-macro,
; if my-macro contains calls to gensym (since they will return different
; results for every value of x). We need another version of gensym
; that will be constant in this case.
; I propose a (gensym name . args), where the returned symbol is
; a hash of args; Thus, we'll be able to write macros that only
; depend on their arguments. However, is this enough to ensure
; hygiene? To be continued!
