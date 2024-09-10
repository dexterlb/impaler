((!lambda (expand-lambda builtins)
  ((!lambda (list cadr if)
    ((!lambda (Y)
      ((!lambda (map)
        ((!lambda (let)
          (!let
            ; helpers that we'll need to define letrec
            ((generate-element-getters (Y (!lambda (rec) (!lambda (list-name l)
              (!if (null? l)
                l
                (!if (pair? l)
                  (cons (list car list-name)
                        (map (!lambda (subl) (list (car subl) (cons cdr (cdr subl))))
                            (rec list-name (cdr l))))
                  (make-fail (list 'list-of-letrec-elements-not-a-list l))))))))
              ; polyvariate Y combinator (operates on a list of functions instead of a single function)
              (polyY
                ; if poly-fix is already available, use it
                ; TODO: more civilised way to check that this exists
                (!if (func? poly-fix)
                  poly-fix
                  (!lambda l
                    ((!lambda (x) (x x))
                    (!lambda (p)
                      (map (!lambda (li) (!lambda args (apply (apply li (p p)) args))) l)))))))

            (!let
              ; definition of "letrec"
              ((letrec (!lambda (defs body)
                (!let
                  ((args (map car defs))
                    (item-bodies (map cadr defs)))
                    (!let (
                      (arg-bodies (map (!lambda (def-body) (expand-lambda args def-body)) item-bodies))
                      (flist (gensym "flist")))

                      (!let (
                        (result-body (cons
                          (expand-lambda args body)
                          (generate-element-getters flist args))))

                        (list (expand-lambda (list flist) result-body) (cons polyY arg-bodies))))))))

                  ; we have now defined letrec - return it!
                  letrec)))

          ; definition of "let"
          (!lambda (letlist body)
            (cons
              (expand-lambda (map car letlist) body)
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
      (!if (func? poly-fix)
        ; if poly-fix is available, define Y through it
        (!lambda (f)
          (car (poly-fix f)))

        ; otherwise, define Y without relying on recursion
        (!lambda (f)
          ((!lambda (x)
            (f (!lambda args (apply (x x) args))))
          (!lambda (x)
            (f (!lambda args (apply (x x) args)))))))))

    ; definition of "list"
    (!lambda args args)

    ; definition of "cadr"
    (!lambda (p) (car (cdr p)))

    ; definition of "if"
    ; (if cond foo bar) -> (((bool-to-k cond) (!lambda () foo) (!lambda () bar)))
    (!lambda (cond a b)
      (cons
        (cons (cons 'bool-to-k (cons cond ()))
        (cons (expand-lambda '() a)
        (cons (expand-lambda '() b)
        ())))

        ()))

      ))

; definition of "expand-lambda"
(!lambda (args body) (cons macroexpand (cons lambda (cons args (cons body ())))))

; definition of "builtins"
(getenv))
