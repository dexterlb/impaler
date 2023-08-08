((!lambda-cps ret (expand-lambda builtins)
  ((!lambda-actual (Y list cadr if)
    ((!lambda-actual (map)
      ((!lambda-actual (let)
        (!let
          ; helpers that we'll need to define letrec
          ((generate-element-getters (Y (!lambda-actual (rec) (!lambda-actual (list-name l)
            (!if (null? l)
              l
              (!if (pair? l)
                (cons (list 'car list-name)
                      (map (!lambda-actual (subl) (list (car subl) (cons 'cdr (cdr subl))))
                          (rec list-name (cdr l))))
                (make-fail (list 'list-of-letrec-elements-not-a-list l))))))))
            ; polyvariate Y combinator (operates on a list of functions instead of a single function)
            (polyY
              (!lambda-actual l
                ((!lambda-actual (x) (x x))
                (!lambda-actual (p)
                  (map (!lambda-actual (li) (!lambda-actual args (apply (apply li (p p)) args))) l))))))

          (!let
            ; definition of "letrec"
            ((letrec (!lambda-actual (defs body)
              (!let
                ((args (map car defs))
                  (item-bodies (map cadr defs)))
                  (!let (
                    (arg-bodies (map (!lambda-actual (def-body) (expand-lambda args def-body)) item-bodies))
                    (flist (gensym "flist")))

                    (!let (
                      (result-body (cons
                        (expand-lambda args body)
                        (generate-element-getters flist args))))

                      (list (expand-lambda (list flist) result-body) (cons polyY arg-bodies))))))))

                ; we have now defined letrec and can continue bootstrapping from there
                (eval
                  (cons (cons 'letrec letrec) builtins)
                  (read-source "__bootstrap_from_letrec")))))

        ; definition of "let"
        (!lambda-actual (letlist body)
          (cons
            (expand-lambda (map car letlist) body)
            (map cadr letlist)))))

      ; definition of "map"
      (Y (!lambda-actual (map)
          (!lambda-actual (f l)
            (!if (null? l)
              l
              (!if (pair? l)
                (cons (f (car l)) (map f (cdr l)))
                (make-fail (list 'not-a-list l)))))))))

    ; definition of "Y"
    (!lambda-actual (f)
      ((!lambda-actual (x)
        (f (!lambda-actual args (apply (x x) args))))
      (!lambda-actual (x)
        (f (!lambda-actual args (apply (x x) args))))))

    ; definition of "list"
    (!lambda-actual args args)

    ; definition of "cadr"
    (!lambda-actual (p) (car (cdr p)))

    ; definition of "if"
    ; (if cond foo bar) -> (((bool-to-k cond) (!lambda-actual () foo) (!lambda-actual () bar)))
    (!lambda-actual (cond a b)
      (cons
        (cons (cons 'bool-to-k (cons cond ()))
        (cons (expand-lambda '() a)
        (cons (expand-lambda '() b)
        ())))

        ()))

      ))

; definition of "expand-lambda"
(!lambda-actual (args body) (cons macroexpand (cons lambda-actual (cons args (cons body ())))))

; definition of "builtins"
(get-env))

; TODO: there's a conceptual problem with "gensym".
; Since our macros have "runtime" semantics, given the current
; version of gensym (which is impure and returns a new value each time)
; the result of the macro *depends* on the context in which it is called
; Thus, we can't expect something like (!lambda-actual (x) (foo (!my-macro baba)))
; to be partially evaluated to (!lambda-actual (x) (the-result-of-applying-my-macro-to-baba)),
; even though baba (the symbol 'baba) is a constant with respect to my-macro,
; if my-macro contains calls to gensym (since they will return different
; results for every value of x). We need another version of gensym
; that will be constant in this case.
; I propose a (gensym name . args), where the returned symbol is
; a hash of args; Thus, we'll be able to write macros that only
; depend on their arguments. However, is this enough to ensure
; hygiene? To be continued!
