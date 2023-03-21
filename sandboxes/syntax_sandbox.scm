; let's say all builtins have names starting with ':'
; we need the following builtins:
; - :clambda - (:clambda <list of args, first is callback> <body>)
;            - a CPS lambda - example: (:clambda (callback foo bar baz) (the body))
; - :quote   - quote
; - :quasiquote, :unquote
; - :expand  - replaced by its argument before evaluation


; bangs as syntax sugar for macro expansion
; (!foo bar baz) is the same as (:expand (foo (:quote bar) (:quote baz)))

; quoting syntax sugar:
; - '() is quasiquote, ,foo is unquote


(map (:clambda (callback x) (callback (+ 1 x))) ...)

(let*
    (lambda
      (:clambda (callback arg_list body)
        (

         something
         ))))



; <def> is a placeholder for some way to define something
(<def> (if cnd t f) ((bool-to-debruijn cnd) t f))

; reading modules could be done like this:
(!with-modules
  ( (foo "foo.legs")
    (bar "bar.legs"))

  <expression containing calls from modules>)


; a file ("module") looks like this:

(list
    ; (if cnd foo bar) -> (((bool-to-debruijn cnd) (lambda () t) (lambda () f)))

    (if (cnd t f) (quote (
                    ((bool-to-debruijn (unquote cnd))
                        (lambda () (unquote t))
                        (lambda () (unquote f))))))
)
