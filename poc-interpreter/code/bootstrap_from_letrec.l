(!letrec (
    (list (!lambda-actual args args))

    (if (!lambda-actual (cond t f)              ; read this definition bottom to top
      (list                                           ; eval the result with no arguments to force the lazy value
        (list                                         ; eval the K or K*, getting one of the two lazy values
          (list bool-to-k cond)                       ; convert the boolean condition to K or K* to do the selection
          (list macroexpand lambda-actual '() t)      ; lazy value that evals the "true" branch
          (list macroexpand lambda-actual '() f)))))  ; lazy value that evals the "false" branch

    (or (!lambda-actual items
      (!if (null? items)
        #f
        (!if (pair? items)
          (if (car items)
            #t
            (apply or (cdr items)))
          (make-fail (list 'unexpected-arg-to-or items))))))

    (and (!lambda-actual items
      (!if (null? items)
        #t
        (!if (pair? items)
          (if (car items)
            (apply and (cdr items))
            #f)
          (make-fail (list 'unexpected-arg-to-and items))))))

    ; some functions for dealing with key-value lists
    ; (only supporting symbols as keys)
    (kv-find (!lambda-actual (k kvl empty f)
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

    (kv-has? (!lambda-actual (k kvl)
      (kv-find k kvl #f
        (!lambda-actual (_kv) #t))))

    (kv-valid? (!lambda-actual (kvl)
      (!or
        (null? kvl)
        (!and
          (pair? kvl)
          (pair? (car kvl))
          (symbol? (car (car kvl)))
          (kv-valid? (cdr kvl))))))

    (is-kv? (!lambda-actual (item)
      (!and (pair? item) (symbol? (car item)))))

    (load-module (!lambda-actual (module-name)
      (eval-module (read-source module-name))))

    (eval-module (!lambda-actual (mod-expr)
      mod-expr))

    (is-even? (!lambda-actual (x) (!if (<= x 0) #t (is-odd?  (add x -1)))))
    (is-odd?  (!lambda-actual (x) (!if (<= x 0) #f (is-even? (add x -1)))))

    (baba (!lambda-actual () (load-module "__main")))
    (dyado (!lambda-actual () (!or #f #f)))

    (fact (!lambda-actual (x)
      (!if (<= x 0)
        1
        (mul x (fact (add x -1)))))))

  ; (list 'factorial-of-5-is (fact 5) 'number-42-is-odd? (is-odd? 42))))))
  (list
    (list 'baba (baba))
    (list 'dyado (dyado))
    (list 'factorial-of-5-is (fact 5))
    (list 'number-26-is-odd? (is-odd? 26))
    ))
