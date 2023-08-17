(!letrec (
    (list (!lambda args args))

    (expand-lambda (!lambda (args body) (list macroexpand lambda args body)))

    (if (!lambda (cond t f)     ; read this definition bottom to top
      (list                          ; eval the result with no arguments to force the lazy value
        (list                        ; eval the K or K*, getting one of the two lazy values
          (list bool-to-k cond)      ; convert the boolean condition to K or K* to do the selection
          (expand-lambda '() t)      ; lazy value that evals the "true" branch
          (expand-lambda '() f)))))  ; lazy value that evals the "false" branch

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
          (!if (is-kv? (car kvl))
            (!if (sym-eq? (car (car kvl)) k)
              (f (car kvl))
              (kv-find k (cdr kvl) empty f))
            (make-fail (list 'not-a-kv-pair (car kvl))))
          (make-fail (list 'not-a-list kvl))))))

    (kv-fetch (!lambda (k dflt kvl)
      (kv-find k kvl dflt cdr)))

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
      (!and (pair? item) (symbol? (car item)))))

    (load-module (!lambda (module-name)
      (eval-module (read-source module-name))))

    (eval-module (!lambda (mod-expr)
      (!letrec (
        (items (!delay (cdr mod-expr)))
        (defs (kv-fetch 'defs (list) (force items))))

        (list (defs car) (force items)))))

    (delay (!lambda (ast) (expand-lambda () ast)))
    (force (!lambda (d) (d)))

    (is-even? (!lambda (x) (!if (<= x 0) #t (is-odd?  (add x -1)))))
    (is-odd?  (!lambda (x) (!if (<= x 0) #f (is-even? (add x -1)))))

    (baba (!lambda () (load-module "__main")))
    (dyado (!lambda () (!or #f #f)))

    (map (!lambda (f l)
      (!if (null? l)
        l
        (!if (pair? l)
          (cons (f (car l)) (map f (cdr l)))
          (make-fail (list 'not-a-list l))))))

    (try-map (!lambda (f l)
      (call/cc (!lambda (return)
        (return (map
          (!lambda (x)
            (!if (fail? (f x))
              (return (make-fail (list 'fail-in-element x (f x))))
              (f x))) l))))))

    (reciprocate (!lambda (x) (div 1 x)))


    (fact (!lambda (x)
      (!if (<= x 0)
        1
        (mul x (fact (add x -1)))))))

  ; (list 'factorial-of-5-is (fact 5) 'number-42-is-odd? (is-odd? 42))))))
  (list
    (list 'baba (baba))
    (list 'dyado (dyado))
    (list 'factorial-of-5-is (fact 5))
    (list 'number-26-is-odd? (is-odd? 26))
    (list 'broken-map
      (!if (fail? (try-map reciprocate (list 1 2 3 0 4)))
        'broken-map-failed-successfully
        'oh-no-broken-map-not-broken))
    (list 'successful-map (try-map reciprocate (list 1 2 3 4)))
    ))
