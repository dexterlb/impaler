(!letrec (
    (list (!lambda args args))

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

    (load-module (!lambda (module-name)
      (eval-module (read-source module-name))))

    (eval-module (!lambda (mod-expr)
      mod-expr))

    (is-even? (!lambda (x) (!if (<= x 0) #t (is-odd?  (add x -1)))))
    (is-odd?  (!lambda (x) (!if (<= x 0) #f (is-even? (add x -1)))))

    (baba (!lambda () (load-module "__main")))
    (dyado (!lambda () (!or #f #f)))

    (fact (!lambda (x)
      (!if (<= x 0)
        1
        (mul x (fact (add x -1)))))))

  ; (list 'factorial-of-5-is (fact 5) 'number-42-is-odd? (is-odd? 42))))))
  (list 'baba (baba) 'dyado (dyado)))
