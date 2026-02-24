((!lambda (builtins)
  (!letrec (
      (list (!lambda args args))

      (expand-lambda (!lambda (args body) (list macroexpand lambda args body)))

      (if (!lambda (cond t f)     ; read this definition bottom to top
        (list                          ; eval the result with no arguments to force the lazy value
          (list                        ; eval the K or K*, getting one of the two lazy values
            (list bool-to-k cond)      ; convert the boolean condition to K or K* to do the selection
            (expand-lambda '() t)      ; lazy value that evals the "true" branch
            (expand-lambda '() f)))))  ; lazy value that evals the "false" branch

      (not (!lambda (b)
        (!if b
          #f
          #t)))

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

      (cadr (!lambda (p) (car (cdr p))))
      (caar (!lambda (p) (car (car p))))

      (let (!lambda (defs body)
        (cons
          (expand-lambda (map car defs) body)
          (map cadr defs))))

      (let* (!lambda (defs body)
        (!if (null? defs)
          body
          (list
            (expand-lambda
              (list (caar defs))
              (let* (cdr defs) body))
            (cadr (car defs))))))

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

      (kv-get (!lambda (k kvl)
        (kv-fetch k (make-fail (list 'key-not-found (list 'key k) (list 'kv-list kvl))) kvl)))

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
        (!let* (
          (items (cdr mod-expr))

          (imports  (kv-fetch 'imports  (list) items))
          (defs     (kv-fetch 'defs     (list) items))
          (exports  (kv-fetch 'exports  (list) items))

          (env (process-imports imports))
          (eval-def (!lambda (def)
            (eval env def)))
          (def-items (map eval-def defs)))

          (eval env
            (letrec def-items (cons list (map sym-item-pair exports)))))))

      (process-imports (!lambda (import-list)
        (concat (map process-import import-list))))

      (process-import (!lambda (import-spec)
        (!if (not (pair? import-spec))
          (make-fail (list 'import-spec-must-be-list import-spec))
          (!if (!and (symbol? (car import-spec)) (sym-eq? (car import-spec) 'builtin))
            (process-import-items builtins (cdr import-spec))
            (!if (string? (car import-spec))
              (process-import-items (load-module (car import-spec)) (cdr import-spec))
              (make-fail (list 'import-spec-invalid import-spec)))))))

      (process-import-items (!lambda (items spec-tail)
        (!if (!and
          (pair? spec-tail)
          (null? (cdr spec-tail))
          (pair? (car spec-tail)))

          (concat (map (!lambda (picker) (execute-picker picker items)) (car spec-tail)))
          (make-fail (list 'expected-picker-list spec-tail)))))

      (execute-picker (!lambda (picker items)
        (!if (symbol? picker)
          ; symbol pickers refer a single exported object
          (list (cons picker (kv-get picker items)))
          'not-implemented)))

      (sym-item-pair (!lambda (sym)
        (list cons (list quote sym) sym)))

      (module-loader (!lambda (entrypoint-module-name entrypoint-func-name entrypoint-func-args)
        (apply
          (kv-get entrypoint-func-name (load-module entrypoint-module-name))
          entrypoint-func-args)))

      (debug-trace (!lambda (name val)
        (yield (list 'trace name val))
        val))

      (concat (!lambda (l)
        (!if (null? l)
          l
          (!if (not (pair? l))
            (make-fail (list 'expected-list-of-lists l))
            (!if (null? (car l))
              (concat (cdr l))
              (!if (not (pair? (car l)))
                (make-fail (list 'expected-list (car l)))
                (cons (car (car l)) (concat (cons (cdr (car l)) (cdr l))))))))))

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

      (reciprocate (!lambda (x) (div 1 x))))


    ;finally, return the module loader
    module-loader))

  ; definition of 'builtins':
  (get-env))
