(module
  (doc "some core functions")
  (exports if list)
  (imports
    (builtin (car cdr cons lambda macroexpand quote bool-to-k))
    )
  (defs
    '(list (!lambda args args))

    '(if (!lambda (cond t f)     ; read this definition bottom to top
      (list                          ; eval the result with no arguments to force the lazy value
        (list                        ; eval the K or K*, getting one of the two lazy values
          (list bool-to-k cond)      ; convert the boolean condition to K or K* to do the selection
          (expand-lambda '() t)      ; lazy value that evals the "true" branch
          (expand-lambda '() f)))))  ; lazy value that evals the "false" branch

    '(expand-lambda (!lambda (args body) (list macroexpand lambda args body)))
  ))
