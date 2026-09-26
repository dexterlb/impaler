#import "/lib/ild-stuff.typ": ild
#import "/lib/misc.typ": paraphrase, note, comment, lst

= Bootstrapping basic constructs <bootstrapping-basic>
Now that we have defined our minimal language with its minimal host environment,
we can build upon them using metaprogramming and macros to incrementally define
more complex ergonomic syntax.

== Lambda <lambda-macro>
First, we define a macro called #ild("lambda") that will let us build abstractions (@abstraction)
easily:
#footnote[The #ild("(free-vars)") closure given to the host #ild("mk-lambda") can be replaced
by a closure that contains only #ild("cons"), #ild("mk-lambda"), #ild("quote") and #ild("free-vars").]
#lst(caption: "Implementation of the lambda macro")[
```ild
(mk-lambda
  (free-vars)
  '(arg-names body)
  '(cons mk-lambda
    (cons (cons free-vars '())      ; closure, equal to the call site's env
      (cons (cons quote (cons arg-names '()))        ; quoted arg-names
        (cons (cons quote (cons body '()))           ; quoted body
          '())))))
```
]

Expansions of the #ild("lambda") macro such as
```ild
(!lambda (x y) (+ x y))
```
will replace themselves by calls to #ild("mk-lambda") such as
```ild
(mk-lambda (free-vars) (quote (x y)) (quote (+ x y)))
```

We can then wrap this definition in another call of #ild("mk-lambda") in order to expose
it as a #ild("lambda") name usable from the body of the host abstraction. For brevity,
we write the definition as an ILD program called `core/bootstrap/lambda-macro.ild`
and then recall it twice (once to define #ild("lambda"), and once to pass itself as the
value of #ild("lambda")):
#lst(caption: "Outermost usage of the lambda macro")[
```
((!(eval (free-vars) (read-source "core/bootstrap/lambda-macro.ild")) (lambda)

  <body that may use the lambda macro>

  ; definition of 'lambda'
  (eval (free-vars) (read-source "core/bootstrap/lambda-macro.ild")))
```
]

== Basic utilities
We do not yet have a mechanism to "define" values other than
using the trick with #ild("lambda") given above, so everything we define from here to
after the definition of #ild("let") would have to be exposed to code that uses it
with nested #ild("lambda") abstractions.

#lst(caption: "Definitions of basic helper primitives")[
```ild
; definition of 'list'
(!lambda args args)

; definition of 'cadr'
(!lambda (p) (car (cdr p)))

; definition of 'Y' (special case of poly-fix)
(!lambda (f) (car (poly-fix f)))

; definition of 'map'
(Y (!lambda (map)
  (!lambda (f l)
    (!if (null? l)
      l
      (!if (pair? l)
        (cons (f (car l)) (map f (cdr l)))
        (make-fail (list 'not-a-list l)))))))))

; definition of 'expand-lambda'
(!lambda (args body) (cons macroexpand (cons lambda (cons args (cons body '())))))
```
]

=== Let
With the building blocks above, we define #ild("let") as:
#lst(caption: "Definition of let")[
```ild
(!lambda (letlist body)
  (cons
    (expand-lambda (map car letlist) body)
    (map cadr letlist)))))
```
]
Now we can bind locally-scoped values ergonomically:
#lst(caption: "Rewriting a lambda-based scope binding to use let")[
#grid(columns: (auto, auto, auto), column-gutter: 0.6em, align: horizon,
```ild
((!lambda (add1 fourty-two)
    (add1 fourty-two))

    (!lambda (x)
      (add x 1)) ; definition of add1
    42)          ; definition of fourty-two
; produces 43
```,
$=>$,
```ild
(!let (
    (add1 (!lambda (x)
            (add x 1)))
    (fourty-two  42))

  (add1 fourty-two))
```,
)
]

== Letrec <letrec>
We define letrec as a macro that builds each binding as a recursive operator
that in turn takes all bindings as arguments, and passes the list of
those bindings to #ild("poly-fix"):
#lst(caption: "Definition of letrec")[
```ild
(!lambda (defs body)
  (!let
    ((args (map car defs))
      (item-bodies (map cadr defs)))
    (!let (
      (arg-bodies (map
        (!lambda (def-body) (expand-lambda args def-body))
        item-bodies))
      (flist (gensym "flist")))

      (!let (
        (result-body (cons
          (expand-lambda args body)
          (generate-element-getters flist args))))

        (list
          (expand-lambda (list flist) result-body)
          (cons poly-fix arg-bodies))))))
```
]
We omit the definition of the #ild("generate-element-getters") helper, which is merely technical.
