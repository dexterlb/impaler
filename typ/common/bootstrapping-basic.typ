#import "/lib/ild-stuff.typ": ild, ildsf
#import "/lib/misc.typ": paraphrase, note, comment, lst, side-by-side

= Bootstrapping basic constructs <bootstrapping-basic>
The base syntax of ILD is insufficient for writing nontrivial programs
in a succint way. However, its $ildsf("macroexpand")$ facillity allows
extending the syntax with arbitrary metaprogramming constructs, which we
build incrementally.

#note[maybe here add a note that this power is similar to fexpr stuff but uses a different mechanism?]

== Lambda <lambda-macro>
First, we define a macro called #ild("lambda") that will let us build abstractions (@abstraction)
easily:
#footnote[The #ild("(free-vars)") closure given to the host #ild("mk-lambda") can be replaced
by a closure that contains only #ild("cons"), #ild("mk-lambda"), #ild("quote") and #ild("free-vars").]
#lst(caption: [Implementation of the #ild("lambda") macro])[
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
#ild("(!lambda (x y) (+ x y))")
will be substituted by calls to #ild("mk-lambda") such as
#ild("(mk-lambda (free-vars) (quote (x y)) (quote (+ x y)))").

We can then wrap this definition in another call of #ild("mk-lambda") in order to expose
it under a #ild("lambda") name usable from the body of the host abstraction. Supposing that
this definition is stored as an ILD program called `core/bootstrap/lambda-macro.ild`,
we recall it twice (once to define #ild("lambda"), and once to pass itself as the
value of #ild("lambda")):
#lst(caption: [Outermost usage of the #ild("lambda") macro])[
```
((!(eval (free-vars) (read-source "core/bootstrap/lambda-macro.ild")) (lambda)

  <body that may use the lambda macro>

  ; definition of 'lambda'
  (eval (free-vars) (read-source "core/bootstrap/lambda-macro.ild")))
```
]

== Basic utilities
We define helper utilities in a scope where the #ild("lambda") macro is present. In particular,
we define an #ild("expand-lambda") macro that generates expressions of the form
#ild("(!lambda <args> <body>)"), which is in turn used in definitions of other
metaprogramming primitives. Since these definitions already operate on code
as data, calling #ild("expand-lambda") is no different from calling a regular
function: an advantage of the macro/function duality in ILD (@macro-function-duality).

Our definition of #ild("if") doesn't require a special form, since delayed execution
and boolean introspection are already provided by #ild("mk-lambda") and #ild("bool-to-k")
respectively.

#side-by-side(
lst(caption: "Definitions of basic helper primitives")[
```ild
; definition of 'list'
(!lambda args args)

; definition of 'cadr'
(!lambda (p) (car (cdr p)))

; 'Y' is special case of poly-fix
(!lambda (f) (car (poly-fix f)))
```
],
lst(caption: [Definition of #ild("expand-lambda")])[
```ild
(!lambda (args body)
  (cons macroexpand
    (cons lambda
      (cons args
        (cons body '())))))
```
],
)

#lst(caption: [Definition of #ild("if")])[
```ild
(!lambda (cond t f)       ; read this definition bottom to top
  (list                          ; force/unwrap the delayed computation
    (list                        ; eval the K or K*, obtaining one of the two
      (list bool-to-k cond)      ; convert the boolean condition to K or K*
      (expand-lambda '() t)      ; computation that evals the "true" branch
      (expand-lambda '() f))))   ; computation that evals the "false" branch
```
]


#side-by-side(
lst(caption: [Definition of #ild("map")])[
```ild
(Y (!lambda (map)
  (!lambda (f l)
    (!if (null? l)
      l
      (!if (pair? l)
        (cons (f (car l)) (map f (cdr l)))
        (make-fail (list 'not-a-list l)))))))))
```
],
lst(caption: [Definition of #ild("let")])[
```ild

(!lambda (letlist body)
  (cons
    (expand-lambda
      (map car letlist)
      body)
    (map cadr letlist)))))
```
],
)

Now we can bind locally-scoped values ergonomically:

#lst(caption: [Rewriting a #ild("lambda")-based scope binding to use #ild("let")])[
#side-by-side(
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
We define letrec (a version of #ild("let") that allows
mutual recursion) as a macro that builds each binding as a recursive operator
that in turn takes all bindings as arguments, and passes the list of those
bindings to #ild("poly-fix"):
#lst(caption: [Definition of #ild("letrec")])[
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
