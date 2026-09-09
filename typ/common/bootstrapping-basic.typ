#import "/lib/ild-stuff.typ": ild
#import "/lib/misc.typ": paraphrase, note, comment

= Bootstrapping basic constructs <bootstrapping-basic>
Now that we have defined our minimal language with its minimal host environment,
we can build upon them using metaprogramming and macros to incrementally define
more complex ergonomic syntax.

== Letrec
To define #ild("letrec"), which would allow us to ergonomically write recursive functions,
we must first bootstrap some lower-level primitives.

=== Lambda <lambda-macro>
First, we define a macro called #ild("lambda") that will let us build abstractions (@abstraction)
easily:
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

#comment[The #ild("(free-vars)") closure given to the host #ild("mk-lambda") can be replaced
by a closure that contains only #ild("cons"), #ild("mk-lambda"), #ild("quote") and #ild("free-vars").]

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
```
((!(eval (free-vars) (read-source "core/bootstrap/lambda-macro.ild")) (lambda)

  <body that may use the lambda macro>

  ; definition of 'lambda'
  (eval (free-vars) (read-source "core/bootstrap/lambda-macro.ild")))
```

=== Basic utilities

#comment[We do not yet have a mechanism to "define" values other than
using the trick with #ild("lambda") given above, so everything we define from here to
after the definition of #ild("let") would have to be exposed to code that uses it
with nested #ild("lambda") abstractions.]

- #ild("expand-lambda")
```ild
  (!lambda (args body) (cons macroexpand (cons lambda (cons args (cons body '())))))
```
- #ild("list")
- #ild("cadr")

#note[this section is unfinished]

=== Y combinator

#note[this section is unfinished]

=== Recursive utilities
- map

#note[this section is unfinished]

=== Let
A programmer would often like to be able to define some #paraphrase[items] and
use them in other code. As the reader is probably used to from
#{sym.lambda}-calculus, the most "low-level" way to do that is by using a
closure:

```ild
((!lambda (add1 fourtytwo)
    (add1 fourtytwo))

    (!lambda (x) (add x 1)) ; definition of add1
    42)                     ; definition of fourtytwo
; produces 43
```

To be able to do this more ergonomically, we define a _macro_ called #ild("let"):
```ild
; definition of "let"
(!lambda (letlist body)
    (cons
        (expand-lambda (map car letlist) body)
        (map cadr letlist)))))
```

We can now rewrite the above example into:
```ild
(!let (
  (add1       (!lambda (x) (add x 1)))
  (fourtytwo  42))

  (add1 fourtytwo))
```

=== Letrec <letrec>

#note[this section is unfinished]
