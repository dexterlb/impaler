#import "/lib/misc.typ": citneeded, clink, paraphrase

= Motivation <motivation>

#paraphrase[We wish to build] a programming language that is as minimal as possible
while being expressive enough for general-purpose use. #paraphrase[This] is characterised
by the following properties:
+ Minimality <c-minimality>
  + Homoiconicity, provided by LISP-like syntax
  + Immutability
  + Few (and simple) special forms
+ Expressiveness
  + Mutual recursion
  + Metaprogramming (allow implementing #paraphrase[convenience structures] as
    libraries written in the language rather than compiler/interpreter
    features)
+ Performance <c-performance>

Some of these properties are at odds at each other: in particular, it is
difficult#citneeded to provide mutual recursion and immutability while at the
same time #paraphrase[having few and simple special forms]. For example,
Scheme, LISP and other similar languages forgo the "immutability" constraint,
which makes it easy#citneeded to implement cyclic data structures like
mutually-recursive function definitions. The toplevel expressions in such
languages are usually _statements_ like `define` that _mutate_ a global
_environment_.

```scheme
(define (even? x)
    (if (= x 0)
        #t
        (odd? (- 1 x))))

(define (odd? x)
    (if (= x 0)
        #t
        (even? (- 1 x))))

; in this example, both functions see each other's definitions because
; during their runtime they see the toplevel environment in its final
; state after both mutations have taken place
(display (even? 42))
```

Other LISP-like languages, such as LFE#citneeded, guarantee immutability of all
data, but handle a lot of the complexity in the interpreter itself: the
language features are written in the host language that implements the
interpreter, and not in the language itself. For example, functions defined in
the global namespace are distinct from locally defined lambda objects, and the
interpreter takes special care to #paraphrase[allow] recursion and mutual
recursion without allowing programs to mutate data. In fact, in LFE it is not
even possible to create a cyclic data structure altogether! The price that is
paid to achieve this is that the global namespace of defined functions is not
#paraphrase[manipulatable] by the program (which violates homoiconicity to some
extent) and that `define` and similar constructs are special forms.

It is therefore interesting to see if we can design a language that meets all
these goals at the same time. We define a language (which we will call ILD)
which adheres to the #clink(<c-minimality>)[minimality] constraints, and then
demonstrate expressiveness by implementing increasingly-complex constructs in
ILD, including a way to define mutually-recursive functions (@letrec).

In further research, we aim to also meet the
#clink(<c-performance>)[performance] constraint by employing partial evaluation
as an optimisation step, which is a technique known#cite(<anydsl>) to give good
results for reducing the overhead incurred by metaprogramming.
