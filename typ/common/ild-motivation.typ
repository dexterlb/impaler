#import "/lib/misc.typ": citneeded, clink, paraphrase, note
#import "/lib/ild-stuff.typ": ildmono

= Motivation <motivation>

#note[
We should focus on the fact that a similar thing has been done before
(#cite(<fexpr-shutt>)#cite(<fexpr-pe>)), and that the novel thing we're
trying here is *callsite-annotated macro expansion*.
Parts of this section should be moved to @discussion.
]

We design a programming language that is as minimal as possible
while being expressive enough for general-purpose use. #paraphrase[We are aiming towards] the
following properties:
+ Minimality <c-minimality>
  + Homoiconicity, provided by LISP-like syntax
  + Immutability
  + Few (and simple) special forms
+ Expressiveness
  + Mutual recursion
  + Metaprogramming (allow implementing #paraphrase[syntactic conveniences] as
    libraries written in the language rather than compiler/interpreter
    features)
+ Performance <c-performance>

Some of these properties are at odds at each other: in particular, it is
difficult to provide mutual recursion and immutability while at the same time
keeping special forms few and simple. For example, Scheme, LISP and other
similar languages forgo the "immutability" constraint, which makes it easy to
implement cyclic data structures like mutually-recursive function definitions
#cite(<sicp>, supplement: "Chapter 4.1.5"). The toplevel expressions in such
languages are usually _statements_ like #ildmono("define") that _mutate_ a global
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

Other LISP-like languages, such as LFE, guarantee immutability of all
data, but handle a lot of the complexity in the interpreter itself: the
language features are written in the host language that implements the
interpreter, and not in the language itself#citneeded. For example, functions defined in
the global namespace are distinct from locally defined lambda objects, and the
interpreter takes special care to support recursion and mutual
recursion without allowing programs to mutate data. In fact, in LFE it is not
even possible to create a cyclic data structure altogether! The price that is
paid to achieve this is that the global namespace of defined functions is not
manipulable by the program (which violates homoiconicity to some
extent) and that #ildmono("define") and similar constructs are special forms.

It is therefore interesting to see if we can design a language that meets all
these goals at the same time. We define a language (which we will call ILD)
which adheres to the #clink(<c-minimality>)[minimality] constraints, and then
demonstrate expressiveness by implementing increasingly-complex constructs in
ILD, including a way to define mutually-recursive functions (@letrec).

In further research, we aim to also meet the
#clink(<c-performance>)[performance] constraint by employing partial evaluation
as an optimisation step, which is a technique known#cite(<hudak>)#cite(<anydsl>) to give good
results for reducing the overhead incurred by metaprogramming.

#set text(size: 0.8em)
#table(
  columns: 7,
  align: (x, y) => if y == 0 { center } else { (left, center, center, left, left, left, left).at(x) },
  table.header(
    [Language], [Immutable], [Continuations], [Special forms], [Macros], [Expansion semantics], [Trigger],
  ),
  [LISP], [no], [yes], [many], [unhygienic], [separate pass], [bind time],
  [Scheme], [no], [yes], [less], [hygienic], [separate pass], [bind time],
  [LFE], [yes], [no], [many], [limited], [separate pass], [bind time],
  [Kraken #cite(<fexpr-pe>)], [yes], [no], [few], [fexprs], [runtime], [bind time],
  [ILD], [yes], [yes], [few], [unhygienic], [runtime], [callsite],
)
