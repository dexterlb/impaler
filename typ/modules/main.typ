#set document(
  title: [Implementing a non-trivial module system in a minimal LISP-like programming language]
)

#import "/lib/paper.typ": paper_template
#show: paper_template

#import "/lib/ild-stuff.typ": ild-stuff
#show: ild-stuff

#import "/lib/misc.typ": citneeded, paraphrase, todo

#title()

== Glossary
Henceforth:

- _LLPL_ shall mean "lisp-like programming language" -- informally, a language
  whose programs are S-expressions, is homoiconic and lists get evaluated as
  function calls (combinations)

== Motivation
To write a nontrivial program in a LLPL, one needs to be able to define functions,
constants, and possibly other #paraphrase[items], in a way that allows them to
reference each other. #paraphrase[Even more], all but the most trivial LLPLs also
allow mutual recursion.

A tradeoff is usually#citneeded made between:

+ True immutability
+ Few (and simple) special forms

For example, Scheme (and Lisp, and most "serious" LLPLs, for that matter) allow
_mutability_, thus making it easy to #paraphrase[create] mutually-recursive
data structures, and, #paraphrase[in a special case], mutually-recursive
functions. The toplevel expressions are usually _statements_, such as `define`,
that mutate a global _environment_ which is then seen in the execution scope of
the defined functions during their runtime.

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

LLPLs like Scheme allow implementing most of the language constructs in itself
at the expense of heavy use of mutability in the definitions of core constructs
(macros).

Other LLPLs, such as LFE#citneeded guarantee immutability of all data, but
handle a lot of the complexity in the interpreter itself: the language features
are written in the host language that implements the interpreter, and not in
the language itself. For example, functions defined in the global namespace are
distinct from locally defined lambda objects, and the interpreter takes special
care to #paraphrase[allow] recursion and mutual recursion without allowing
programs to mutate data. In fact, in LFE it is not even possible to create a
cyclic data structure altogether! The price that is paid to achieve this is
that `define` and similar constructs are special forms.

We aim to construct an LLPL that is at the same time:
- Fully immutable
- Expressive enough to write nontrivial programs in
- Minimal at its core (few special forms, none of which deal with state mutation)

After defining such a language, we show a way to decompose programs into
ergonomic _modules_, and define a program called a _module loader_ which
lets us run such #paraphrase[programs].

In further research, we aim to show that the severe performance overhead
incurred by implementing such complex metaprogramming constructs using a very
limited set of base special forms can be significantly reduced by employing
partial evaluation as an optimisation step.

== ILD definition
blah blah symbols, numbers, pairs, lists, blah blah lambda blah blah quote
blah blah macroexpand

=== Base stuff
- symbols, pairs, dots, lists
- numbers and strings (these aren't formally needed)

=== Metaprogramming
- quote
- macroexpand
- mechanism of action
    - macros in ILD, as implemented now, get evaluated from
      the outside towards the inside. Is this different to other LLPLs? Is it better or worse?

== Bootstrapping
First, we need to define `lambda`.
#todo[definition of lambda]

First of all, one would like to be able to define some #paraphrase[items] and
use them in other code. As the reader is probably used to from
#{sym.lambda}-calculus, the most "low-level" way to do that is by using a closure:

```ild
((!lambda (add1 fourtytwo)
    (add1 fourtytwo))

    (lambda (x) (add x 1))  ; definition of add1
    42)                     ; definition of fourtytwo
; produces 43
```

To be able to do this more ergonomically, we define a _macro_ called `let`:
```ild
; definition of "let"
(!lambda (letlist body)
    (cons
        (expand-lambda (map car letlist) body)
        (map cadr letlist)))))
```



== Sandbox
```ild
(!foo "bar" bar qux)
```
