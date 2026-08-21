#set document(
  title: [Implementing a module system in a minimal LISP-like language]
)

#import "/lib/paper.typ": paper_template
#show: paper_template

#import "/lib/ild-stuff.typ": ild-stuff
#show: ild-stuff

#import "/lib/misc.typ": citneeded, paraphrase, todo, review, note, comment

#set heading(numbering: "1.")

#title()

= Motivation <motivation>

#paraphrase[We wish to build] a programming language that is as minimal as possible
while being expressive enough for general-purpose use. #paraphrase[This] is characterised
by the following properties:
+ Minimality
  + Homoiconicity, provided by LISP-like syntax
  + Immutability
  + Few (and simple) special forms
+ Expressiveness
  + Mutual recursion
  + Metaprogramming (allow implementing #paraphrase[convenience structures] as
    libraries written in the language rather than compiler/interpreter
    features)
+ Performance

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
which adheres to the #paraphrase[ref-to-Minimality] constraints, and then
demonstrate expressiveness by #paraphrase[writing] a program in ILD called a
_module loader_ that is able to in turn run programs that are decomposed into
ergonomic to read and write files called _modules_. Functions defined in these
modules can be #paraphrase[ref-to-mutually-recursive].

== PE for performance <pe-later>
In further research, we aim to also meet the #paraphrase[ref-to-Performance]
constraint by showing that the severe performance overhead incurred by
implementing such complex metaprogramming constructs using a very limited set
of base special forms can be significantly reduced by employing partial
evaluation as an optimisation step.

= The language ILD

== Programs and values

Due to ILD being homoiconic, programs and values share the same domain $V$:
$ V = "Sym" union { () } union { (v_1 . v_2) | v_1, v_2 in V } union "SF" union "Ext" union { "Fail"(v) | v in V } $

a value being one of:
- Base syntax
  - a _Symbol_ -- $s in "Sym"$
  - a _Pair_ -- $(v_1 . v_2) | v_1, v_2 in V$
  - _Null_ (the empty list) -- $()$
- Non-syntax values
  - an _external value_ -- $phi in "Ext"$ -- opaque to ILD
  - one of the three special forms -- $xi in "SF" = { mono("free-vars"), mono("quote"), mono("macroexpand") }$
  - a _Fail_ -- $mono("Fail")(v) | v in V$ -- signifies failure, carries a value that
    describes the failure

ILD is designed to be embedded#citneeded into a host environment that provides
a set of external data structures. #note[I don't like to call these "external
values", a better term is needed] #paraphrase[Inhabitants] of these data
structures are treated as #paraphrase[ref-to-external-values], and so are the
functions that operate on them. To facillitate this, external values may be
callable, which means that `apply` is defined for external values that are
treated as functions #paraphrase[see section explaining how apply works].

#comment[Note on lists: we will use $(v_1, v_2, ..., v_n)$ to denote
the value $(v_1 . (v_2 . (... (v_n . ())...)))$, which we will call a _list_.]

== Syntax
A subset of ILD values, which we call _programs_, can be represented as text:
the syntax is based on standard S-expressions#citneeded with two extra syntax
sugars:
- Quote: `'<expr>` $arrow.r.double.bar$ `(quote <expr>)` -- see #paraphrase[ref-to-section-that-explains-quote]
- Macroexpand: `(!<expr1> ... <exprN>)` $arrow.r.double.bar$
  `(macroexpand <expr1> ... <exprN>)` -- used in #paraphrase[ref-to-section-that-explains-macroexpand]
Additionally, although formally unnecessary, the parser is assumed to allow syntax
for numeric, string and boolean external value types.

#note[instead of using `foo`, we should define an ILD-specific inline block that
is pretty]

== Semantics of ILD <semantics>
=== Environments
An _environment_ is a finite partial map $rho : "Sym" #paraphrase[$- ->$] V$ that gives semantics
to symbols. Let $"Env"$ be the set of all such environments.

Looking up a symbol in an environment shall be defined as:

$ mono("get")(rho, s) = cases(
  v & s in "Sym" and rho(s) = v,
  mono("Fail")("<err: unbound symbol>") & s in ("Sym" \\ "dom"rho),
) $

=== Apply
We say that certain external values $v in "Ext"$ are _callable_ if $mono("apply")(v, a_1, a_2, ..., a_n) in V$
is defined for some natural $n$ and $v, a_1, a_2, ..., a_n in V$.

We can extend $mono("apply")$ to a total function over $V$ by making it return
a $mono("Fail")$ in cases where it is not defined. We will not cause further
boredom for the reader by formally defining this extension.

=== Eval
Now we can define the evaluation function as so:

$ ⟦v⟧_rho = cases(
  mono("get")(rho, v) & v in "Sym",
  mono("apply")(⟦f⟧_rho, ⟦a_1⟧_rho, ..., ⟦a_n⟧_rho) & v = (f, a_1, a_2, ... a_n) and ⟦f⟧_rho in.not "SF",
  mono("apply-sf")(rho, ⟦xi⟧_rho, a_1, ..., a_n) & v = (xi, a_1, a_2, ... a_n) and ⟦xi⟧_rho in "SF",
  mono("Fail")("<err: cannot eval ()>") & v = (),
  v & v in "SF" union "Ext" union { mono("Fail")(w) | w in V },
) $

#note[why is spacing so tight??]

Informally:
- Symbols are looked up in the currently-scoped environment
- Proper nonempty lists are evaluated by first evaluating their head, and then:
  - If the evaluated head is a special form, apply that special form to the *unevaluated* rest of the elements of the list
  - If the evaluated head is not a special form, apply the evaluated head to the *evaluated* rest of the elements of the list
- Trying to evaluate an improper or empty list results in a $mono("Fail")$
- All other values evaluate to themselves

=== Evaluating special forms

==== Quote
$ mono("apply-sf")(rho, mono("quote"), v) = v $

Quote works similarly to other LISP-like languages.

==== Macro expansion
$ mono("apply-sf")(rho, mono("macroexpand"), m, a_1, a_2, ... a_n) = ⟦ mono("apply")(⟦m⟧_rho, a_1, a_2, ..., a_n) ⟧_rho $

The $mono("macroexpand")$ special form allows metaprogramming by treating a certain function
as a _macro_. A regular function evaluation $(f a_1 a_2 ... a_n)$ evaluates f and all
arguments and then passes the evaluated arguments to the evaluated f. In contrast,
$(mono("macroexpand") f a_1 a_2 ... a_n)$ evaluates just $f$ and then passes the
*unevaluated* arguments to it. The result is then in turn evaluated. This allows $f$
to treat the program passed to it as data and to transform it arbitrarily before it
gets evaluated. This is similar to unhygienic macro systems like the one in LISP.
#note[ILD macros are evaluated from outside-in. Is this true for LISP macros?]

Unlike LISP, ILD denotes macro expansion at the callsite rather than differentiating
between _functions_ and _macros_. This is mainly a stylistic choice that greatly
simplifies the semantics and implementation.

Another difference from LISP macros is that ILD does not have a separate macro expansion
phase: instead, macros are evaluated as encountered. We will call this _runtime semantics
of macro expansion_. The astute reader will notice that this defeats one of the
reasons macros are used in the first place, which is to move some code execution
ahead-of-time. We argue that this is not a problem #paraphrase[because in future
research] we extend ILD with another, more powerful, method of AOT code execution,
namely _partial evaluation_ (as per @pe-later).

==== Capturing the binding environment <free-vars>
$mono("apply-sf")(rho, mono("free-vars"))$ shall return a
list-of-pairs#footnote[For the sake of performance, implementations may use a
more efficient data structure. However, this is not relevant at the moment.]
representation of $rho$.

For all other cases, $mono("apply-sf")$ shall return a suitable $mono("Fail")$.

Since ILD has no function definition special form, $mono("free-vars")$ is used
by the lambda macro (#paraphrase[ref-to-the-lambda-macro]) to capture the binding
environment.

== A minimal host environment
ILD, as defined in @semantics, is useless by itself.
#note[why? show that nothing useful can be computed with just the base language]

We define a host environment $rho$. We will write $(mono("foo") v_1 v_2 ... v_n) := v$
to denote that $mono("apply")(rho(mono("foo")), v_1, v_2, ..., v_n) = v$ for a
$mono("foo") in "Sym"$. Similarly to $mono("apply-sf")$, we assume that the result
of $mono("apply")$ is a $mono("Fail")$ for all improper cases.

=== Boring values
- Access to the special forms
  - $rho(mono("quote")) = mono("quote")$
  - $rho(mono("free-vars")) = mono("free-vars")$
  - $rho(mono("macroexpand")) = mono("macroexpand")$
  #note[special forms should be rendered not with mono() but with something that looks different from symbols]
- Numbers, and associated functions for manipulating them
  - $Q subset "Ext"$
  - $(mono("add") x_1 x_2 ... x_n) := x_1 + x_2 + ... + x_n$ for $x_1 ... x_n in Q$
  - #paraphrase[...]
- Functions for working with lists
  - #paraphrase[cons, car, cdr, null?, etc]
- Predicates
  - #paraphrase[is-sym?, is-pair?, sym-eq?, etc]

=== Abstractions <abstraction>
For ILD to become turing-complete, and, equivalently, a superset of the $lambda$-calculus,
we give it a mechanism for building $lambda$-abstractions.

Let

$ { lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B) | eta in "Env", alpha_1 ... alpha_n in "Sym", B in v} subset "Ext" $

be the set of abstractions. Each abstraction carries a binding environment ($eta$),
a list of formal parameters ($P$) and a body ($B$).

An abstraction is applied by substituting the formal parameters by the #paraphrase[concrete]
operands in the binding environment, and then evaluating the body in the resulting environment:

$ mono("apply")(lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B), a_1, a_2, ..., a_n) = ⟦B⟧_rho $
where
$ rho = eta [ alpha_1 / a_1 ] [ alpha_2 / a_2 ] ... [ alpha_n / a_n ] $

The function $mono("mk-lambda")$ shall be provided in order to allow constructing such abstractions:
$ (mono("mk-lambda") e P B) = lambda(eta, P, B) $
where $eta$ is an environment constructed from the key-value list $e$
(the opposite operation of the one done in @free-vars)#footnote[Instead of encoding/decoding
environments into key/value lists, we may encode them directly as an external value. High-performance
implementations will do that, but for us it is a stylistic choice.]

=== A recursion operator
A meta-operator $mono("poly-fix")$ shall be provided to allow constructing
mutually-recursive functions.

#comment[If we don't care about performance and have infinite memory, the external implementation
of $mono("poly-fix")$ is optional, since we can just implement the Y-combinator in ILD itself.
For this exercise, see @poly-fix-Y.]

$ (mono("poly-fix") Gamma_1 Gamma_2 ... Gamma_n) := (f_1, f_2, ..., f_n) $

where $f_1, ..., f_n$ are such that:

$ mono("apply")(f_i, a_1, ..., a_n) = mono("apply")(Gamma_i (f_1, f_2, ..., f_n), a_1, ..., a_n) $

#comment[A stronger definition of $f_1, ..., f_n$ would be $f_i = Gamma_i (f_1, f_2, ..., f_n)$,
but this leads to divergence problems when the language has strict (non-lazy) semantics.#citneeded
Indeed, other strictly-evaluated languages, like Scheme, only support the weaker version
of the recursion operator: `letrec` in Scheme does not allow non-functional right-hand
sides#citneeded. Lazy languages do not have this problem -- for example, the `rec` operator
in Nix has the stroger version of this semantic.#citneeded]

= Bootstrapping <bootstrapping>
Now that we have defined our minimal language with its minimal host environment,
we can build upon them using metaprogramming and macros to incrementally define
more complex ergonomic syntax.

First, we define a macro called `lambda` that will let us build abstractions (@abstraction)
easily:
```ild
(mk-lambda
  (free-vars)
  '(arg-names body)
  '(cons mk-lambda
    (cons (cons free-vars '())                       ; closure, equal to the call site's env
      (cons (cons quote (cons arg-names '()))        ; quoted arg-names
        (cons (cons quote (cons body '()))           ; quoted body
          '())))))
```

#comment[The `(free-vars)` closure given to the external `mk-lambda` can be replaced
by a closure that contains only `cons`, `mk-lambda`, `quote` and `free-vars`.]

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



= Sandbox <sandbox>
```ild
(!foo "bar" bar qux)
```

== Appendix

=== Polyvariate Y-combinator <poly-fix-Y>
Instead of relying on an external implementation of $mono("poly-fix")$, we can
quite elegantly define it as such:

```ild
(!lambda l
  ((!lambda (x) (x x))
    (!lambda (p)
      (map (!lambda (li) (!lambda args (apply (apply li (p p)) args))) l)))))))
```
