#set document(
  title: [Implementing a module system in a minimal LISP-like language]
)

#import "/lib/paper.typ": paper-template
#show: paper-template

#import "/lib/ild-stuff.typ": ild-template, ildfail, ildsf, ildsym, interop, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont
#show: ild-template

#import "/lib/misc.typ": citneeded, clink, paraphrase, todo, review, note, comment, cases, definition

#set heading(numbering: "1.")

#title()

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
demonstrate expressiveness by #paraphrase[writing] a program in ILD called a
_module loader_ that is able to in turn run programs that are decomposed into
ergonomic to read and write files called _modules_. Functions defined in these
modules can be mutually recursive (@bootstrapping).

== PE for performance <pe-later>
In further research, we aim to also meet the #clink(<c-performance>)[performance]
constraint by showing that the severe performance overhead incurred by
implementing such complex metaprogramming constructs using a very limited set
of base special forms can be significantly reduced by employing partial
evaluation as an optimisation step.

= The language ILD

== Programs and values <values>

Due to ILD being homoiconic, programs and values share the same domain $V$:
$ V = "Sym" union { () } union { (v_1 . v_2) | v_1, v_2 in V } union "SF" union "Ext" union { ildfail(v) | v in V } $

a value being one of:
- Base syntax
  - a _Symbol_ -- $s in "Sym"$
  - a _Pair_ -- $(v_1 . v_2) | v_1, v_2 in V$
  - _Null_ (the empty list) -- $()$
- Non-syntax values
  - an _external value_ -- $phi in "Ext"$ -- opaque to ILD
  - one of the three special forms -- $xi in "SF" = { ildsf("free-vars"), ildsf("quote"), ildsf("macroexpand") }$
  - a _Fail_ -- $ildfail(v) | v in V$ -- signifies failure, carries a value that
    describes the failure

ILD is designed to be embedded#citneeded into a host environment that provides
a set of external data structures. #note[I don't like to call these "external
values", a better term is needed - for example, "host". TODO: rename "external
value to "host value" everywhere, and also rename "builtin" to "host" in the
module loader code] #paraphrase[Inhabitants] of these data structures are
treated as external values (@values), and so are the functions that
operate on them. To facillitate this, external values may be callable, which
means that `apply` is defined for external values that are treated as functions
(@apply).

#comment[Note on lists: since ILD is a LISP, we will use $(v_1, v_2, ..., v_n)$
to denote the value $(v_1 . (v_2 . (... (v_n . ())...)))$, which we will call a
_list_.]

== Syntax
A subset of ILD values, which we call _programs_, can be represented as text:
the syntax is based on standard S-expressions#citneeded with two extra syntax
sugars:
- Quote: `'<expr>` $arrow.r.double.bar$ `(quote <expr>)` -- see @quote
- Macroexpand: `(!<expr1> ... <exprN>)` $arrow.r.double.bar$
  `(macroexpand <expr1> ... <exprN>)` -- used in @macroexpand
Additionally, although formally unnecessary, the parser is assumed to allow syntax
for numeric, string and boolean external value types.

#note[instead of using `foo`, we should define an ILD-specific inline block that
is pretty]

== Semantics of ILD <semantics>

=== Environments
An _environment_ is a finite partial map $rho : "Sym" harpoon.rt V$ that gives semantics
to symbols. Let $"Env"$ be the set of all such environments.

Looking up a symbol in an environment shall be defined as:

$ interop("lookup")_(rho)(s) = cases(
  v & s in "Sym" and rho(s) = v,
  ildfail("<err: unbound symbol>") & s in ("Sym" \\ "dom"rho),
) $

=== Continuations

We define the semantics of ILD in terms of a _continuation monad_
#cite(<wadler>, supplement: [Section 3]), in order to be able to reason about
first-class continuations (@first-class-continuations) and side effects (@side-effects).

#definition[
$contmonad(A, W)$ is the set of computations of type $(W -> A) -> A$, where A
is a set of "answers".

The unit computation is:
$ ret(x) = lambda c (c x) $
The bind operation is defined as:
$ (bindop) : contmonad(A, W) -> (W -> contmonad(A, U)) -> contmonad(A, U) $
$ (phi bindop f) = lambda c (phi (lambda x (f x c))) $

Throughout this paper we use the standard monadic $ildmono("do")$-notation as
sugar for $bindop$:
$ mdo(bind(x_1, m_1), bind(x_2, m_2), ..., bind(x_n, m_n), e) $
stands for the nested binds:
$ m_1 bindop (lambda x_1 (m_2 bindop (lambda x_2 (dots.h m_n bindop (lambda x_n (e)) dots.h)))). $
]

The interpretation of ILD values is defined as:
$ sem(dot)_rho : V -> contmonad(V) $
#comment[
In these definitions, we do not care what the set of answers is, so we will
denote the continuation monad as simply $contmonad(V)$. For a simple, _pure_ version
of the language, passing the identity function to a continuation returned by $sem(dot)$
yields the resulting value as answer. For modeling side-effects, see @side-effects.
]

=== Apply <apply>
We say that certain external values $v in "Ext"$ are _callable_ if $interop("apply")(v, a_1, a_2, ..., a_n) in contmonad(V)$
is defined for some natural $n$ and $v, a_1, a_2, ..., a_n in V$.

We can extend $interop("apply")$ to a total function over $V$ by making it return
a $ildfail("_")$ in cases where it is not defined. We will not cause further
boredom for the reader by formally defining this extension.


=== Eval
Now we can define the evaluation function as so:

$ sem(v)_rho = cases(
  ret(interop("lookup")(rho, v)) & v in "Sym",
  interop("eval-combination")_(rho)(f, a_1, a_2, ..., a_n) & v = (f, a_1, a_2, ... a_n),
  ret(ildfail("<err: cannot eval ()>")) & v = (),
  ret(v) & v in "SF" union "Ext" union { ildfail(w) | w in V },
) $

Symbols are looked up in the environment. Proper lists are evaluated as *combinations*.
Attempts to evaluate improper lists result in a failure. All other values evaluate to
themselves.

To evaluate a combination, we first evaluate its head, and then decide how to proceed
depending on the result:

$ interop("eval-combination")_(rho)(f, accent(a, arrow)) = mdo(bind(phi, sem(f)_(rho)), interop("apply-cases")_(rho)(phi, accent(a, arrow))) $

$ interop("apply-cases")_(rho)(phi, accent(a, arrow)) = cases(
  interop("apply-sf")_(rho)(phi, accent(a, arrow)) & phi in "SF",
  interop("apply-func")_(rho)(phi, accent(a, arrow)) & phi in.not "SF",
) $

If the head evaluated to a special form, use special-form rules (@eval-special-form).
Otherwise, evaluate the list of arguments in order, and pass the resulting list of
values to the function $phi$ via $interop("apply")$:

$ interop("apply-func")_(rho)(phi, a_1, ..., a_n) = mdo(bind(alpha_1, sem(a_1)_rho), ..., bind(alpha_n, sem(a_n)_rho), interop("apply")(phi, alpha_1, ..., alpha_n)) $

#note[why is spacing so tight??]

=== Evaluating special forms <eval-special-form>

==== Quote <quote>
$ interop("apply-sf")_(rho)(ildsf("quote"), v) = ret(v) $

Quote works similarly to other LISP-like languages.

==== Macro expansion <macroexpand>
$ interop("apply-sf")_(rho)(ildsf("macroexpand"), m, accent(a, arrow)) = mdo(bind(mu, sem(m)_(rho)), bind(nu, interop("apply")(mu, accent(a, arrow))), sem(nu)_rho) $

The $ildsf("macroexpand")$ special form allows metaprogramming by treating a certain function
as a _macro_. A regular function evaluation $(f a_1 a_2 ... a_n)$ evaluates f and all
arguments and then passes the evaluated arguments to the evaluated f. In contrast,
$(ildsf("macroexpand") f a_1 a_2 ... a_n)$ evaluates just $f$ and then passes the
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
$interop("apply-sf")_(rho)(ildsf("free-vars"))$ shall return a
list-of-pairs#footnote[For the sake of performance, implementations may use a
more efficient data structure. However, this is not relevant at the moment.]
representation of $rho$.

Since ILD has no function definition special form, $ildsf("free-vars")$ is used
by the lambda macro (@lambda-macro) to capture the binding
environment.

==== All else
$interop("apply-sf")_(rho)$ shall return an appropriate $ildfail(...)$ if given
arguments unlike those listed in the previous sections.

== A minimal host environment
ILD, as defined in @semantics, is useless by itself.
#note[why? show that nothing useful can be computed with just the base language]

We define a host environment $rho$. We will write $(ildsym("foo") v_1 v_2 ... v_n) := v$
to denote that $interop("apply")_(rho)(ildsym("foo"), v_1, v_2, ..., v_n) = ret(v)$ for a
$ildsym("foo") in "Sym"$. Similarly to $interop("apply-sf")$, we assume that the result
of $interop("apply")$ is a $ildfail("...")$ for all improper cases.

=== Boring values
- Access to the special forms
  - $rho(ildsym("quote")) = ildsf("quote")$
  - $rho(ildsym("free-vars")) = ildsf("free-vars")$
  - $rho(ildsym("macroexpand")) = ildsf("macroexpand")$
- Numbers, and associated functions for manipulating them
  - $Q subset "Ext"$
  - $(ildsym("add") x_1 x_2 ... x_n) := x_1 + x_2 + ... + x_n$ for $x_1 ... x_n in Q$
  - #paraphrase[...]
- Functions for working with lists
  - #paraphrase[cons, car, cdr, null?, etc]
- Predicates
  - #paraphrase[is-sym?, is-pair?, sym-eq?, etc]
- Read source
  - $(ildsym("read-source") x)$ is a facillity function that returns an ILD program whose
    name is $x$ (typically implemented by parsing an ILD source file).

=== Abstractions <abstraction>
For ILD to become turing-complete, and, equivalently, a superset of the $lambda$-calculus,
we give it a mechanism for building $lambda$-abstractions.

Let

$ { lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B) | eta in "Env", alpha_1 ... alpha_n in "Sym", B in v} subset "Ext" $

be the set of abstractions. Each abstraction carries a binding environment ($eta$),
a list of formal parameters ($P$) and a body ($B$).

An abstraction is applied by substituting the formal parameters by the #paraphrase[concrete]
operands in the binding environment, and then evaluating the body in the resulting environment:

$ interop("apply")(lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B), a_1, a_2, ..., a_n) = sem(B)_rho $
where
$ rho = eta [ alpha_1 / a_1 ] [ alpha_2 / a_2 ] ... [ alpha_n / a_n ] $

The function $ildsym("mk-lambda")$ shall be provided in order to allow constructing such abstractions:
$ (ildsym("mk-lambda") e P B) := lambda(eta, P, B) $
where $eta$ is an environment constructed from the key-value list $e$
(the opposite operation of the one done in @free-vars)#footnote[Instead of encoding/decoding
environments into key/value lists, we may encode them directly as an external value. High-performance
implementations will do that, but for us it is a stylistic choice.]

#comment[Note that $ildsym("mk-lambda")$ accepts a single body expression instead
of a list of body expressions to be evaluated in order. This is just for the sake
of simplicity/minimality: sequential execution can easily be implemented in the form
of a $ildsym("do")$ procedure.]

=== A recursion operator
A meta-operator $ildsym("poly-fix")$ shall be provided to allow constructing
mutually-recursive functions.

#comment[If we don't care about performance and have infinite memory, the external implementation
of $ildsym("poly-fix")$ is optional, since we can just implement the Y-combinator in ILD itself.
For this exercise, see @poly-fix-Y.]

$ (ildsym("poly-fix") Gamma_1 Gamma_2 ... Gamma_n) := (f_1, f_2, ..., f_n) $

where $f_1, ..., f_n$ are such that:

$ interop("apply")(f_i, a_1, ..., a_n) = mdo(bind(phi, interop("apply")(Gamma_i, f_1, f_2, ..., f_n)), interop("apply")(phi, a_1, ..., a_n)) $

#comment[A stronger definition of $f_1, ..., f_n$ would be $f_i = Gamma_i (f_1, f_2, ..., f_n)$,
but this leads to divergence problems when the language has strict (non-lazy) semantics.#citneeded
Indeed, other strictly-evaluated languages, like Scheme, only support the weaker version
of the recursion operator: `letrec` in Scheme does not allow non-functional right-hand
sides#citneeded. Lazy languages do not have this problem -- for example, the `rec` operator
in Nix has the stroger version of this semantic.#citneeded]

=== Gensym
#note[describe gensym here]

=== First-class continuations <first-class-continuations>
To allow ILD programs to implement complex #paraphrase[flow control], we define a host
function $ildsym("call/cc")$ that passes the current continuation as a first-class
value to a given callable. To do this, we first extend $"Ext"$ with the set of
#paraphrase[first class (reified)] continuations $"Cont"$, such that
$ "Cont" = { ildcont(k) | k : (W -> A) -> A } = { ildcont(k) | k in contmonad(W, A) } $

We can then define the function $ildsym("call/cc")$ such that:
$ interop("apply")(ildsym("call/cc"), f) = lambda k (interop("apply")(f, ildcont(k)) k) $

= Bootstrapping <bootstrapping>
Now that we have defined our minimal language with its minimal host environment,
we can build upon them using metaprogramming and macros to incrementally define
more complex ergonomic syntax.

== Letrec
To define `letrec`, which would allow us to ergonomically write recursive functions,
we must first bootstrap some lower-level primitives.

=== Lambda <lambda-macro>
First, we define a macro called `lambda` that will let us build abstractions (@abstraction)
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

#comment[The `(free-vars)` closure given to the external `mk-lambda` can be replaced
by a closure that contains only `cons`, `mk-lambda`, `quote` and `free-vars`.]

Expansions of the `lambda` macro such as
```ild
(!lambda (x y) (+ x y))
```
will replace themselves by calls to `mk-lambda` such as
```ild
(mk-lambda (free-vars) (quote (x y)) (quote (+ x y)))
```

We can then wrap this definition in another call of `mk-lambda` in order to expose
it as a `lambda` name usable from the body of the external abstraction. For brevity,
we write the definition as an ILD program called `core/bootstrap/lambda-macro.ild`
and then recall it twice (once to define `lambda`, and once to pass itself as the
value of `lambda`):
```
((!(eval (free-vars) (read-source "core/bootstrap/lambda-macro.ild")) (lambda)

  <body that may use the lambda macro>

  ; definition of 'lambda'
  (eval (free-vars) (read-source "core/bootstrap/lambda-macro.ild")))
```

=== Basic utilities

#comment[We do not yet have a mechanism to "define" values other than
using the trick with `lambda` given above, so everything we define from here to
after the definition of `let` would have to be exposed to code that uses it
with nested `lambda` abstractions.]

- `expand-lambda`
```ild
  (!lambda (args body) (cons macroexpand (cons lambda (cons args (cons body '())))))
```
- `list`
- `cadr`

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

To be able to do this more ergonomically, we define a _macro_ called `let`:
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

=== Letrec itself

#note[this section is unfinished]

== The module loader

#note[this section is unfinished]

== An example program
#note[this section is unfinished]

```ild
(module
  (doc "this module calculates the factorial of 5")
  (exports main)
  (imports
    (builtin (macroexpand <= * + lambda))
    ("core/prelude.ild" (if))
    ("core/module-utils.ild" (fn)))
  (defs
    (!fn main () (fact 5))

    (!fn fact (x)
      (!if (<= x 0)
        1
        (* x (fact (+ x -1)))))))
```

== Complex control flow on top of CPS

#note[this section is unfinished]

The following example illustrates early return from the recursive computation
enacted by `map`:
```ild
(!fn try-map (f l)
  (call/cc (!lambda (return)
    (return (map
      (!lambda (x)
        (!if (fail? (f x))
          (return (make-fail (list 'fail-in-element x (f x))))
          (f x))) l)))))
```
If `f` returns failure for an item in the list, the subsequent items will not
be processed.

This technique can also be used to implement mechanisms like scoped try/catch,
iterative loops and other constructs that are separate features in other
languages.

== Side effects <side-effects>

#note[this section is unfinished]

= Appendix

== Polyvariate Y-combinator <poly-fix-Y>
Instead of relying on an external implementation of $ildsym("poly-fix")$, we can
quite elegantly define it as such:

```ild
(!lambda l
  ((!lambda (x) (x x))
    (!lambda (p)
      (map (!lambda (li) (!lambda args (apply (apply li (p p)) args))) l)))))))
```

#bibliography("refs.bib")
