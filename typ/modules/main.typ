#set document(
  title: [Implementing a module system in a minimal LISP-like language]
)

#import "/lib/paper.typ": paper_template
#show: paper_template

#import "/lib/ild-stuff.typ": ild-stuff
#show: ild-stuff

#import "/lib/misc.typ": citneeded, paraphrase, todo, review, note

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

In further research, we aim to also meet the #paraphrase[ref-to-Performance]
constraint by showing that the severe performance overhead incurred by
implementing such complex metaprogramming constructs using a very limited set
of base special forms can be significantly reduced by employing partial
evaluation as an optimisation step.

= ILD definition

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

#review[
== Environments and evaluation

An _environment_ $rho$ is a finite partial map from symbols to values. There is
no mutation: evaluation never changes $rho$, it only consults and extends copies
of it. Evaluation is a _total_ function $lr(⟦dot.c⟧)_rho : V -> V$ -- there is no
separate error channel, because an ill-formed evaluation simply yields a
$mono("Fail")$ value:

$ lr(⟦ e ⟧)_rho = cases(
  e & quad e "is a number, string, boolean," mono("Fail")", procedure" phi "or special form" xi "(self-evaluating)",
  rho(s) & quad e = s "and" s in "dom" rho,
  mono("Fail")(mono("\"unbound\""); s) & quad e = s "and" s in.not "dom" rho,
  mono("Fail")(mono("\"eval-null\"")) & quad e = (),
  mono("Fail")(mono("\"eval-pair\""); e) & quad e = (a . d) "improper",
  C lr([ (f #h(2pt) a_1 #h(2pt) dots.c #h(2pt) a_n) ], size: #120%)_rho & quad e = (f #h(2pt) a_1 #h(2pt) dots.c #h(2pt) a_n) "a proper list," n >= 0,
) $

Crucially $()$ and improper pairs are _not_ self-evaluating: data that happens to
look like a form must be `quote`d. This is exactly what gives a proper list its
meaning as a _combination_.

To evaluate a combination $C lr([(f #h(2pt) a_1 #h(2pt) dots.c #h(2pt) a_n)])_rho$
the head is evaluated first, $h = lr(⟦ f ⟧)_rho$, and then one of two rules fires
depending on what $h$ is:

$ C lr([(f #h(2pt) a_1 #h(2pt) dots.c #h(2pt) a_n)])_rho = cases(
  xi(rho; #h(2pt) a_1, dots.c, a_n) & quad h = xi "a special form",
  mono("apply")(h, #h(2pt) [lr(⟦ a_1 ⟧)_rho, dots.c, lr(⟦ a_n ⟧)_rho]) & quad "otherwise.",
) $

A special form $xi$ receives its operands _unevaluated_, together with the
environment, and dictates everything that follows (the three forms are defined
below). Otherwise the operands are _evaluated_, left to right, and handed to
$mono("apply")$, which yields a result only for a procedure $phi$:
$ mono("apply")(h, [v_1, dots.c, v_n]) = cases(
  "the value" phi "yields on" [v_1, dots.c, v_n] & quad h = phi "a procedure",
  mono("Fail")(mono("\"cannot-apply\""); h) & quad "otherwise.",
) $
A procedure $phi$ is precisely a value that carries such a rule; applying
anything else (a number, a pair, a $mono("Fail")$) is itself a $mono("Fail")$, so
a failed operator surfaces as a failure rather than being silently ignored.

Because ILD has no mutable state and no side effects, evaluation is _pure_: the
left-to-right order of operand evaluation is unobservable, and a $mono("Fail")$
does not short-circuit a combination -- an operand that evaluates to a failure is
handed to the procedure like any other argument, and propagating it is the
procedure's responsibility by convention. The base language contributes _no_
procedures at all; every $phi$ a program can name originates in the initial
environment. The core provides only the rule above and the following three
special forms $xi$.
]

#review[
== Special forms <special-forms>

A _special form_ $xi$ is a distinguished head value that suspends the default
operand-evaluation rule. The base of ILD has exactly three, and none of them
mutates anything.

*`quote`* returns its single operand verbatim,
$ mono("quote")(rho; #h(2pt) x) = x, $
turning a fragment of program into inert data (surface syntax #raw("'x", lang: "ild")).

*`free-vars`* reifies the current environment as an association list,
$ mono("free-vars")(rho; #h(2pt)) = ((s_1 . rho(s_1)) #h(3pt) dots.c #h(3pt) (s_k . rho(s_k))), quad {s_1, dots.c, s_k} = "dom" rho $
(the order of pairs is unspecified). It is the only way a running program can
capture the bindings visible at a point as a first-class value; @bootstrapping
relies on it to let a macro-produced closure snapshot its definition environment.

*`macroexpand`* is the sole metaprogramming primitive. On operands
$m, a_1, dots.c, a_n$ (surface syntax #raw("!m a_1 … a_n", lang: "ild")) it is
defined by
$ mono("macroexpand")(rho; #h(2pt) m, a_1, dots.c, a_n) = lr(⟦ #h(2pt) mono("apply")(lr(⟦ m ⟧)_rho, #h(2pt) [a_1, dots.c, a_n]) #h(2pt) ⟧)_rho. $
Read inside-out: evaluate $m$ to a procedure $mu$ (the _macro_); apply $mu$ to the
_unevaluated_ operand forms to obtain an _expansion_ $x$, itself an expression;
then evaluate $x$ in the _caller's_ environment $rho$.

A macro is therefore nothing more than an ordinary function from syntax to
syntax. `macroexpand` supplies only the two ingredients that make it a macro --
"do not evaluate my operands" and "do evaluate my result, here, now" -- so ILD
needs neither a separate class of macro values nor a distinct expansion phase.
]

#review[
== Mechanism of action: expansion is evaluation

Expansion proceeds _outside-in_. `(macroexpand m …)` runs the outermost macro
`m` first; any `macroexpand` forms sitting _inside_ the resulting expansion are
untouched until the final $lr(⟦x⟧)_rho$ reaches them. Since that last step is an
ordinary evaluation, a macro that expands into further macro calls simply has
those calls expanded as evaluation walks into them. There is no fixed-point
pre-pass over the whole form: in ILD, macro expansion and evaluation are _one and
the same traversal_.

The outermost-first _ordering_ is shared with Lisp and Scheme#citneeded. What
differs is _phase_: those languages expand macros completely, ahead of time, in a
dedicated expander with its own notion of environment, and only then evaluate the
fully expanded form. ILD has no such separation -- the head `m` of a macro call
is an ordinary value produced by evaluation, so macros are first-class (they can
be passed, returned, and built by other macros) and live in the same single
namespace and environment as everything else.

This buys a genuinely minimal core at two costs. First, there is no hygiene: an
expansion sees whatever the caller's environment holds, and any discipline over
name capture must be arranged explicitly by the macro author, typically via
`free-vars`. Second, because a form is re-expanded every time evaluation reaches
it rather than once in advance, repeatedly evaluated code pays for its expansion
repeatedly -- the performance overhead whose removal, by partial evaluation, we
identify as the target of further research (@motivation).
]

= Bootstrapping <bootstrapping>
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



= Sandbox <sandbox>
```ild
(!foo "bar" bar qux)
```

== Glossary
Henceforth:

- _LLPL_ shall mean "lisp-like programming language" -- informally, a language
  whose programs are S-expressions, is homoiconic and lists get evaluated as
  function calls (combinations)
