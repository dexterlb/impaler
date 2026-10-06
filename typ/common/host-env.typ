#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, ildmono, ildcont, ild, step, row, dbarrow, evalsto, defas, ildlist, ildapp, ildpair, ildabstr, ildsetsym, ildsethost, ildsetval, ildsetnum, ildsetbool, ildsetstr, ildsetenv, ildsetcont, interop, cpsabstr, cpsapp, ildhost, ildconst, cpsret, ildsetabstr, ildsethostfunc, ildsetcomp
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref

== A minimal host environment <root-env>
The base language, as defined in @semantics, is useless by itself. In this section we constrain
$ildsethost$ to contain data structures, functions and constants that allow implementing non-trivial
programs. To make these values available to programs, we also define a root binding environment $E$
that is used for the outermost eval.

We will use the following notation
to define the semantics of the host functions:

$ defas(E, omega, "foo", v_1, v_2, ..., v_n) $
means that the symbol #ildsym("foo") stands for the host value #ildhost("foo"),
which in turn, when applied to arguments $arrow(v)$, results in the term $omega$.
Within $omega$-terms, we use the marker $cpsret$ to denote the _current continuation_.
Formally, the aforementioned definition of #ildsym("foo") stands for:
#row(
  $ildhost("foo") in ildsethostfunc,$,
  $#evalsto($interop("apply", C, ildhost("foo"), v_1, v_2, dots, v_n)$, $omega[cpsret / C]$),$,
  $E = ildlist(dots, ildpair(ildsym("foo"), ildhost("foo")), dots)$,
)

=== #paraphrase[Boring] values
Let $ildsetnum$, $ildsetbool$, $ildsetstr subset ildsethost$ for the host numbers, booleans
and strings.

- Special forms:
$ E = ildlist(dots, ildpair(ildsym("quote"), ildsf("quote")),
  ildpair(ildsym("macroexpand"), ildsf("macroexpand")),
  ildpair(ildsym("free-vars"), ildsf("free-vars")), dots) $
- Arithmetic on $ildsetnum$ -- $ildapp("+", x_1, ..., x_n)$, $ildapp("*", x_1, ..., x_n)$,
  $ildapp("-", x, y)$, $ildapp("/", x, y)$.
- Comparison, $ildsetnum times ildsetnum -> ildsetbool$ -- $ildsym("=")$, $ildsym("<")$,
  $ildsym(">")$, $ildsym("<=")$, $ildsym(">=")$.
- Pairs -- $defas(E, cpsapp(cpsret, ildpair(a, d)), "cons", a, d)$, $defas(E, cpsapp(cpsret, a), "car", ildpair(a, d))$,
  $defas(E, cpsapp(cpsret, d), "cdr", ildpair(a, d))$.
- Predicates, $ildsetval -> ildsetbool$ -- $ildsym("null?")$, $ildsym("pair?")$,
  $ildsym("symbol?")$, $ildsym("string?")$, $ildsym("func?")$,
  $ildsym("fail?")$
- Equality -- $ildapp("sym-eq?", s_1, s_2)$, $ildapp("str-eq?", s_1, s_2)$
- Failure -- $defas(E, cpsapp(cpsret, ildfail(v)), "make-fail", v)$.
- Branching -- $ildapp("bool-to-k", b)$ returns a function on two arguments that
  returns its first argument if $b$ is true and the second otherwise.

=== Abstractions <abstraction>
For ILD to become Turing-complete, and, equivalently, a superset of the $lambda$-calculus,
we give it a mechanism for building $lambda$-abstractions.

An $ildsetabstr$-term carrying a binding environment $e$,
a list of formal parameters $P$ and a body
#footnote[The body being a single expression instead of a list of body
expressions to be evaluated in order is purely a stylistic choise for the sake
of simplicity.] $beta$
is applied by substituting (@env-substitution) the formal parameters by the actual parameters
in the local binding environment, and then evaluating the body in the resulting environment:
$
  #evalsto(
      $interop(
        "apply", C,
        ildabstr(e, P = ildlist(p_1, p_2, dots, p_n), beta),
        a_1, a_2, ..., a_n
      )$,
      $interop("eval", C, e', beta)$
  )
  text(", where")
  e' = e [ p_1 / a_1 ] [ p_2 / a_2 ] dots [ p_n / a_n ]
$

To let programs build such abstractions, we provide a data constructor:
$ defas(E, cpsapp(cpsret, ildabstr(e, P, B)), "mk-lambda", e, P, B) $

=== A recursion operator
A meta-operator $ildsym("poly-fix")$ shall be provided to allow constructing
mutually-recursive functions
#footnote[If we don't care about performance and have infinite memory, the host implementation
of $ildsym("poly-fix")$ is optional, since we can just implement the Y-combinator in ILD itself.#context if query(<poly-fix-Y>).len() > 0 [ For this exercise, see @poly-fix-Y.]]:

$ defas(E, cpsapp(cpsret, ildlist(ildconst("#f_1"), ildconst("#f_2"), ..., ildconst("#f_n"))), "poly-fix", Gamma_1, Gamma_2, ..., Gamma_n) $

where $ildconst("#f_1"), ..., ildconst("#f_n")$ are such that:

$ #evalsto($interop("apply", C, ildconst("#f_i"), a_1, ..., a_n)$, $interop("apply", cpsabstr(attach(f_i, tr: '), interop("apply", C, attach(f_i, tr: '), a_1, ..., a_n)), Gamma_i, ildconst("#f_1"), ildconst("#f_2"), ..., ildconst("#f_n"))$) $

// #comment[A stronger definition of $ildconst("#f_1"), ..., ildconst("#f_n")$ would be $ildconst("#f_i") = Gamma_i (ildconst("#f_1"), ildconst("#f_2"), ..., ildconst("#f_n"))$,
// but this leads to divergence problems when the language has strict (non-lazy) semantics.
// Indeed, other strictly-evaluated languages, like Scheme, only support the weaker version
// of the recursion operator: `letrec` in Scheme does not allow non-functional right-hand
// sides#citneeded. Lazy languages like Haskell and Nix do not have this problem.#citneeded]

=== Interpreter #paraphrase[access]
Even though formally unnecessary (an ILD interpreter can be implemented in ILD),
it is #paraphrase[useful] to allow programs to call into the interpreter:
- Expose the evaluation terms to programs:
  #row(
    $defas(E, interop("apply", cpsret, f, a_1, dots, a_n), "apply", f, a_1, dots, a_n)$,
    $defas(E, interop("eval", cpsret, e, v), "eval", e, v)$,
  )
  #v(0.5em)
- $ildapp("read-source", p)$ returns the ILD program named
  with the string $p$#footnote[in actual implementations $p$ is a file path and
  $ildsym("read-source")$ parses the file and returns the parsed program.]

=== Effectful computations <side-effects>
The CPS calculus can be extended to model any monadic effect system
#cite(<wadler>, supplement: [Section 3.3]). This includes side effects like IO,
and functions like $ildsym("gensym")$:

$ defas(E, cpsapp(cpsret, <text("a fresh symbol whose prefix is ")s>), "gensym", s) $

=== First-class continuations <first-class-continuations>
To allow ILD programs to implement complex control flow, we define
a host function $ildsym("call/cc")$ that passes the current continuation as a
first-class value to a given callable, and a rule to consume such reified
continuations by discarding the current continuation:
#row(
  $defas(E, interop("apply", f, ildcont(cpsret)), "call/cc", f)$,
  $evalsto(interop("apply", C, ildcont(C'), v), cpsapp(C', v))$,
)

