#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, interop, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont, ild, step, row, evalsto, defas, ildlist, ildapp, ildpair, ildabstr, ildsetsym, ildsethost, ildsetval, ildsetnum, ildsetbool, ildsetstr, ildsetenv, ildsetcont, interpeval, interpcomb, interpapply, ildhost
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref

== A minimal host environment
The base language, as defined in @semantics, is useless by itself. In this section we constrain
$ildsethost$ to contain data structures, functions and constants that allow implementing non-trivial
programs. To make these values available to programs, we also define a root binding environment $Rho$
that is used for the outermost eval.

We will write $defas(Rho, omega, "foo", v_1, v_2, ..., v_n)$ to mean that:
#row(
  $ildhost("foo") in ildsethost,$,
  $#evalsto($interpapply(ildhost("foo"), v_1, v_2, dots, v_n)$, $omega$),$,
  $Rho = ildlist(dots, ildpair(ildsym("foo"), ildhost("foo")), dots)$,
)

=== #paraphrase[Boring] values
Let $ildsetnum$, $ildsetbool$, $ildsetstr subset ildsethost$ for the host numbers, booleans
and strings.

- Special forms -- $rho(ildsym("quote")) = ildsf("quote")$, and likewise for
  $ildsf("macroexpand")$ and $ildsf("free-vars")$.
- Arithmetic on $ildsetnum$ -- $ildapp("+", x_1, ..., x_n)$, $ildapp("*", x_1, ..., x_n)$,
  $ildapp("-", x, y)$, $ildapp("/", x, y)$.
- Comparison, $ildsetnum times ildsetnum -> ildsetbool$ -- $ildsym("=")$, $ildsym("<")$,
  $ildsym(">")$, $ildsym("<=")$, $ildsym(">=")$.
- Pairs -- $defas(Rho, ret(ildpair(a, d)), "cons", a, d)$, $defas(Rho, ret(a), "car", ildpair(a, d))$,
  $defas(Rho, ret(d), "cdr", ildpair(a, d))$.
- Predicates, $ildsetval -> ildsetbool$ -- $ildsym("null?")$, $ildsym("pair?")$,
  $ildsym("symbol?")$, $ildsym("string?")$, $ildsym("func?")$,
  $ildsym("fail?")$
- Equality -- $ildapp("sym-eq?", s_1, s_2)$, $ildapp("str-eq?", s_1, s_2)$
- Failure -- $defas(Rho, ret(ildfail(v)), "make-fail", v)$.
- Branching -- $ildapp("bool-to-k", b)$ returns a function on two arguments that
  returns its first argument if $b$ is true and the second otherwise.

=== Abstractions <abstraction>
For ILD to become Turing-complete, and, equivalently, a superset of the $lambda$-calculus,
we give it a mechanism for building $lambda$-abstractions.
Let

$ { ildabstr(eta, P = (alpha_1, alpha_2, ..., alpha_n), B) | eta in ildsetenv, alpha_1 ... alpha_n in ildsetsym, B in ildsetval} subset ildsethost $

be the set of abstractions. Each abstraction carries a binding environment ($eta$),
a list of formal parameters ($P$) and a body ($B$).
#footnote[The body being a single expression instead of a list of body
expressions to be evaluated in order is purely a stylistic choise for the sake
of simplicity.]

An abstraction is applied by substituting the formal parameters by the actual parameters
in the local binding environment, and then evaluating the body in the resulting environment:

$ #evalsto($interpapply(ildabstr(eta, P = (alpha_1, alpha_2, ..., alpha_n), B), a_1, a_2, ..., a_n)$, $interpeval(rho, B)$) $
where
$ rho = eta [ alpha_1 / a_1 ] [ alpha_2 / a_2 ] ... [ alpha_n / a_n ] $

To let programs build such abstractions, we provide a data contructor:
$ defas(Rho, ret(ildabstr(eta, P, B)), "mk-lambda", eta, P, B) $

=== A recursion operator
A meta-operator $ildsym("poly-fix")$ shall be provided to allow constructing
mutually-recursive functions.

#comment[If we don't care about performance and have infinite memory, the host implementation
of $ildsym("poly-fix")$ is optional, since we can just implement the Y-combinator in ILD itself.#context if query(<poly-fix-Y>).len() > 0 [ For this exercise, see @poly-fix-Y.]]

$ defas(Rho, ret(ildlist(f_1, f_2, ..., f_n)), "poly-fix", Gamma_1, Gamma_2, ..., Gamma_n) $

where $f_1, ..., f_n$ are such that:

$ #evalsto($interpapply(f_i, a_1, ..., a_n)$, $mdo(bind(phi, interpapply(Gamma_i, f_1, f_2, ..., f_n)), interpapply(phi, a_1, ..., a_n))$) $

#comment[A stronger definition of $f_1, ..., f_n$ would be $f_i = Gamma_i (f_1, f_2, ..., f_n)$,
but this leads to divergence problems when the language has strict (non-lazy) semantics.
Indeed, other strictly-evaluated languages, like Scheme, only support the weaker version
of the recursion operator: `letrec` in Scheme does not allow non-functional right-hand
sides#citneeded. Lazy languages like Haskell and Nix do not have this problem.#citneeded]

=== Interpreter #paraphrase[access]
Even though formally unnecessary (an ILD interpreter can be implemented in ILD),
it is #paraphrase[useful] to allow programs to call into the interpreter:
- $ildapp("apply", f, (a_1 ... a_n))$ and $ildapp("eval", e, v)$
  expose $interop("apply")$ and $interop("eval")$ to programs:
  #row(
    $#evalsto($interpapply(ildhost("apply"), f, ildlist(a_1, dots, a_n))$, $interpapply(f, a_1, ..., a_n)$)$,
    $#evalsto($interpapply(ildhost("eval"), e, v)$, $interpeval(e, v)$)$,
  )
  #v(0.5em)
- $ildapp("read-source", p)$ returns the ILD program named
  with the string $p$#footnote[in actual implementations $p$ is a file path and
  $ildsym("read-source")$ parses the file and returns the parsed program.]

== Effectful computations <side-effects>
By choosing the answer set for the continuation monad to be $M(A)$ for another monad $M$,
we can incorporate any effects modelled by $M$ into the CPS semantics of ILD
#cite(<wadler>, supplement: [Section 3.3]). This includes side effects like IO.

=== Gensym
Our host environment shall provide a function $ildsym("gensym")$ such that:

$ defas(Rho, ret(<text("a fresh symbol whose prefix is ")s>), "gensym", s) $

Freshness can be guaranteed by e.g. storing the last generated symbol id in a
State monad wrapper.

=== First-class continuations <first-class-continuations>
Let

$ { ildcont(k) | k : (W -> A) -> A } = { ildcont(k) | k in contmonad(W, A) } subset ildsethost $

Be the set of first-class (reified) continuations.
To allow ILD programs to implement complex control flow, we define
a host function $ildsym("call/cc")$ that passes the current continuation as a
first-class value to a given callable #cite(<wadler>, supplement: [Section 3.2]):

$ defas(Rho, lambda k (interpapply(f, ildcont(k)) k), "call/cc", f) $
