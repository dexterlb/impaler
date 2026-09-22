#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, interop, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont, ild, step, stepfol
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref

== A minimal host environment
ILD, as defined in @semantics, is useless by itself (all programs are either
basic values that evaluate to themselves or evaluate to a $ildfailbare$).

We define a host environment $rho$. We will write $(ildsym("foo") v_1 v_2 ...
v_n) := v$ to denote that $C(rho(ildsym("foo")), v_1, v_2, ..., v_n) = ret(v)$
for a $ildsym("foo") in "Sym"$.

=== #paraphrase[Boring][Primitive] values
- Access to the special forms
  - $rho(ildsym("quote")) = ildsf("quote")$
  - $rho(ildsym("free-vars")) = ildsf("free-vars")$
  - $rho(ildsym("macroexpand")) = ildsf("macroexpand")$
- Numbers, and associated functions for manipulating them
  - $Q subset "Host"$
  - $(ildsym("add") x_1 x_2 ... x_n) := x_1 + x_2 + ... + x_n$ for $x_1 ... x_n in Q$
  - #paraphrase[...]
- Functions for working with lists
  - #paraphrase[cons, car, cdr, null?, etc]
- Predicates
  - #paraphrase[is-sym?, is-pair?, sym-eq?, etc]
- Read source
  - $(ildsym("read-source") x)$ is a facility function that returns an ILD program whose
    name is $x$ (typically implemented by parsing an ILD source file).

=== Abstractions <abstraction>
For ILD to become Turing-complete, and, equivalently, a superset of the $lambda$-calculus,
we give it a mechanism for building $lambda$-abstractions.

Let

$ { lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B) | eta in "Env", alpha_1 ... alpha_n in "Sym", B in v} subset "Host" $

be the set of abstractions. Each abstraction carries a binding environment ($eta$),
a list of formal parameters ($P$) and a body ($B$).

An abstraction is applied by substituting the formal parameters by the #paraphrase[concrete][supplied]
operands in the binding environment, and then evaluating the body in the resulting environment:

$ interop("apply")(lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B), a_1, a_2, ..., a_n) = sem(B)_rho $
where
$ rho = eta [ alpha_1 / a_1 ] [ alpha_2 / a_2 ] ... [ alpha_n / a_n ] $

The function $ildsym("mk-lambda")$ shall be provided in order to allow constructing such abstractions:
$ (ildsym("mk-lambda") e P B) := lambda(eta, P, B) $
where $eta$ is an environment constructed from the key-value list $e$
(the opposite operation of the one done in @semantics-notes)#footnote[Instead of encoding/decoding
environments into key/value lists, we may encode them directly as a host value. High-performance
implementations will do that, but for us it is a stylistic choice.]

#comment[Note that $ildsym("mk-lambda")$ accepts a single body expression instead
of a list of body expressions to be evaluated in order. This is just for the sake
of simplicity/minimality: sequential execution can easily be implemented in the form
of a $ildsym("do")$ procedure.]

=== A recursion operator
A meta-operator $ildsym("poly-fix")$ shall be provided to allow constructing
mutually-recursive functions.

#comment[If we don't care about performance and have infinite memory, the host implementation
of $ildsym("poly-fix")$ is optional, since we can just implement the Y-combinator in ILD itself.#context if query(<poly-fix-Y>).len() > 0 [ For this exercise, see @poly-fix-Y.]]

$ (ildsym("poly-fix") Gamma_1 Gamma_2 ... Gamma_n) := (f_1, f_2, ..., f_n) $

where $f_1, ..., f_n$ are such that:

$ interop("apply")(f_i, a_1, ..., a_n) = mdo(bind(phi, interop("apply")(Gamma_i, f_1, f_2, ..., f_n)), interop("apply")(phi, a_1, ..., a_n)) $

#comment[A stronger definition of $f_1, ..., f_n$ would be $f_i = Gamma_i (f_1, f_2, ..., f_n)$,
but this leads to divergence problems when the language has strict (non-lazy) semantics.#citneeded
Indeed, other strictly-evaluated languages, like Scheme, only support the weaker version
of the recursion operator: `letrec` in Scheme does not allow non-functional right-hand
sides#citneeded. Lazy languages do not have this problem -- for example, the `rec` operator
in Nix has the stronger version of this semantic.#citneeded]

== Effectful computations <side-effects>
By choosing the answer set for the continuation monad to be $M(A)$ for another monad $M$,
we can incorporate any effects modelled by $M$ into the CPS semantics of ILD
#cite(<wadler>, supplement: [Section 3.3]). This includes side effects like IO.

=== Gensym
Our host environment shall provide a function $ildsym("gensym")$ such that:

$ (ildsym("gensym") s) := <text("a fresh symbol whose prefix is ")s> $

Freshness can be guaranteed by e.g. storing the last generated symbol id in a
State monad wrapper.

=== First-class continuations <first-class-continuations>
To allow ILD programs to implement complex #paraphrase[flow control][control flow], we define
a host function $ildsym("call/cc")$ that passes the current continuation as a
first-class value to a given callable #cite(<wadler>, supplement: [Section 3.2]).
To do this, we first extend $"Host"$ with the set of #paraphrase[first class
(reified)][reified, first-class] continuations $"Cont"$, such that $ "Cont" = { ildcont(k) | k : (W
-> A) -> A } = { ildcont(k) | k in contmonad(W, A) } $

We can then define the function $ildsym("call/cc")$ such that:
$ interop("apply")(ildsym("call/cc"), f) = lambda k (interop("apply")(f, ildcont(k)) k) $
