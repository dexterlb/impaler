#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, interop, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont, ild, step, evalsto, defas
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref

== A minimal host environment
ILD, as defined in @semantics, is useless by itself. We define a host (root)
environment $rho$. We will write #defas($(ildsym("foo") v_1 v_2 ... v_n)$, $v$) to
denote that $#evalsto($interop("apply")(rho(ildsym("foo")), v_1, v_2, ..., v_n)$, $ret(v)$)$ for a
$ildsym("foo") in "Sym"$.

=== #paraphrase[Boring] values
Let $"Num"$, $"Bool"$, $"Str" subset "Host"$ for the host numbers, booleans
and strings.

- Special forms -- $rho(ildsym("quote")) = ildsf("quote")$, and likewise for
  $ildsf("macroexpand")$ and $ildsf("free-vars")$.
- Arithmetic on $"Num"$ -- $(ildsym("+") x_1 ... x_n)$, $(ildsym("*") x_1 ... x_n)$,
  $(ildsym("-") x y)$, $(ildsym("/") x y)$.
- Comparison, $"Num" times "Num" -> "Bool"$ -- $ildsym("=")$, $ildsym("<")$,
  $ildsym(">")$, $ildsym("<=")$, $ildsym(">=")$.
- Pairs -- #defas($(ildsym("cons") a d)$, $(a . d)$), #defas($(ildsym("car") (a . d))$, $a$),
  #defas($(ildsym("cdr") (a . d))$, $d$).
- Predicates, $V -> "Bool"$ -- $ildsym("null?")$, $ildsym("pair?")$,
  $ildsym("symbol?")$, $ildsym("string?")$, $ildsym("func?")$,
  $ildsym("fail?")$
- Equality -- $(ildsym("sym-eq?") s_1 s_2)$, $(ildsym("str-eq?") s_1 s_2)$
- Failure -- #defas($(ildsym("make-fail") v)$, $ildfail(v)$).
- Branching -- $(ildsym("bool-to-k") b)$ returns a function on two arguments that
  returns its first argument if $b$ is true and the second otherwise.

=== Interpreter #paraphrase[access]
- $(ildsym("apply") f (a_1 ... a_n))$ and $(ildsym("eval") e v)$
  expose $interop("apply")$ and $interop("eval")$ to programs, with $e$ an
  environment encoded as returned by $ildsf("free-vars")$ (@semantics-notes).
- $(ildsym("read-source") p)$ parses and returns the ILD program named
  with the string $p$ (in actual implementations $p$ is a file path).

=== Abstractions <abstraction>
For ILD to become Turing-complete, and, equivalently, a superset of the $lambda$-calculus,
we give it a mechanism for building $lambda$-abstractions.

Let

$ { lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B) | eta in "Env", alpha_1 ... alpha_n in "Sym", B in v} subset "Host" $

be the set of abstractions. Each abstraction carries a binding environment ($eta$),
a list of formal parameters ($P$) and a body ($B$).

An abstraction is applied by substituting the formal parameters by the #paraphrase[concrete][supplied]
operands in the binding environment, and then evaluating the body in the resulting environment:

$ #evalsto($interop("apply")(lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B), a_1, a_2, ..., a_n)$, $interop("eval")_(rho)(B)$) $
where
$ rho = eta [ alpha_1 / a_1 ] [ alpha_2 / a_2 ] ... [ alpha_n / a_n ] $

The function $ildsym("mk-lambda")$ shall be provided in order to allow constructing such abstractions:
$ #defas($(ildsym("mk-lambda") e P B)$, $lambda(eta, P, B)$) $
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

$ #defas($(ildsym("poly-fix") Gamma_1 Gamma_2 ... Gamma_n)$, $(f_1, f_2, ..., f_n)$) $

where $f_1, ..., f_n$ are such that:

$ #evalsto($interop("apply")(f_i, a_1, ..., a_n)$, $mdo(bind(phi, interop("apply")(Gamma_i, f_1, f_2, ..., f_n)), interop("apply")(phi, a_1, ..., a_n))$) $

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

$ #defas($(ildsym("gensym") s)$, $<text("a fresh symbol whose prefix is ")s>$) $

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
$ #evalsto($interop("apply")(ildsym("call/cc"), f)$, $lambda k (interop("apply")(f, ildcont(k)) k)$) $
