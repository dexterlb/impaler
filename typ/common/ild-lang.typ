#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, interop, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont, ild
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref

= The language ILD

== Programs and values <values>

Due to ILD being homoiconic, programs and values share the same domain $V$:
$ V = "Sym" union { () } union { (v_1 . v_2) | v_1, v_2 in V } union "SF" union "Host" union { ildfail(v) | v in V } $

a value being one of:
- Base syntax
  - a _Symbol_ -- $s in "Sym"$
  - a _Pair_ -- $(v_1 . v_2) | v_1, v_2 in V$
  - _Null_ (the empty list) -- $()$
- Non-syntax values
  - a _host value_ -- $phi in "Host"$ -- opaque to ILD
  - one of the three special forms -- $xi in "SF" = { ildsf("free-vars"), ildsf("quote"), ildsf("macroexpand") }$
  - a _Fail_ -- $ildfail(v) | v in V$ -- signifies failure, carries a value that
    describes the failure

ILD is designed to be embedded#citneeded into a host environment that provides
a set of host data structures. #paraphrase[Inhabitants] of these data
structures are treated as host values (@values), and so are the functions that
operate on them. To facillitate this, host values may be callable, which means
that #ild("apply") is defined for host values that are treated as functions (@apply).

#comment[Note on lists: since ILD is a LISP, we will use $(v_1, v_2, ..., v_n)$
to denote the value $(v_1 . (v_2 . (... (v_n . ())...)))$, which we will call a
_list_.]

== Syntax
A subset of ILD values, which we call _programs_, can be represented as text:
the syntax is based on standard S-expressions#citneeded with two extra syntax
sugars:
- Quote: #ild("'<expr>") $arrow.r.double.bar$ #ild("(quote <expr>)") -- see @quote
- Macroexpand: #ild("(!<expr1> ... <exprN>)") $arrow.r.double.bar$
  #ild("(macroexpand <expr1> ... <exprN>)") -- used in @macroexpand
Additionally, although formally unnecessary, the parser is assumed to allow syntax
for numeric, string and boolean host value types.

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
We say that certain host values $v in "Host"$ are _callable_ if $interop("apply")(v, a_1, a_2, ..., a_n) in contmonad(V)$
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
  ret(v) & v in "SF" union "Host" union { ildfail(w) | w in V },
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
As in traditional LISP macro systems #cite(<kohlbecker1986syntactic>), ILD macros
expand from the outside in: the outermost macro call is expanded first, and its
expansion may itself contain further macro calls.

Unlike LISP, ILD denotes macro expansion at the callsite rather than differentiating
between _functions_ and _macros_. This is mainly a stylistic choice that greatly
simplifies the semantics and implementation.

Another difference from LISP macros is that ILD does not have a separate macro expansion
phase: instead, macros are evaluated as encountered. We will call this _runtime semantics
of macro expansion_. The astute reader will notice that this defeats one of the
reasons macros are used in the first place, which is to move some code execution
ahead-of-time. We argue that this is not a problem #paraphrase[because in future
research] we extend ILD with another, more powerful, method of AOT code execution,
namely _partial evaluation_.

==== Capturing the binding environment <free-vars>
$interop("apply-sf")_(rho)(ildsf("free-vars"))$ shall return a
list-of-pairs#footnote[For the sake of performance, implementations may use a
more efficient data structure. However, this is not relevant at the moment.]
representation of $rho$.

Since ILD has no function definition special form, $ildsf("free-vars")$ is used
by the lambda macro (@lambda-macro) to capture the binding
environment.

==== All else
$interop("apply-sf")_(rho)$ shall return an appropriate $ildfailbare$ if given
arguments unlike those listed in the previous sections.

== A minimal host environment
ILD, as defined in @semantics, is useless by itself (all programs are either
basic values that evaluate to themselves or evaluate to a $ildfailbare$).

We define a host environment $rho$. We will write $(ildsym("foo") v_1 v_2 ... v_n) := v$
to denote that $interop("apply")_(rho)(ildsym("foo"), v_1, v_2, ..., v_n) = ret(v)$ for a
$ildsym("foo") in "Sym"$. Similarly to $interop("apply-sf")$, we assume that the result
of $interop("apply")$ is a $ildfail("...")$ for all improper cases.

=== #paraphrase[Boring] values
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
  - $(ildsym("read-source") x)$ is a facillity function that returns an ILD program whose
    name is $x$ (typically implemented by parsing an ILD source file).

=== Abstractions <abstraction>
For ILD to become turing-complete, and, equivalently, a superset of the $lambda$-calculus,
we give it a mechanism for building $lambda$-abstractions.

Let

$ { lambda(eta, P = (alpha_1, alpha_2, ..., alpha_n), B) | eta in "Env", alpha_1 ... alpha_n in "Sym", B in v} subset "Host" $

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
in Nix has the stroger version of this semantic.#citneeded]

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
To allow ILD programs to implement complex #paraphrase[flow control], we define
a host function $ildsym("call/cc")$ that passes the current continuation as a
first-class value to a given callable #cite(<wadler>, supplement: [Section 3.2]).
To do this, we first extend $"Host"$ with the set of #paraphrase[first class
(reified)] continuations $"Cont"$, such that $ "Cont" = { ildcont(k) | k : (W
-> A) -> A } = { ildcont(k) | k in contmonad(W, A) } $

We can then define the function $ildsym("call/cc")$ such that:
$ interop("apply")(ildsym("call/cc"), f) = lambda k (interop("apply")(f, ildcont(k)) k) $
