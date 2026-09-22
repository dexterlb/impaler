#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, interop, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont, ild, step
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref

= The language ILD

== Programs and values <values>

We define ILD as a homoiconic language where programs and values share the same domain $V$:
$ V = "Sym" union { () } union { (v_1 . v_2) | v_1, v_2 in V } union "SF" union "Host" union { ildfail(v) | v in V } $
Possible values are:
- Base S-Expression syntax -- symbols ($"Sym"$), pairs and the null list $()$. We will use $(v_1, v_2, ..., v_n)$
to denote the list $(v_1 . (v_2 . (... (v_n . ())...)))$, and use $L_V$ for the set of proper lists.
- Special forms -- $"SF" = { ildsf("free-vars"), ildsf("quote"), ildsf("macroexpand") }$
- Fail objects -- $ildfail(v) | v in V$ -- signify failure, carry a context value
- Host values -- $"Host"$ -- opaque to ILD (@embedding)

== Syntax
A subset of ILD values, which we call _programs_, can be represented as text:
the syntax is based on standard S-expressions#cite(<sexp>) with two extra syntax
sugars:
- Quote: #ild("'<expr>") $arrow.r.double.bar$ #ild("(quote <expr>)") -- see @stepped-semantics
- Macroexpand: #ild("(!<expr1> ... <exprN>)") $arrow.r.double.bar$
  #ild("(macroexpand <expr1> ... <exprN>)") -- used in @macroexpand-mechanism
In addition, although formally unnecessary, the parser is assumed to allow syntax
for numeric, string and boolean host value types.

== Semantics <semantics>

=== Continuations
We define the semantics of ILD in terms of a _continuation monad_
#cite(<wadler>, supplement: [Section 3]), in order to be able to reason about
first-class continuations (@first-class-continuations) and side effects (@side-effects):

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

#comment[
When the answer set is not relevant, we omit it from notation and write $contmonad(V)$ instead of $contmonad(A, V)$.]

=== Environments
An _environment_ is a finite partial map $rho : "Sym" harpoon.rt V$ that gives semantics
to symbols. Let $"Env"$ be the set of all such environments.

=== Embedding <embedding>
ILD is designed to be embedded into a host environment, which supplies the set
$"Host"$ of _host values_: values of the host's data structures, together with
the functions over them. Host values are opaque to ILD: We define the FFI function
$ C: "Host" times L_V -> contmonad(V) $
to give semantics to *calling* a host value with a list of arguments. $C$ can be
assumed to be total (applying a non-callable value yields a $ildfailbare$).

=== Small-step semantics <stepped-semantics>

Environment lookup:
#step($s in "Sym" and rho(s) = v$, $interop("lookup")_(rho)(s)$, $v$)
#step($s in ("Sym" \\ "dom"rho)$, $interop("lookup")_(rho)(s)$, $ildfail("<err: unbound symbol>")$)

Eval:
#step($v in "Sym"$, $interop("eval")_(rho)(v)$, $ret(interop("lookup")(rho, v))$)
#step($v = (f, a_1, a_2, ..., a_n)$, $interop("eval")_(rho)(v)$, $interop("eval-combination")_(rho)(f, a_1, a_2, ..., a_n)$)
#step($v = () or v "is an improper list"$, $interop("eval")_(rho)(v)$, $ret(ildfail("<err: cannot eval improper list>"))$)
#step($v in "SF" union "Host" union { ildfail(w) | w in V }$, $interop("eval")_(rho)(v)$, $ret(v)$)

Combinations:
#step($$, $interop("eval-combination")_(rho)(f, accent(a, arrow))$, $mdo(bind(phi, interop("eval")_(rho)(f)), interop("apply-cases")_(rho)(phi, accent(a, arrow)))$)
#step($phi in "SF"$, $interop("apply-cases")_(rho)(phi, accent(a, arrow))$, $interop("apply-sf")_(rho)(phi, accent(a, arrow))$)
#step($phi in.not "SF"$, $interop("apply-cases")_(rho)(phi, accent(a, arrow))$, $interop("apply-func")_(rho)(phi, accent(a, arrow))$)
#step($$, $interop("apply-func")_(rho)(phi, a_1, ..., a_n)$, $mdo(bind(alpha_1, interop("eval")_(rho)(a_1)), ..., bind(alpha_n, interop("eval")_(rho)(a_n)), interop("apply")(phi, alpha_1, ..., alpha_n))$)

Special forms:
#step($$, $interop("apply-sf")_(rho)(ildsf("quote"), v)$, $ret(v)$)
#step($$, $interop("apply-sf")_(rho)(ildsf("macroexpand"), m, accent(a, arrow))$, $mdo(bind(mu, interop("eval")_(rho)(m)), bind(nu, interop("apply")(mu, accent(a, arrow))), interop("eval")_(rho)(nu))$)
#step($$, $interop("apply-sf")_(rho)(ildsf("free-vars"))$, $rho "as list of pairs"$)

=== Notes on selected cases <semantics-notes>
- $interop("apply-sf")_(rho)(ildsf("free-vars"))$ returns a
  list-of-pairs#footnote[For the sake of performance, implementations may use a
  more efficient data structure.] representation of $rho$. This special form
  is used to capture the binding environment by higher-level constructs like
  the lambda macro (@lambda-macro).
- The head of a combination is always evaluated. If the result of that is a special
  form, the special form is applied on the unevaluated tail of the combination.
  Otherwise, the (non-special) head is applied on the arguments after they have been
  evaluated.
- $(ildsf("macroexpand") f a_1 a_2 ... a_n)$ evaluates just $f$ and then passes the
  *unevaluated* arguments to it. The result is then in turn evaluated. This mechanism
  is discussed in @macroexpand-mechanism.
- All unlisted cases result in a $ildfailbare$.
