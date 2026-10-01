#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, interop, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont, ild, step, row, evalsto, ildlist, ildpair, ildsetsym, ildsetlist, ildsetsf, ildsetfail, ildsethost, ildsetenv, interpeval, interpcomb, interpapply
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref, cong

= The language ILD

== Programs and values <values>

We define ILD as a homoiconic language where programs and values share the same domain $V$:
$ V cong ildsetsym union ildsetlist union ildsetsf union ildsetfail union ildsethost $
Where:
- $ildsetsym$ is the set of _symbols_ (as in standard S-expressions)
- $ildsetlist cong { () } union { ildpair(v_1, v_2) | v_1, v_2 in V }$ is the set of S-expression _lists_
- $ildsetsf = { ildsf("free-vars"), ildsf("quote"), ildsf("macroexpand") }$ is the set of _special forms_
- $ildsetfail = { ildfail(v) | v in V }$ is the set of _failure objects_ (each carries a context value)
- $ildsethost$ is the set of _host values_, which are opaque to the base language (@embedding)

== Syntax
A subset of ILD values can be represented as text. We call such values _programs_.
The syntax is based on standard S-expressions#cite(<sexp>) with two extra syntax
sugars:
- Quote: #ild("'<expr>") $arrow.r.double.bar$ #ild("(quote <expr>)")
- Macroexpand: #ild("(!<expr1> ... <exprN>)") $arrow.r.double.bar$
  #ild("(macroexpand <expr1> ... <exprN>)")

In addition, although formally unnecessary, the parser is assumed to allow syntax
for numeric, string and boolean host value types.

== Semantics <semantics>

We define the semantics of ILD in terms of a _continuation monad_ with unit
$ret : V -> contmonad(A, V)$ and the standard do-notation:
$ mdo(bind(x_1, m_1), bind(x_2, m_2), ..., bind(x_n, m_n), e) $
For more details, see @continuation-monad.

=== Binding environments
A _binding environment_ is a key-value list $ildlist(ildpair(a_1, alpha_1), dots, ildpair(a_n, alpha_n)) in ildsetlist$
where $a_i eq.not a_j "for" i eq.not j$ and ${a_1, dots, a_n} subset ildsetsym$.
#footnote[
  Performant implementations will use other representations of environments
]
Throughout this paper, we will use $rho$ to denote a binding environment.

=== Embedding <embedding>
ILD is designed to be embedded into a host environment, which supplies the set
$ildsethost$ of _host values_: values of the host's data structures, together with
the functions over them. Host values are opaque to ILD: We define FFI semantics
for some host values like
$ #evalsto($interpapply(v, a_1, a_2, ..., a_n)$, $omega$) $
to denote that _calling_ the host value $v$ with arguments $a_1 ... a_n$ results
in the computation $omega$.

=== Small-step semantics <stepped-semantics>

Simple cases:
#row(
  step($s in ildsetsym and rho = ildlist(dots, ildpair(s, v), dots)$, $interpeval(rho, s)$, $ret(v)$),
  step($v in ildsetsf union ildsethost union ildsetfail$, $interpeval(rho, v)$, $ret(v)$),
)


Evaluating a combination (list): eval the head, decide what to do depending on result:
#step($$, $interpeval(rho, ildlist(f, a_1, dots, a_n))$, $mdo(bind(phi, interpeval(rho, f)), interpcomb(rho, phi, a_1, dots, a_n))$)

#step($phi in ildsethost$, $interpcomb(rho, phi, a_1, ..., a_n)$, $mdo(bind(alpha_1, interpeval(rho, a_1)), ..., bind(alpha_n, interpeval(rho, a_n)), interpapply(phi, alpha_1, ..., alpha_n))$)

Special forms:
#row(
  step($phi = ildsf("quote")$, $interpcomb(rho, phi, v)$, $ret(v)$),
  step($phi = ildsf("free-vars")$, $interpcomb(rho, phi)$, $ret(rho)$),
)
#step($phi = ildsf("macroexpand")$, $interpcomb(rho, phi, m, accent(a, arrow))$, $mdo(bind(mu, interpeval(rho, m)), bind(nu, interpapply(mu, accent(a, arrow))), interpeval(rho, nu))$)

=== Notes on selected cases <semantics-notes>
- $interpcomb(rho, ildsf("free-vars"))$ returns $rho$. This special form
  is used to capture the binding environment by higher-level constructs like
  the lambda macro (@lambda-macro).
- The head of a combination is always evaluated. If the result of that is a special
  form, the special form is applied on the unevaluated tail of the combination.
  If the head is a host value, the operands are evaluated and then the head is applied
  to the resulting arguments.
- $ildlist(ildsf("macroexpand"), f, a_1, a_2, ..., a_n)$ evaluates just $f$ and then passes the
  *unevaluated* arguments to it. The result is then in turn evaluated. This mechanism
  is discussed in @macroexpand-mechanism.
- All unlisted cases result in a $ildfailbare$.
