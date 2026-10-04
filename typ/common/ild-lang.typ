#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont, ild, step, row, grules, evalsto, ildlist, ildpair, ildabstr, ildsetval, ildsetprog, ildsetsym, ildsetsexp, ildsetlist, ildsetparlist, ildsetsf, ildsetfail, ildsethost, ildsethostfunc, ildsetnum, ildsetbool, ildsetstr, ildsetabstr, ildsetcont, ildsetcv, ildsetcc, ildsetenv, ildhost, interop, interopword, cpsabstr, cpsapp, ildsetcomp, ildsetansw, ildsetvv, ildsetcval, cpsyield
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref, cong

= The base language

== Syntax <syntax>

#grules(
  (
    $ildsetval$,
    $ildsetsym | ildsetsexp | ildsetsf | ildsetfail | ildsethost$,
    [#paraphrase[Tangible] values]
  ),
  ($ildsetsym$, [_symbols_]),
  ($ildsetsexp$, $() | ildpair(ildsetval, ildsetval)$, [S-expressions]),
  ($ildsetlist$, $() | ildpair(ildsetval, ildsetlist)$, [S-expression _lists_]),
  (
    $ildsetparlist$,
    $() | ildpair(ildsetsym, ildsetparlist)$,
    [Param lists (lists of symbols)]
  ),
  (
    $ildsetenv$,
    $() | ildpair(ildpair(ildsetsym, ildsetval), ildsetenv)$,
    [Binding environments#footnote[We assume keys are restricted to be unique
      (@stepped-semantics). Furthermore, performant implementations will use
      other representations of binding environments.] (K/V lists)]
  ),
  (
    $ildsetsf$,
    $ildsf("free-vars") | ildsf("quote") | ildsf("macroexpand")$,
    [Special forms]
  ),
  (
    $ildsetfail$,
    $ildfail(ildsetval)$,
    [Failure objects, carrying a context value]
  ),
  (
    $ildsethostfunc$,
    $ildhost("+") | ildhost("cons") | ildhost("apply") |
      ildhost("call/cc") | ...$,
    [Host functions, opaque to the base language (@embedding)]
  ),
  (
    $ildsethost$,
    (
      $ildsetnum | ildsetbool | ildsetstr | ildsetabstr | ildsetcont |$,
      $ildsethostfunc | ...$
    ),
    [Host values, opaque to the base language (@embedding)]
  ),
  ($ildsetnum$, [_numbers_]),
  ($ildsetbool$, $ildhost("t") | ildhost("f")$),
  ($ildsetstr$, [_strings_]),
  (
    $ildsetabstr$,
    $ildabstr(ildsetenv, ildsetparlist, ildsetval)$,
    [Abstraction terms, @abstraction]
  ),
  (
    $ildsetcont$,
    $ildcont(ildsetcont)$,
    [Reified continuations, @first-class-continuations],
  ),
  (
    $ildsetprog$,
    $ildsetsym | () | ildpair(ildsetprog, ildsetprog) |
      ildsetstr | ildsetnum | ildsetbool$,
    [Surface syntax (programs)]
  ),
  ($ildsetvv$, $x_1, x_2, dots$, [CPS calculus value variables]),
  ($ildsetcval$, $ildsetvv | ildsetval$, [CPS calculus value terms]),
  (
    $ildsetcomp$,
    (
      $interop("eval", ildsetcont, ildsetenv, ildsetcval) |$,
      $interop("apply", ildsetcont, ildsethostfunc, (ildsetcval*)) |$,
      $interop("comb", ildsetcont, ildsetenv, ildsetcval, (ildsetcval*)) |$,
      $cpsapp(ildsetcont, ildsetcval)$,  // applying a continuation to a value yields a computation
    ),
    [CPS calculus computation terms]
  ),
  (
    $ildsetcont$,
    (
      $cpsyield | cpsabstr(ildsetvv, ildsetcomp)$
    ),
    [CPS calculus continuations#footnote[We handwave the details of variable renaming to avoid collisions away]]
  ),
)

We define ILD as a homoiconic language where parseable programs ($ildsetprog$)
are a subset of the internal syntax $ildsetval$. Source code is parsed as
standard S-expressions augmented with the following syntax sugars:
- Quote: #ild("'<expr>") $arrow.r.double.bar$ #ild("(quote <expr>)")
- Macroexpand: #ild("(!<expr1> ... <exprN>)") $arrow.r.double.bar$
  #ild("(macroexpand <expr1> ... <exprN>)")
We also introduce a set of computation terms ($ildsetcomp$) that facillitate
the operational semantics.

== Semantics <semantics>
We define the semantics of ILD in terms of a CPS calculus based on
the #paraphrase[untyped][semi-typed?] $lambda$-calculus
over the set of _computations_ ($ildsetcomp$). Evaluation starts from
the term $interop("eval", cpsyield, Rho, v)$ for some value $v$, the root
continuation $cpsyield$ and a root binding environment $Rho$ (@root-env).

=== Embedding <embedding>
#note[this needs to be heavily retold]
ILD is designed to be embedded in a host environment that provides a set of datastructures
and library functions ($ildsethost$). We split the semantics of ILD into:
- Base language, whose semantics are shown in this section
- FFI semantics of the host environment ($ildsethostfunc$) via
  the term $interopword("apply")$. A small host environment, sufficient
  for writing nontrivial programs, is discussed in @root-env.

=== Small-step semantics <stepped-semantics>

Simple cases:
#row(
  step(
    $s in ildsetsym and rho = ildlist(dots, ildpair(s, v), dots)$,
    $interop("eval", C, rho, s)$, $cpsapp(C, v)$
  ),
  step(
    $v in ildsetsf union ildsethost union ildsetfail$,
    $interop("eval", C, rho, v)$, $cpsapp(C, v)$
  ),
)


Evaluating a combination (list): eval the head, decide what to do depending on result:
#row(evalsto($interop("eval", C, rho, ildlist(f, a_1, dots, a_n))$,
  $interop("eval", cpsabstr(phi, interop("comb", C, rho, phi, a_1, dots, a_n)), rho, f)$))

#step($phi in ildsethostfunc$,
  $interop("comb", C, rho, phi, a_1, ..., a_n)$,
  $interop("eval", cpsabstr(alpha_1, dots.h interop("eval", cpsabstr(alpha_n, interop("apply", C, phi, alpha_1, dots, alpha_n)), rho, a_n) dots.h), rho, a_1)$)

Special forms:
#row(
  evalsto($interop("comb", C, rho, ildsf("quote"), v)$, $cpsapp(C, v)$),
  evalsto($interop("comb", C, rho, ildsf("free-vars"))$, $cpsapp(C, rho)$),
)
#row(evalsto($interop("comb", C, rho, ildsf("macroexpand"), m, accent(a, arrow))$,
  $interop("eval", cpsabstr(mu, interop("apply", cpsabstr(nu, interop("eval", C, rho, nu)), mu, accent(a, arrow))), rho, m)$))

=== Notes on selected cases <semantics-notes>
- $interop("comb", C, rho, ildsf("free-vars"))$ returns $rho$. This special form
  is used to capture the binding environment by higher-level constructs like
  the lambda macro (@lambda-macro).
- The head of a combination is always evaluated. If the result of that is a special
  form, the special form is applied on the unevaluated tail of the combination.
  If the head is a host function, the operands are evaluated and then the head is applied
  to the resulting arguments.
- $ildlist(ildsf("macroexpand"), f, a_1, a_2, ..., a_n)$ evaluates just $f$ and then passes the
  *unevaluated* arguments to it. The result is then in turn evaluated. This mechanism
  is discussed in @macroexpand-mechanism.
- All unlisted cases result in a $ildfailbare$.
