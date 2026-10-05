#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, ildmono, ildcont, ild, step, row, rlabel, grules, evalsto, ildlist, ildpair, ildabstr, ildsetval, ildsetprog, ildsetsym, ildsetsexp, ildsetlist, ildsetparlist, ildsetsf, ildsetfail, ildsethost, ildsethostfunc, ildsetnum, ildsetbool, ildsetstr, ildsetabstr, ildsetcont, ildsetvcont, ildsetenv, ildhost, interop, interopword, cpsabstr, cpsapp, ildsetcomp, ildsetvv, ildsetcval, cpsyield
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref

= The base language

== Syntax <syntax>
#grules(
  (
    $ildsetval$,
    $ildsetsym | ildsetsexp | ildsetsf | ildsetfail | ildsethost$,
    [#paraphrase[Tangible] values]
  ),
  ($ildsetsym$, [_symbols_], [], $in$),
  ($ildsetsexp$, $() | ildpair(ildsetval, ildsetval)$, [S-expressions]),
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
    $ildsethost$,
    (
      $ildsetnum | ildsetbool | ildsetstr | ildsetabstr | ildsetvcont |$,
      $ildsethostfunc | ...$
    ),
    [Host values, opaque to the base language (@embedding)]
  ),
  (
    $ildsethostfunc$,
    $ildhost("+") | ildhost("cons") | ildhost("apply") |
      ildhost("call/cc") | ...$,
    [Host functions, opaque to the base language (@embedding)]
  ),
  ($ildsetnum$, [_numbers_], [], $in$),
  ($ildsetbool$, $ildhost("t") | ildhost("f")$),
  ($ildsetstr$, [_strings_], [], $in$),
  (
    $ildsetprog$,
    $ildsetsym | () | ildpair(ildsetprog, ildsetprog) |
      ildsetstr | ildsetnum | ildsetbool$,
    [Surface syntax (programs)]
  ),
  ($ildsetlist$, $() | ildpair(ildsetval, ildsetlist)$, [S-expression _lists_]),
  (
    $ildsetenv$,
    $() | ildpair(ildpair(ildsetsym, ildsetval), ildsetenv)$,
    [Binding environments#footnote[We assume keys are restricted to be unique
      (@stepped-semantics). Furthermore, performant implementations will use
      other representations of binding environments.] (K/V lists)]
  ),
  (
    $ildsetabstr$,
    $ildabstr(ildsetenv, ildsetparlist, ildsetval)$,
    [Abstraction terms, @abstraction]
  ),
  (
    $ildsetparlist$,
    $() | ildpair(ildsetsym, ildsetparlist)$,
    [Param lists (lists of symbols)]
  ),
  (
    $ildsetvcont$,
    $ildcont(ildsetcont)$,
    [Reified continuations, @first-class-continuations],
  ),
  (
    $ildsetcomp$,
    (
      $interop("eval", ildsetcont, ildsetenv, ildsetcval) |$,
      $interop("apply", ildsetcont, ildsethostfunc, (ildsetcval*)) |$,
      $interop("comb", ildsetcont, ildsetenv, ildsetcval, (ildsetcval*)) |$,
      $cpsapp(ildsetcont, ildsetcval)$,
      // applying a continuation to a value yields a computation
    ),
    [CPS calculus computation terms]
  ),
  (
    $ildsetcont$,
    (
      $cpsyield | cpsabstr(ildsetvv, ildsetcomp)$
    ),
    [CPS calculus continuations#footnote[We handwave the details of variable
      renaming to avoid collisions away]]
  ),
  ($ildsetcval$, $ildsetvv | ildsetval$, [CPS calculus value terms]),
  ($ildsetvv$, ${ x_1, x_2, dots }$, [CPS calculus value variables], $in$),
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
the term $interop("eval", cpsyield, E, v)$ for some value $v$, the root
continuation $cpsyield$ and a root binding environment $E$ (@root-env).

=== Embedding <embedding>
#note[this needs to be heavily retold]
ILD is designed to be embedded in a host environment that provides a set of datastructures
and library functions ($ildsethost$). We split the semantics of ILD into:
- Base language, whose semantics are shown in this section
- FFI semantics of the host environment ($ildsethostfunc$) via
  the term $interopword("apply")$. A small host environment, sufficient
  for writing nontrivial programs, is discussed in @root-env.

=== Small-step semantics <stepped-semantics>

#rlabel([Simple cases: ], row(
  step(
    $s in ildsetsym and e = ildlist(dots, ildpair(s, v), dots)$,
    $interop("eval", C, e, s)$, $cpsapp(C, v)$
  ),
  step(
    $v in ildsetsf union ildsethost union ildsetfail$,
    $interop("eval", C, e, v)$, $cpsapp(C, v)$
  ),
))

#rlabel([Combinations: ], [
  #row(evalsto($interop("eval", C, e, ildlist(phi, alpha_1, dots, alpha_n))$,
    $interop("eval", cpsabstr(f, interop("comb", C, e, f, alpha_1, dots, alpha_n)), e, phi)$))
])
#step($f in ildsethostfunc$,
  $interop("comb", C, e, f, alpha_1, ..., alpha_n)$,
  $interop("eval", cpsabstr(x_1, dots.h interop("eval", cpsabstr(x_n, interop("apply", C, f, x_1, dots, x_n)), e, alpha_n) dots.h), e, alpha_1)$)

#rlabel([Special forms: ], [
  #row(
    evalsto($interop("comb", C, e, ildsf("quote"), alpha)$, $cpsapp(C, alpha)$),
    evalsto($interop("comb", C, e, ildsf("free-vars"))$, $cpsapp(C, e)$),
  )
])
#row(evalsto($interop("comb", C, e, ildsf("macroexpand"), phi, accent(alpha, arrow))$,
  $interop("eval", cpsabstr(f, interop("apply", cpsabstr(x, interop("eval", C, e, x)), f, accent(alpha, arrow))), e, phi)$))

#rlabel([CPS calculus $beta$-reduction:], row(evalsto($cpsapp(cpsabstr(x, M), v)$, $M[x / v]$)))

=== Notes on selected cases <semantics-notes>
- $ildsf("free-vars")$ captures the current binding environment and is used by higher-level constructs
  such as the lambda macro (@lambda-macro).
- The head of a combination is always evaluated. If the result of that is a special
  form, the special form is applied on the *unevaluated* remaining elements of the combination.
  If the head is a host function, the operands are first *evaluated* and then the head is applied
  to the resulting arguments.
- $ildlist(ildsf("macroexpand"), phi, alpha_1, alpha_2, ..., alpha_n)$ evaluates just $f$ and then passes the
  *unevaluated* arguments to it. The result is then in turn evaluated. This mechanism
  is discussed in @macroexpand-mechanism.
- Congruence rules are omitted for brevity. Any $ildsetcont$ or $ildsetcomp$ subterm
  is considered to be a reduction hole.
- All unlisted cases result in a $ildfailbare$.
