#import "/lib/ild-stuff.typ": ildfail, ildfailbare, ildsf, ildsym, sem, contmonad, retbare, ret, bind, mdo, bindop, ildmono, ildcont, ild, step, row, grules, evalsto, ildlist, ildpair, ildabstr, ildsetval, ildsetprog, ildsetsym, ildsetsexp, ildsetlist, ildsetparlist, ildsetsf, ildsetfail, ildsethost, ildsethostfunc, ildsetnum, ildsetbool, ildsetstr, ildsetabstr, ildsetcont, ildsetcv, ildsetcc, ildsetenv, ildhost, interop, interopword, cpsabstr, cpsapp
#import "/lib/misc.typ": citneeded, paraphrase, note, comment, cases, definition, optref, cong

= The base language

== Syntax <syntax>

#grules(
  (
    $ildsetval$,
    $ildsetsym | ildsetsexp | ildsetsf | ildsetfail | ildsethost$,
    [values and programs]
  ),
  ($ildsetsym$, [_symbols_]),
  ($ildsetsexp$, $() | ildpair(ildsetval, ildsetval)$, [S-expressions]),
  ($ildsetlist$, $() | ildpair(ildsetval, ildsetlist)$, [S-expression _lists_]),
  (
    $ildsetparlist$,
    $() | ildpair(ildsetsym, ildsetparlist)$,
    [param lists (lists of symbols)]
  ),
  (
    $ildsetenv$,
    $() | ildpair(ildpair(ildsetsym, ildsetval), ildsetenv)$,
    [binding environments#footnote[We assume keys are restricted to be unique
      (@stepped-semantics). Furthermore, performant implementations will use
      other representations of binding environments.]]
  ),
  (
    $ildsetsf$,
    $ildsf("free-vars") | ildsf("quote") | ildsf("macroexpand")$,
    [special forms]
  ),
  (
    $ildsetfail$,
    $ildfail(ildsetval)$,
    [failure objects, each carries a context value]
  ),
  (
    $ildsethostfunc$,
    $ildhost("+") | ildhost("cons") | ildhost("apply") |
      ildhost("call/cc") | ...$,
    [host functions, opaque to the base language (@embedding)]
  ),
  (
    $ildsethost$,
    (
      $ildsetnum | ildsetbool | ildsetstr | ildsetabstr | ildsetcont |$,
      $ildsethostfunc | ...$
    ),
    [host values, opaque to the base language (@embedding)]
  ),
  ($ildsetnum$, [_numbers_]),
  ($ildsetbool$, $ildhost("t") | ildhost("f")$),
  ($ildsetstr$, [_strings_]),
  (
    $ildsetabstr$,
    $ildabstr(ildsetenv, ildsetparlist, ildsetval)$,
    [abstractions, @abstraction]
  ),
  (
    $ildsetcont$,
    $ildcont(ildsetcc)$,
    [continuations, @first-class-continuations],
  ),
  (
    $ildsetprog$,
    $ildsetsym | () | ildpair(ildsetprog, ildsetprog) |
      ildsetstr | ildsetnum | ildsetbool$,
    [surface syntax]
  ),
  ($ildsetcv$, $alpha_1, alpha_2, dots$, [continuation variables]),
  (
    $ildsetcc$,
    (
      $interop("eval", ildsetenv, ildsetcc, dots) |$,
      $interop("apply", ildsetcc, dots) |
        interop("comb", ildsetenv, ildsetcc, dots) |$,
      $ret(ildsetval) | mdo(ildsetcv <- ildsetcc, dots, ildsetcc) |$,
      $ildsetcv | cpsapp(ildsetcc, ildsetcc) |
        cpsapp(ildsetcc, ildsetval) | cpsabstr(ildsetcv, ildsetcc)$
    ),
    [evaluation CPS calculus]
  ),
)

#note[think about what to put in Cont - maybe CV? also think about restricting CC more]

We define ILD as a homoiconic language where parseable programs ($ildsetprog$,
standard S-expressions) are a subset of the internal syntax $ildsetcc$. In
addition, we assume that the parser supports the following shorthands:
- Quote: #ild("'<expr>") $arrow.r.double.bar$ #ild("(quote <expr>)")
- Macroexpand: #ild("(!<expr1> ... <exprN>)") $arrow.r.double.bar$
  #ild("(macroexpand <expr1> ... <exprN>)")

== Semantics <semantics>
We define the semantics of ILD in terms of a CPS calculus based on
the untyped $lambda$-calculus reduction rules and monadic combinators
over the set of _computations_ ($ildsetcc$). The computation domain
is further discussed in @continuation-monad.

=== Embedding <embedding>
#note[this can probably be merged with the intro at host-env]
ILD is designed to be embedded into a host environment, which supplies the set
$ildsethost$ of _host values_: values of the host's data structures, together with
the functions over them. Host values are opaque to ILD: We define FFI semantics
for some host values like
$ #evalsto($interop("apply", v, a_1, a_2, ..., a_n)$, $omega$) $
to denote that _calling_ the host value $v$ with arguments $a_1 ... a_n$ results
in the computation $omega$.

=== Small-step semantics <stepped-semantics>

Simple cases:
#row(
  step($s in ildsetsym and rho = ildlist(dots, ildpair(s, v), dots)$, $interop("eval", rho, s)$, $ret(v)$),
  step($v in ildsetsf union ildsethost union ildsetfail$, $interop("eval", rho, v)$, $ret(v)$),
)


Evaluating a combination (list): eval the head, decide what to do depending on result:
#row(evalsto($interop("eval", rho, ildlist(f, a_1, dots, a_n))$, $mdo(bind(phi, interop("eval", rho, f)), interop("comb", rho, phi, a_1, dots, a_n))$))

#step($phi in ildsethostfunc$, $interop("comb", rho, phi, a_1, ..., a_n)$, $mdo(bind(alpha_1, interop("eval", rho, a_1)), ..., bind(alpha_n, interop("eval", rho, a_n)), interop("apply", phi, alpha_1, ..., alpha_n))$)

Special forms:
#row(
  evalsto($interop("comb", rho, ildsf("quote"), v)$, $ret(v)$),
  evalsto($interop("comb", rho, ildsf("free-vars"))$, $ret(rho)$),
)
#row(evalsto($interop("comb", rho, ildsf("macroexpand"), m, accent(a, arrow))$, $mdo(bind(mu, interop("eval", rho, m)), bind(nu, interop("apply", mu, accent(a, arrow))), interop("eval", rho, nu))$))

Monadic CPS calculus:
#row(evalsto($mdo(bind(alpha_1, omega_1), dots, bind(alpha_n, omega_n), omega)$, $omega_1 bindop cpsabstr(alpha_1, dots.h omega_n bindop cpsabstr(alpha_n, omega) dots.h)$))
#row(
  evalsto($phi bindop f$, $cpsabstr(c, cpsapp(phi, cpsabstr(x, cpsapp(f, x, c))))$),
  evalsto($ret(v)$, $cpsabstr(c, cpsapp(c, v))$),
)

=== Notes on selected cases <semantics-notes>
- $interop("comb", rho, ildsf("free-vars"))$ returns $rho$. This special form
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
