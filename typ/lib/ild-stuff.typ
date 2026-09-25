#let mono-font = "IBM Plex Mono"
#let mono-weight = "medium"

#let ild-template(body) = {
  show raw: set text(font: mono-font)
  show raw.where(lang: "ild"): set raw(syntaxes: "/lib/ild.sublime-syntax")
  show heading: it => {
    counter("definition").update(0)
    it
  }
  body
}

#let ild(body) = raw(body, lang: "ild", block: false)

#let c-interop = rgb("#1a7f37")
#let c-sym = rgb("#0b6e99")
#let c-sf = rgb("#8250df")
#let c-fail = rgb("#cf222e")
#let c-ildset = rgb("#a94a2b")

#let ildmono(body, color: black) = text(font: mono-font, weight: mono-weight, size: 0.9em, fill: color)[#body]

#let interop(body) = ildmono(body, color: c-interop)
#let ildsym(body) = ildmono(body, color: c-sym)
#let ildsf(body) = ildmono(body, color: c-sf)
#let ildfailbare = $#ildmono("Fail", color: c-fail)$
#let ildfail(body) = $#ildfailbare lr((#body))$
#let ildcont(body) = $#ildmono("cont")_(#body)$
#let ildset(name) = $#text(name, fill: c-ildset, weight: "bold")$

#let ildsetsym = ildset("Sym")
#let ildsetlist = ildset("List")
#let ildsetsf = ildset("SF")
#let ildsetfail = ildset("Fail")
#let ildsethost = ildset("Host")
#let ildsetnum = ildset("Num")
#let ildsetbool = ildset("Bool")
#let ildsetstr = ildset("Str")
#let ildsetenv = ildset("Env")
#let ildsetcont = ildset("Cont")

#let sem(body) = $lr(⟦ #body ⟧)$
#let contmonad(..args) = {
  let p = args.pos()
  if p.len() == 1 { $K lr((#p.at(0)))$ } else { $K_(#p.at(0)) lr((#p.at(1)))$ }
}
#let retbare = ildmono("ret")
#let ret(body) = $retbare lr((#body))$

#let bind(v, m) = $#v <- #m$
#let mdo(..steps) = $#ildmono("do")lr({ #steps.pos().join($ ; $) })$

#let bindop = box(baseline: 0.1em, image("/lib/bind.svg", height: 0.72em))

#let evalsto(from, to) = $#from -> #to$
#let defas(lhs, rhs) = $#lhs := #rhs$

#let ildlist-gap = 0.25em
#let ildlist(..items) = $lr((#items.pos().join(h(ildlist-gap))))$
#let ildpair(a, d) = $lr((#a #h(ildlist-gap) . #h(ildlist-gap) #d))$
#let ildabstr(env, params, body) = $Lambda(#env, #params, #body)$
#let step(prereqs, from, to) = $ #prereqs / #evalsto(from, to) $
#let steprow(..items) = align(center, grid(
  columns: items.pos().len(), column-gutter: 1em, align: horizon, ..items.pos(),
))
