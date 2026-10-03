#let mono-font = "IBM Plex Mono"
#let mono-weight = "medium"

#let ild-template(body) = {
  show raw: set text(font: mono-font)
  show raw.where(lang: "ild"): set raw(syntaxes: "/lib/ild.sublime-syntax")
  show heading: it => {
    counter("definition").update(0)
    counter(figure.where(kind: "listing")).update(0)
    it
  }
  body
}

#let ild(body) = raw(body, lang: "ild", block: false)

#let c-interop = rgb("#1a7f37")
#let c-sym = rgb("#0b6e99")
#let c-sf = rgb("#8250df")
#let c-host = rgb("#cf222e")
#let c-ildset = rgb("#a94a2b")

#let ildmono(body, color: black) = text(font: mono-font, weight: mono-weight, size: 0.9em, fill: color)[#body]

#let ildsym(body) = ildmono(body, color: c-sym)
#let ildsf(body) = ildmono(body, color: c-sf)
#let ildset(name) = $#text(name, fill: c-ildset, weight: "bold")$

#let ildhost(body) = $#ildmono("#" + body, color: c-host)$
#let ildangles(..args) = $#text(fill: c-host)[$⟨$]#args.pos().join($, $)#text(fill: c-host)[$⟩$]$
#let ildabstrbare = $#ildmono("#Λ", color: c-host)$
#let ildabstr(..args) = $#(ildabstrbare)#ildangles(..args)$
#let ildfailbare = $#ildmono("#fail", color: c-host)$
#let ildfail(..args) = $#(ildfailbare)#ildangles(..args)$
#let ildcontbare = $#ildmono("#cont", color: c-host)$
#let ildcont(..args) = $#(ildcontbare)#ildangles(..args)$

#let ildsetsym = ildset("Sym")
#let ildsetval = ildset("Val")
#let ildsetprog = ildset("Prog")
#let ildsetsexp = ildset("Sexp")
#let ildsetlist = ildset("List")
#let ildsetparlist = ildset("ParList")
#let ildsetsf = ildset("SF")
#let ildsetfail = ildset("Fail")
#let ildsethost = ildset("Host")
#let ildsethostfunc = ildset("HostFunc")
#let ildsetnum = ildset("Num")
#let ildsetbool = ildset("Bool")
#let ildsetstr = ildset("Str")
#let ildsetenv = ildset("Env")
#let ildsetabstr = ildset("Abstr")
#let ildsetcont = ildset("Cont")
#let ildsetcomp = ildset("Comp")
#let ildsetcv = ildset("CV")
#let ildsetcc = ildset("CC")

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

#let ildlist-gap = 0.25em
#let ildlist(..items) = $lr((#items.pos().join(h(ildlist-gap))))$
#let interop(kind, ..args) = $[ #ildmono(kind, color: c-interop) #args.pos().join(h(ildlist-gap)) ]$
#let ildapp(f, ..args) = ildlist(ildsym(f), ..args)
#let defas(env, rhs, f, ..args) = $#(ildapp(f, ..args)) class("relation", attach(limits(#pad(top: -0.45em)[$arrow.r.long.squiggly$]), t: script(#env))) #rhs$
#let grule(name, def, comment) = block($#name := #def #h(1em) (#comment)$)

#let grules(..rules) = {
  let cells = ()
  for r in rules.pos() {
    let lines = if type(r.at(1)) == array { r.at(1) } else { (r.at(1),) }
    for (i, line) in lines.enumerate() {
      cells.push(if i == 0 { r.at(0) } else { [] })
      cells.push(if i == 0 { if r.len() > 3 { r.at(3) } else { $:=$ } } else { [] })
      cells.push(line)
      cells.push(if i == 0 and r.len() > 2 { $(#r.at(2))$ } else { [] })
    }
  }
  block(grid(
    columns: (auto, auto, auto, auto),
    column-gutter: 0.7em,
    row-gutter: 0.5em,
    align: (right + top, center + top, left + top, left + top),
    ..cells,
  ))
}

#let ildpair(a, d) = $lr((#a #h(ildlist-gap) . #h(ildlist-gap) #d))$
#let step(prereqs, from, to) = $ #prereqs / #evalsto(from, to) $
#let row(..items) = align(center, grid(
  columns: items.pos().len(), column-gutter: 1em, align: horizon, ..items.pos(),
))
