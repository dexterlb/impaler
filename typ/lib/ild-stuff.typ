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
#let c-cps = rgb("#a94a2b")

#let ildmono(body, color: black) = text(font: mono-font, weight: mono-weight, size: 0.9em, fill: color)[#body]

#let ildsym(body) = ildmono(body, color: c-sym)
#let interopword(body) = ildmono(body, color: c-interop)
#let ildsf(body) = ildmono(body, color: c-sf)
#let cpsword(body) = ildmono(body, color: c-cps)
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
#let ildsetvcont = ildset("VCont")
#let ildsetcomp = ildset("Comp")
#let ildsetvv = ildset("VV")
#let ildsetcval = ildset("CVal")

#let cpsyield = cpsword("yield")
#let cpsret = cpsword("ret")

#let evalsto(from, to) = $#from -> #to$

#let ildlist-gap = 0.25em
#let ildlist(..items) = $lr((#items.pos().join(h(ildlist-gap))))$
#let interop(kind, sep: none, ..args) = $[ #ildmono(kind, color: c-interop) #args.pos().join(if sep == none { h(ildlist-gap) } else { sep }) ]$
#let cpsabstr(cv, cc) = $lr((lambda #cv #h(ildlist-gap) . #h(ildlist-gap) #cc))$
#let cpsapp(..cc) = $lr((#cc.pos().join(h(ildlist-gap))))$
#let ret(..body) = interop("ret", ..body)
#let ildapp(f, ..args) = ildlist(ildsym(f), ..args)
#let defas(env, rhs, f, ..args) = $#(ildapp(f, ..args)) class("relation", attach(limits(#pad(top: -0.45em)[$arrow.r.long.squiggly$]), t: script(#env))) #rhs$
#let dbarrow = rotate(90deg, reflow: true, $arrow.r.double.bar$)

#let rlabel(label, body) = grid(
  columns: (auto, 1fr),
  column-gutter: 1em,
  align: (right + top, left + top),
  label, body,
)

#let grules(..rules) = {
  let cells = ()
  for r in rules.pos() {
    let lines = if type(r.at(1)) == array { r.at(1) } else { (r.at(1),) }
    for (i, line) in lines.enumerate() {
      cells.push(if i == 0 { r.at(0) } else { [] })
      cells.push(if i == 0 { if r.len() > 3 { r.at(3) } else { $:=$ } } else { [] })
      cells.push(line)
      if i == 0 {
        cells.push(table.cell(rowspan: lines.len(), if r.len() > 2 { [#r.at(2)] } else { [] }))
      }
    }
  }
  block(table(
    columns: (auto, auto, auto, auto),
    column-gutter: 0.7em,
    row-gutter: 0.5em,
    stroke: none,
    inset: 0pt,
    align: (right + top, center + top, left + top, left + top),
    ..cells,
  ))
}

#let ildpair(a, d) = $lr((#a #h(ildlist-gap) . #h(ildlist-gap) #d))$
#let step(prereqs, from, to) = $ #prereqs / #evalsto(from, to) $
#let row(..items) = align(center, grid(
  columns: items.pos().len(), column-gutter: 3em, align: horizon, ..items.pos(),
))
