#let citneeded = text(fill: blue)[[citation needed]]

#let clink(dest, body) = link(dest, text(fill: blue, body))
#let optref(target) = context if query(target).len() > 0 [ (#ref(target))]

#let cong = sym.tilde.equiv

#let squiggly_underline(body, color) = box(context {
  let w = measure(body).width
  let amp = 1pt
  let period = 4pt
  let n = int(calc.max(1, calc.round(w / period)))
  let step = w / n
  let segs = (curve.move((0pt, 0pt)),)
  for i in range(n) {
    let x0 = i * step
    let x1 = (i + 1) * step
    let dir = if calc.even(i) { amp } else { -amp }
    segs.push(curve.cubic((x0 + step / 3, dir), (x1 - step / 3, dir), (x1, 0pt)))
  }
  place(bottom, dy: amp + 1pt, curve(stroke: color, ..segs))
  body
})

#let paraphrase(..args) = {
  let a = args.pos()
  let underlined = squiggly_underline(a.at(0), orange)
  let replacement = a.at(1, default: none)
  if replacement == none {
    underlined
  } else {
    [#underlined#h(0.15em)#text(fill: orange, size: 0.85em)[(→ #replacement)]]
  }
}
#let todo(body) = squiggly_underline(body, green)

#let review(body) = block(
  width: 100%,
  breakable: true,
  fill: rgb("#fff8dc"),
  inset: (x: 8pt, y: 6pt),
  radius: 2pt,
  stroke: (left: 2pt + rgb("#e0b000")),
  body,
)

#let note(body) = block(
  width: 100%,
  breakable: true,
  fill: rgb("#ffdcf8"),
  inset: (x: 8pt, y: 6pt),
  radius: 2pt,
  stroke: (top: 2pt + rgb("#e0b000")),
  body,
)

#let comment(body) = block(
  width: 100%,
  breakable: true,
  fill: rgb("#aadcff"),
  inset: (x: 8pt, y: 6pt),
  radius: 2pt,
  stroke: (top: 2pt + rgb("#e0b022")),
  body,
)

#let abstract(body) = align(center, block(width: 85%)[
  #set par(justify: true)
  #set align(left)
  *Abstract.* #body
])

#let definition-counter = counter("definition")
#let definition(body) = {
  definition-counter.step()
  block(above: 1em, below: 1em, width: 100%)[
    *Definition #context {
      (counter(heading).get() + definition-counter.get()).map(str).join(".")
    }.* #body
  ]
}

#let listing-fill = rgb("#fff3dd")
#let lst(body, label: none, caption: none) = {
  let boxed = [
    #show raw.where(block: true): it => block(
      fill: listing-fill, inset: (x: 10pt, y: 8pt), radius: 4pt, it,
    )
    #body
  ]
  let fig = figure(
    boxed,
    kind: "listing",
    supplement: [Listing],
    numbering: n => context (counter(heading).get() + (n,)).map(str).join("."),
    caption: caption,
  )
  if label == none { fig } else { [#fig#std.label(label)] }
}

#let cases-gap = 0.7em
#let cases(gap: cases-gap, ..args) = {
  let add-gap(row) = {
    if type(row) != content { return row }
    let elems = if row.has("children") { row.children } else { (row,) }
    elems.map(e => if type(e) == content and repr(e.func()) == "align-point" {
      (e, h(gap))
    } else {
      (e,)
    }).flatten().join()
  }
  math.cases(..args.named(), ..args.pos().map(add-gap))
}
