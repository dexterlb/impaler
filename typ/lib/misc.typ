#let citneeded = text(fill: blue)[[citation needed]]

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

#let paraphrase(body) = squiggly_underline(body, orange)
#let todo(body) = squiggly_underline(body, green)
