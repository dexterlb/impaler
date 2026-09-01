#let ild-template(body) = {
  show raw.where(lang: "ild"): set raw(syntaxes: "/lib/ild.sublime-syntax")
  body
}

// TODO: make these different colours
#let interop(body) = $mono(body)$
#let ildsym(body) = $mono(body)$
#let ildsf(body) = $mono(body)$
#let ildfail(body) = $mono("Fail") (body)$

#let sem(body) = $⟦ body ⟧$
#let contmonad(body) = $C (body)$
#let retbare = $mono("ret")$
#let ret(body) = $retbare (body)$
