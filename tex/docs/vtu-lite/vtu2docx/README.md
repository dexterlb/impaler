# vtu2docx

Converts the `vtu-lite` LaTeX paper into the Word template required by the
journal *Mathematics, Computer Science and Education* (MCSE), St. Cyril and
St. Methodius University of Veliko Tarnovo.

The result is built from the journal's own `sample-mcse-eng.docx`, so the page
setup (A4, 2 cm margins), the running header, the Times New Roman body style
and the absence of page numbers come straight from the template.

## How it works

Prose becomes real, editable Word text: sections, paragraphs, item lists,
citations and cross references are converted, with `\emph`, `\textbf` and the
paper's inline code macros (`\ild`, `\ildsym`, `\interopword`, ...) mapped to
Word runs (including the paper's colour coding).

Anything whose typography is LaTeX specific is typeset by the paper's *own*
preamble and embedded as a 600 dpi PNG:

| LaTeX                                   | Word output                                        |
| --------------------------------------- | -------------------------------------------------- |
| `$...$`, `\(...\)`, `\[...\]`           | inline/display image, baseline aligned             |
| `equation`                              | centred image with `(n)` at the right margin       |
| `listingbox` + `ildcode` + `\lstcaption`| framed, line-numbered listing image + caption      |
| `cfighere` / `ctab`                     | table image, caption above, right aligned          |
| `note` / `commentbox` / `reviewbox`     | coloured box image                                 |
| `\includegraphics`                      | embedded image                                     |

Image sizes come from the tight LaTeX bounding box, not from guessing.
Inline maths is rendered with its descent and placed through an anchored
picture over a transparent inline spacer, so it sits on the text baseline in
both Word and LibreOffice (`w:position` is ignored for pictures by both).

References are read from `refs.bib`, sorted alphabetically by the first
author's last name, numbered, and formatted in an ACM-like style. Citation
lists are sorted and compressed (`[1, 3]`, `[1–3]`). Footnotes become real Word
footnotes.

`\newif` conditionals in the document body are evaluated, so the blind-review
switch in `main.tex` is honoured: with `\blindreviewtrue` the author block and
the "About the Author" section are omitted.

## Build

From the `tex/` directory:

```sh
nix build .#vtu-lite-docx
# result/vtu-lite.docx
```

The derivation is `docs/vtu-lite/vtu2docx/default.nix` and is wired up in
`docs/vtu-lite/default.nix`.

## Run directly

Inside the development shell (which provides lualatex, poppler and python):

```sh
nix develop
cd docs/vtu-lite
python3 vtu2docx/vtu2docx.py \
  --paper main.tex \
  --common ../../common \
  --template vtu2docx/template/sample-mcse-eng.docx \
  --out /tmp/vtu-lite.docx
```

Useful flags:

* `--drop-notes` – omit the coloured editorial note boxes.
* `--no-blind` – keep author names and the About the Author section.
* `--cache DIR` – cache rendered images between runs.
* `--dry-run` – parse only, print element/job statistics.
* `--keep-temp` – keep the generated `images.tex`, `images.pdf` and log.

## Requirements

`lualatex` with the packages listed in `flake.nix` (`preview` is needed),
`pdftocairo` and `pdfinfo` from poppler, and Python 3.11+ (standard library
only).
