# Review summary: "A minimalistic, Statically-scoped, Purely-functional LISP"

- **Date:** 2026-11-26 (review round 1)
- **Target:** Journal "Mathematics, Computer Science and Education" (MCSE), St. Cyril and St. Methodius University of Veliko Tarnovo. Assumed to be the initial blind-review submission.
- **Scope:** everything `main.tex` includes (`abstract.tex`, `intro.tex`, `ild-lang.tex`, `bootstrapping.tex`, `meat.tex`) plus the front matter. `related-work.tex` is not included by `main.tex`, so it was used only as background.
- **Passes:** language, style, structure, technical, journal guidelines.

Each finding is a `% PAPER-REVIEW[PR-…]` comment block in the source, placed just before the paragraph it concerns; `grep -n "PR-TECHNICAL-0001" *.tex` finds one. The full record (evidence, guideline checklist, edits, limitations) is in `MEMORY.md`.

## Must fix: demonstrated errors

| ID | Where | Problem | Fix |
| --- | --- | --- | --- |
| PR-TECHNICAL-0001 | `bootstrapping.tex` §4.3, Listing 4.7 | The `letrec` expansion drops each definition's inner `(!lambda (n) …)` / `(!lambda () …)`. The expanded code refers to an unbound `n`, and `main`'s generator returns a boolean instead of a function. The real implementation (`ild-code/core/bootstrap/letrec.ild`) keeps the inner lambdas. | Corrected lines 12–16 are in the comment. |
| PR-TECHNICAL-0002 | `ild-lang.tex` §3.3.3, eq. (14) | `apply(#eval; e, v) = ⟦e⟧ᵥ` evaluates the environment inside the expression. Listing 4.3 and `rs-interpreter`'s `do_eval(ret, env_spec, body)` both take the environment first. | Use `\denot{e}{v}` (⟦v⟧ₑ). |
| PR-TECHNICAL-0005 | `ild-lang.tex` §2, Grammar 2.1 | The footnote on "Binding environments" is never printed, because footnotes inside `tabular` are dropped. Its "keys are unique" assumption, which eqs. (1) and (9) rely on, is missing from the PDF. | Use `\footnotemark` + `\footnotetext`, or move the text into prose. |

## Main content issues

| ID | Where | Problem |
| --- | --- | --- |
| PR-TECHNICAL-0007 | `ild-lang.tex` §3.1.3 | The central claim, that static scoping survives macros, is asserted but not argued. Eq. (7) evaluates the expanded code in the caller's environment, which the paper itself calls unhygienic. Macros avoid depending on the caller's bindings only because they splice in values (`#mk-lambda`, special forms) instead of symbols, and the paper never states this technique. Environments are also ordinary data (`free-vars`, `mk-lambda`, `eval`). Define "statically scoped" for ILD and either argue the claim or qualify it. |
| PR-TECHNICAL-0010, PR-STRUCTURE-0002 | `intro.tex` ¶2; `meat.tex` §5.1 | No positioning against prior work in the compiled paper. "Extensively researched" and "most of them…" have no citations. Kraken (ref. [2], a purely functional Lisp based on fexprs) and Kernel are close to ILD and not discussed. "Fexprs" first appears, undefined, in Future work. Suggestion: move 3–4 sentences from `related-work.tex` into the introduction rather than restoring the whole section. |
| PR-TECHNICAL-0009 | `meat.tex` §5; abstract; §4.3 | Unsupported claims: "nontrivial programs" (the largest example is even?/odd?), "Extremely simple to implement" (no implementation mentioned), "Expressive". The `letrec` implementation is omitted, although it is about 20 lines. Mention the implementation (anonymised link, size of the evaluator core) and include `letrec`. |
| PR-TECHNICAL-0008 | `ild-lang.tex` §3.3.3 | `read-source` reads files, which conflicts with "purely functional" and "fully immutable". State the assumption that the source tree is fixed during evaluation, or name `read-source` as the one exception. |
| PR-STRUCTURE-0004 | `bootstrapping.tex` §4.1 | The lambda-macro walkthrough is too terse. Listing 4.1 shows symbols while Listing 4.2 splices values, and the self-application in Listing 4.3 is explained in one sentence. |
| PR-STRUCTURE-0001 | `abstract.tex` | The abstract mentions neither property in the title (statically scoped, purely functional) nor the core claim. |

## Journal compliance

Requirements come from https://journals.uni-vt.bg/mcse/eng/req.aspx (accessed 2026-11-26) and the journal sample file `vtu2docx/template/sample-mcse-eng.docx`.

| ID | Rule | Problem |
| --- | --- | --- |
| PR-GUIDELINES-0002 | Paragraphs: 1 cm first-line indent, no spacing before or after | `main.tex` sets `\parindent=0pt` and `\parskip=6pt`. |
| PR-GUIDELINES-0001 | Title: "Capitalize Each Word" (sample file) | "A minimalistic, Statically-scoped, Purely-functional LISP" |
| PR-GUIDELINES-0004 | ACM reference format | [7] Steele & Gabriel is missing its book title (`@inbook` entry); [4] Kohlbecker repeats "Ph.D. dissertation". The fix belongs in the shared `../../common/refs.bib`, so it was only proposed. |
| PR-GUIDELINES-0003 | Figure captions centred below | Listing captions are left-aligned. This matters only if the editors count listings as figures. |

Everything else checked is satisfied: English, blind review, A4 and 2 cm margins, header, no page numbers, font sizes, numbered equations, `[n]` citations, alphabetical references. Open questions:
- **ILD in the abstract.** If ILD is an acronym, it is never expanded, and the sample file forbids abbreviations in the abstract.
- **Page ranges in citations.** The sample file shows page ranges (`[3, 67-78]`); the website does not ask for them.

## Other findings

| ID | Where | Issue |
| --- | --- | --- |
| PR-TECHNICAL-0003 | `ild-lang.tex` §3.3.2 | `poly-fix`: the index should run "f₁ … fₙ", not "f₁ … fᵢ". The reason the definition terminates is not explained, and the host objects `#f₁…#fₙ` look like the boolean `#f`. |
| PR-TECHNICAL-0006 | `ild-lang.tex` §3.4 | Failure behaviour is unspecified for arity mismatches, builtins given the wrong types, `#fail` passed as an argument, and `macroexpand` whose head is not a function. |
| PR-TECHNICAL-0011 | `meat.tex` §5.1 | Unclear whether [6] (Kernel report) supports "has been studied". [2] does support it (arXiv abstract checked). The "significant performance cost" is not measured. |
| PR-TECHNICAL-0012 | `ild-lang.tex` §3.2 | The substitution notation (x stacked over v) reads backwards against the common [v/x]. The body is written β in one place and B in another. |
| PR-TECHNICAL-0004 | `ild-lang.tex` §3 | The prose says every non-symbol, non-pair value evaluates to itself, but `()` is neither and §3.4 says it evaluates to `#fail`. |
| PR-STRUCTURE-0005 | `bootstrapping.tex` §4.2 | The paper never shows how library definitions (`cadr`, `Y`, `if`, `map`) are bound to their names. |
| PR-STRUCTURE-0003 | `intro.tex` ¶2 | "One such example is Lispkit Lisp": unclear which shortcoming it exemplifies. |
| PR-STYLE-0001 | `meat.tex` §5 | The conclusion's feature list is promotional ("Extremely", "Expressive") and its items are not parallel. |
| PR-STYLE-0002 | `ild-lang.tex` §3.1 | "Unlike Scheme or Common LISP, ILD is a simple language" implies those languages are not simple. |
| PR-STYLE-0003 | `ild-lang.tex` §2 | The set of values is called "internal syntax", "tangible values" and "Observable values". |
| PR-STYLE-0004 | `bootstrapping.tex` Listings 4.3–4.6 | `; body of 'X'` labels the value bound to X, which clashes with "body" meaning a function's body. |
| PR-STYLE-0005 | `intro.tex` ¶2 | The contribution sentence is hard to parse; a rewrite is suggested. |
| PR-LANGUAGE-0001 | paper-wide | Compounds with -ly adverbs are hyphenated ("statically-scoped", "purely-functional", "commonly-used", …). |
| PR-LANGUAGE-0002 | `bootstrapping.tex` §4.1 | "we recall it twice": the intended verb is unclear ("load" or "use"). |
| PR-LANGUAGE-0003 | `ild-lang.tex` §2, §3.3 | "e.g." and "i.e." get a wide sentence-ending space in the PDF. |

## Edits already made

These preserve meaning, macros, labels and citations.

- `intro.tex`: "consist of series" → "consist of a series"; citation moved before the full stop.
- `ild-lang.tex`: ``…'' → “…” around "evaluating the value v…", which rendered as ‘‘…’’ in the PDF.
- `ild-lang.tex`: "an expression (macroexpand …)" → "An expression"; "data structures: (Num, Bool, Str)," → "data structures (Num, Bool, Str):".
- `bootstrapping.tex`: Listing 4.1 caption "Example expansion of a !lambda expression"; "under the name lambda, usable from…"; "'Y' is a special case of poly-fix".
- `meat.tex`: "side effects and IO and more" → "side effects, IO and more".

## Not resolved

These author notes were left untouched:
- The note box "can this section be shortened further?" in §4 still renders in the PDF and should be removed or resolved before submission.
- The note in `related-work.tex` suggests removing that section. PR-STRUCTURE-0002 proposes keeping a short version in the introduction instead.

## Assumptions and limitations

- Assumed without confirmation: initial blind-review stage; `related-work.tex` out of scope; shared files in `../../common` not edited.
- Built with local `latexmk -lualatex` (9 pages, citations resolve). The Nix build was not run because `nix` is not installed. The Word version (`vtu2docx`) was not built.
- Of the cited sources, only the arXiv abstract of [2] was checked, so support from the other references is unverified.
- The journal guidelines page was fetched without TLS certificate verification.

## Suggested order

1. PR-TECHNICAL-0001, 0002, 0005: clear errors, quick to fix.
2. PR-TECHNICAL-0007, 0010 and PR-STRUCTURE-0002: the central claim and its positioning.
3. Journal compliance (GUIDELINES-0001 to 0004) and the §4 note box.
4. The remaining findings.
