<!-- paper-review:start -->
# Paper review

## Active context
- Review round: 1
- Setup status: ready
- Working directory: `tex/docs/vtu-lite` (repo `impaler`, branch `trifon`)
- Main TeX entry point: `main.tex`
- Source/include map and bibliography: `main.tex` → `abstract.tex`, `intro.tex`, `ild-lang.tex` (Syntax §2, Semantics §3), `bootstrapping.tex` (§4), `meat.tex` (Conclusion §5); macros `../../common/macros/paper.tex`, `../../common/notation.tex`; bibliography `../../common/refs.bib` with `ACM-Reference-Format` (natbib, numeric).
- Included/excluded material and protected files: in scope = everything `\input` by `main.tex` plus the front matter in `main.tex`. `related-work.tex` is NOT `\input` (author note: "since this is a lite paper, probably remove this entire section"), so it gets no line-level review; it is used only as evidence for structure and technical findings. `conclusion.tex` is empty and not input. Protected (not edited): `../../common/**` (shared with other papers), `vtu2docx/**` (Word converter), `.agents/**`, generated files (`main.aux/log/out/pdf`, `x.log`).
- Target venue, edition/year, track/article type, submission stage: Journal "Mathematics, Computer Science and Education" (MCSE), St. Cyril and St. Methodius University of Veliko Tarnovo; 2026 header (`\journalyear`); regular paper; **initial blind-review submission** (inferred from `\blindreviewtrue` and `Vol. xxx` placeholders); final version must be Word (handled by `vtu2docx`, out of scope).
- Language variety and user preferences: English; spelling variety not specified (only marker: British "optimisation" in `meat.tex`), so variety is not enforced. No user writing guidelines supplied.
- Build command, prerequisites, and baseline result: documented build is Nix (`tex/flake.nix`, `docs/vtu-lite/default.nix` → `packages.vtu-lite-paper`), but `nix` is not installed here. Equivalent local build used: `TEXINPUTS=../../common:../../common/macros: BIBINPUTS=../../common: latexmk -lualatex -interaction=nonstopmode -outdir=<scratchpad>/build main.tex`. Baseline (2026-11-26): success, 9 pages A4, all citations resolved; only warnings: caption `hypcap` ignored, unused `\captionsetup[table]`. The committed `main.pdf`/`main.log` in the directory are from an earlier run without BibTeX (all citations undefined); not used.
- Context agreed on: 2026-11-26. Inferred from repository and venue site; the session was treated as non-interactive, so no questions were asked. Defaults to confirm in triage: initial-submission stage, exclusion of `related-work.tex`, `../../common` treated as protected.

## Requirements and sources
Sources:
- S1 = https://journals.uni-vt.bg/mcse/eng/req.aspx ("Guidelines for authors"), accessed 2026-11-26 via `curl -k` (WebFetch rejected the site's TLS certificate chain; content looked authentic but TLS was not verified). Current issue listed: Vol. 9, 2026, iss. 1.
- S2 = `vtu2docx/template/sample-mcse-eng.docx` (journal sample file; header says 2024), read locally 2026-11-26. S1 says "Header – as given in the sample file" and "Please apply the sample".

| Rule ID | Requirement | Applies to | Source | Verification/limitations |
| --- | --- | --- | --- | --- |
| G01 | Paper in Bulgarian or English | whole paper | S1 | mandatory |
| G02 | Blind review: author names and author-information section removed on initial submission | front/back matter, PDF metadata | S1, S2 | mandatory |
| G03 | Initial submission as PDF by e-mail; final version as MS Word | submission | S1 | external; unverifiable here |
| G04 | Title 14 pt bold; S2 adds "Capitalize Each Word" | title | S1, S2 | mandatory (S2 detail) |
| G05 | Authors' names 12 pt | front matter | S1 | n/a under blind review |
| G06 | Abstract in English, 10 pt italic; S2: ≤900 characters, no references, no abbreviations | abstract | S1, S2 | mandatory |
| G07 | Keywords in English, 10 pt italic; S2: ≤7, comma-separated | keywords | S1, S2 | mandatory |
| G08 | Body text 12 pt, several sections | body | S1 | mandatory |
| G09 | Figures integrated in text, number and caption centred below | figures (and listings if treated as figures) | S1 | mandatory; listing status ambiguous |
| G10 | Tables numbered, caption above, right-aligned | tables | S1 | mandatory |
| G11 | Equations numbered, numbers in brackets, right-aligned | display math | S1 | mandatory |
| G12 | Citations as reference numbers in square brackets; S2 additionally shows page ranges, e.g. "[3, 67-78]" | citations | S1, S2 | S1 vs S2 conflict on page ranges; S1 (current site) treated as authoritative, S2 page ranges advisory |
| G13 | Acknowledgements optional, 12 pt | back matter | S1 | optional |
| G14 | References 12 pt, alphabetical by first author's surname, ACM reference format | references | S1, S2 | mandatory |
| G15 | Cyrillic references also transliterated | references | S1 | n/a (no Cyrillic references) |
| G16 | Author information 12 pt | back matter | S1 | n/a under blind review |
| G17 | A4; margins 2 cm on all sides; header as in sample file | page setup | S1, S2 | mandatory |
| G18 | Pages not numbered | page setup | S1 | mandatory |
| G19 | Times New Roman; single line spacing | typography | S1 | mandatory |
| G20 | Paragraphs: first-line indent 1 cm; spacing above/after 0 | typography | S1 | mandatory |
| G21 | Section titles bold | headings | S1 | mandatory |
| G22 | Authors' declaration that the paper is not published or under review elsewhere | submission | S1 | external; unverifiable here |
| G23 | S2 content guidance: Introduction justifies topicality; main section gives a literature-supported overview; "Conclusions and future work" summarises contributions | content | S2 | advisory (sample-file guidance, not phrased as a hard rule on S1) |

## Pass progress
| Pass | Status | Covered files/sections and last anchor | Remaining scope | Limitations |
| --- | --- | --- | --- | --- |
| language | complete | main.tex title/front matter, abstract, §1–§5 incl. grammar annotations, captions, listing comments; last anchor meat.tex "namely side effects, IO" | none | related-work.tex not covered (not compiled) |
| style | complete | same scope; last anchor meat.tex Conclusion list | none | — |
| structure | complete | abstract → §1 → §2 → §3 → §4 → §5; related-work.tex read as evidence only | none | — |
| technical | complete | all claims, equations (1)–(14), Listings 4.1–4.7, citations; cross-checked against `ild-code/core/bootstrap/*.ild` and `rs-interpreter/src/sandbox.rs` | none | Kernel report, Shutt thesis, SICP, Henderson, Steele–Gabriel, Kohlbecker not inspected; only the arXiv abstract of fexpr-pe was checked |
| guidelines | complete | G01–G23 against source and the rendered PDF (2026-11-26 build) | none | G03, G22 external; G12 page-range conflict S1 vs S2 |

## Direct edits
All passes are language-pass edits unless noted; each preserves meaning, macros, keys and citations.
- intro.tex ¶1: "consist of series of statements" → "consist of a series of statements" (missing article); moved `~\cite{steele-gabriel-evolution,sicp}` before the full stop (citation punctuation).
- ild-lang.tex §3 ¶1: ``…'' → “…” around "evaluating the value v in the binding environment e". In the PDF the TeX ligatures were not active (rendered ‘‘…’’); Unicode quotes render correctly, and vtu2docx passes them through.
- ild-lang.tex §3.1.3: "an expression (macroexpand …)" → "An expression" (sentence began in lowercase after eq. (7)).
- ild-lang.tex §3.3 ¶1: "builtin data structures: (Num, Bool, Str)," → "builtin data structures (Num, Bool, Str):" (malformed punctuation).
- bootstrapping.tex Listing 4.1 caption: "Example expansion for !lambda expression" → "Example expansion of a !lambda expression".
- bootstrapping.tex after Listing 4.2: "under a lambda name usable from" → "under the name lambda, usable from".
- bootstrapping.tex Listing 4.4 code comment: "'Y' is special case of poly-fix" → "'Y' is a special case of poly-fix".
- meat.tex §5.1: "namely side effects and IO and more advanced data structures" → "namely side effects, IO and more advanced data structures".
- Not edited (deliberately): ``St. Kliment Ohridski'' in main.tex author block. It is hidden under blind review, and vtu2docx converts `` '' for the Word version.

## Findings
All round 1, status open, created 2026-11-26. Comment location = the block named by ID in the listed file (`grep -n "PR-…"`).

- PR-LANGUAGE-0001 — abstract.tex (before \begin{abstract}); also main.tex title, intro.tex, ild-lang.tex §3.3.2, bootstrapping.tex §4.3, meat.tex §5. Quote: "commonly-used". Issue: -ly adverb compounds hyphenated paper-wide. Proposal: drop the hyphens (statically scoped, purely functional, commonly used, mutually recursive, minimally verbose). Related: PR-GUIDELINES-0001.
- PR-LANGUAGE-0002 — bootstrapping.tex §4.1, after Listing 4.2. Quote: "we recall it twice". Issue: wrong or ambiguous verb. Proposal: "load" (or "use"), depending on intent.
- PR-LANGUAGE-0003 — ild-lang.tex §2 (before "Source code is parsed as"); also §3.3 list item "constructed via e.g." and §3.3.1 "(e.g. \cref{if})". Issue: inter-sentence spacing after i.e./e.g. Proposal: `e.g.,~` / `e.g.\ ` / \frenchspacing, after checking vtu2docx handles it.
- PR-STYLE-0001 — meat.tex §5 (before "The language ILD, presented"). Quote: "Extremely simple to implement", "Expressive", "minimally-verbose". Issue: promotional, non-parallel feature list. Proposal: parallel concrete noun phrases. Related: PR-TECHNICAL-0009.
- PR-STYLE-0002 — ild-lang.tex §3.1. Quote: "Unlike Scheme or Common LISP, ILD is a simple language". Issue: implies Scheme and CL are not simple. Proposal: "…which define many special forms, ILD has only three."
- PR-STYLE-0003 — ild-lang.tex §2 ¶1. Quote: "internal syntax (tangible values)" vs grammar "Observable values". Issue: three terms for Val. Proposal: one term.
- PR-STYLE-0004 — bootstrapping.tex §4.2 (before Listing 4.4); also Listings 4.3, 4.5, 4.6. Quote: "; body of 'cadr'". Issue: "body" means the bound value here but β in §3.2. Proposal: "; definition of 'X'". Related: PR-STRUCTURE-0005.
- PR-STYLE-0005 — intro.tex ¶2. Quote: "ILD does not sacrifice expressiveness of metaprogramming while retaining". Issue: stacked contribution sentence. Proposal: rewrite given in the comment.
- PR-STRUCTURE-0001 — abstract.tex. Issue: the abstract omits the title's properties and the core claim, and promises "nontrivial programs". Proposal: problem → idea → result outline (≤900 chars, G06). Related: PR-TECHNICAL-0009.
- PR-STRUCTURE-0002 — intro.tex ¶2 (main) + meat.tex §5.1 (secondary). Issue: related-work.tex is excluded, so there is no positioning and "fexprs" first appears undefined in Future work. Proposal: move 3–4 sentences (fexpr definition, Kernel distinction, Kraken) into the intro. Respects the author note in related-work.tex. Related: PR-TECHNICAL-0010; G23.
- PR-STRUCTURE-0003 — intro.tex ¶2. Quote: "One such example is Lispkit Lisp". Issue: ambiguous referent. Proposal: name the shortcoming (author to confirm).
- PR-STRUCTURE-0004 — bootstrapping.tex §4.1 (after Listing 4.1). Issue: the lambda-macro exposition is too terse. Listing 4.1 is schematic (symbols) while Listing 4.2 splices values, and the Listing 4.3 self-application is unexplained. Proposal: show the real expansion, explain value-splicing, walk through Listing 4.3. Related: PR-TECHNICAL-0007.
- PR-STRUCTURE-0005 — bootstrapping.tex §4.2 (start). Issue: how named library definitions are bound is never shown. Proposal: one sentence or skeleton (the pattern matches ild-code/core/bootstrap/*.ild). Related: PR-STYLE-0004.
- PR-TECHNICAL-0001 — bootstrapping.tex §4.3 (before "With these building blocks"). **Demonstrated error**: the Listing 4.7 expansion drops the inner (!lambda (n) …)/(!lambda () …), leaving n unbound and Γ_main returning a boolean. Confirmed against ild-code/core/bootstrap/letrec.ild (bind-operators = (lambda names (cadr def))). Proposal: corrected lines given in the comment.
- PR-TECHNICAL-0002 — ild-lang.tex §3.3.3. **Demonstrated inconsistency**: eq. (14) `\denot{v}{e}` (⟦e⟧_v) swaps program and environment. Listing 4.3 and rs-interpreter `do_eval(ret, env_spec, body)` both take the environment first. Proposal: `\denot{e}{v}`.
- PR-TECHNICAL-0003 — ild-lang.tex §3.3.2. Issue: "f_1 … f_i" should be "… f_n"; the well-foundedness explanation is missing; #f_i collides with the boolean #f. Proposal in the comment.
- PR-TECHNICAL-0004 — ild-lang.tex §3 (before "Values that are neither symbols nor pairs"). Minor inconsistency: () is neither a symbol nor a pair but evaluates to #fail (§3.4). Proposal: reword.
- PR-TECHNICAL-0005 — ild-lang.tex §2 (before Grammar 2.1). **Demonstrated rendering loss**: the footnote inside tabular is not typeset (pdftotext finds no "We assume keys"). Proposal: \footnotemark/\footnotetext or move the text to prose.
- PR-TECHNICAL-0006 — ild-lang.tex §3.4. Missing specification: arity mismatch, ill-typed builtin arguments, #fail propagation, and a macroexpand head that is not a function. Proposal: one catch-all rule plus a stated propagation policy.
- PR-TECHNICAL-0007 — ild-lang.tex §3.1.3. Central claim (static scoping is kept with macros) not argued; eq. (7) is unhygienic, value-splicing is an unstated discipline, and environments are first-class. Question for the author; proposal (a)–(c) in the comment. Related: PR-STRUCTURE-0004, PR-TECHNICAL-0010.
- PR-TECHNICAL-0008 — ild-lang.tex §3.3.3 (before "Furthermore, to allow programs"). Tension: read-source reads files vs the purity/immutability claims. Proposal: state the fixed-source-tree assumption or name the exception.
- PR-TECHNICAL-0009 — meat.tex §5 (before "The language ILD, presented"); also abstract and bootstrapping.tex §4.3. Unsupported claims (nontrivial programs, extremely simple to implement, expressive); letrec omitted although it is about 20 lines. Proposal: cite the implementation, give its size and an anonymised link, include letrec. Related: PR-STYLE-0001, PR-STRUCTURE-0001.
- PR-TECHNICAL-0010 — intro.tex ¶2. "extensively researched" and "most of them…" uncited; Kraken [fexpr-pe] and Kernel are close counterexamples; novelty delta not stated. Proposal in the comment. Related: PR-STRUCTURE-0002, PR-TECHNICAL-0007.
- PR-TECHNICAL-0011 — meat.tex §5.1. The [kernel-report] support for "has been studied" is unverified; [fexpr-pe] (verified abstract) supports it. "Significant performance cost" is unmeasured. Proposal: re-cite or hedge.
- PR-TECHNICAL-0012 — ild-lang.tex §3.2 (start). Notation: stacked x-over-v substitution reads backwards against the [v/x] convention; β vs B. Proposal: e[x ↦ v], one letter.
- PR-GUIDELINES-0001 — main.tex (before title \begin{center}). Rule G04 (S2 "Capitalize Each Word"): violation. Proposal: capitalised title. Related: PR-LANGUAGE-0001.
- PR-GUIDELINES-0002 — main.tex preamble (\parindent/\parskip). Rule G20: violation (0 pt indent and 6 pt skip vs 1 cm indent and 0 spacing). Proposal in the comment.
- PR-GUIDELINES-0003 — main.tex preamble (\lstcaption redefinition). Rule G09: conditional violation (listing captions flush left). Editorial question.
- PR-GUIDELINES-0004 — main.tex (before \bibliographystyle). Rule G14: violation in rendering of [7] (missing booktitle, @inbook) and [4] (duplicate "Ph.D. dissertation"). The fix is in protected ../../common/refs.bib, so it is proposed only.

## Guideline checklist (round 1, PDF from 2026-11-26 build)
| Rule | Status | Evidence / ID |
| --- | --- | --- |
| G01 | satisfied | English throughout |
| G02 | satisfied | `\blindreviewtrue`; no author block/about-author in PDF; PDF Author metadata empty |
| G03 | unverified | external submission step |
| G04 | violation | 14 pt bold OK; capitalisation → PR-GUIDELINES-0001 |
| G05 | not applicable | blind review (non-blind branch uses 12 pt) |
| G06 | satisfied / unverified | 10 pt bold-italic label + italic text; ~450 chars ≤900; no citations. Abbreviations: "ILD" is never expanded. If it is an acronym, the S2 "no abbreviations" rule may apply; author to confirm |
| G07 | satisfied | 5 keywords, 10 pt italic |
| G08 | satisfied | 12pt article, 5 sections |
| G09 | satisfied (figures) / conditional (listings) | Grammar 2.1 caption centred below; listings → PR-GUIDELINES-0003 |
| G10 | not applicable | no tables |
| G11 | satisfied | display equations (1)–(14) numbered "(n)" flush right |
| G12 | satisfied (S1) / unverified (S2) | numeric [n] citations; S2 sample asks for page ranges "[3, 67-78]", S1 does not; not raised as a finding |
| G13 | not applicable | no acknowledgements |
| G14 | violation | alphabetical ACM style OK; entries [4], [7] → PR-GUIDELINES-0004 |
| G15 | not applicable | no Cyrillic references |
| G16 | not applicable | blind review |
| G17 | satisfied (minor deviation) | A4 (595×842 pt), geometry margin=2cm; two-line 10 pt bold header as in S2, but the Cyrillic line uses Libertinus Serif rather than Times New Roman; year placeholder 2026 vs S2 2024 sample. No action proposed |
| G18 | satisfied | `\fancyhf{}`; no page numbers in PDF |
| G19 | satisfied | TeX Gyre Termes X (Times clone) via newtxtext; single spacing |
| G20 | violation | → PR-GUIDELINES-0002 |
| G21 | satisfied | section titles bold |
| G22 | unverified | external declaration |
| G23 | advisory, partially met | no literature-supported overview in compiled paper → PR-STRUCTURE-0002 |

## Handoffs between passes
- language → technical: ild-lang.tex §3.3 "builtin data structures (Num, Bool, Str)" lists only three, while the bullets also cover pairs, functions and failures. Disposition (technical): minor, no finding.
- language → structure: bootstrapping.tex §4 intro "Keeping that complexity out of the core language" has a vague referent. Disposition (structure): minor, no finding.
- style → technical: claim strength in the conclusion list → PR-TECHNICAL-0009.
- structure → technical: static-scoping argument → PR-TECHNICAL-0007.

## Existing author comments (not acted on; for triage)
- bootstrapping.tex §4: `\begin{note} can this section be shortened further? \end{note}`. Renders as a pink box in the PDF (p. 5); must be removed or resolved before submission.
- related-work.tex: `\begin{note} since this is a lite paper, probably remove this entire section \end{note}`. File not compiled. PR-STRUCTURE-0002 proposes keeping a 3–4 sentence condensation in the intro.
- related-work.tex line 11: stray `%,` before the footnote (not compiled).

## Checks and limitations
- 2026-11-26: local latexmk/LuaLaTeX build into scratchpad (see Active context): success, baseline.
- 2026-11-26: rendered PDF inspected page by page (PNG at 70 dpi) and with `pdftotext`.
- 2026-11-26: post-review rebuild (same command, separate outdir): success, 9 pages, same two caption warnings, no review text in PDF. pdftotext diff vs baseline shows exactly the 7 prose edits above.
- 2026-11-26: `git diff -U0` checked: only `%`-comment additions plus the listed direct edits; no blank lines, macros, labels or keys altered.
- 2026-11-26: cross-checked Listings 4.2/4.5/4.6 against ild-code/core/bootstrap/{lambda-macro,if,map}.ild (consistent), Listing 4.7 against letrec.ild (inconsistent → PR-TECHNICAL-0001), and eval argument order against rs-interpreter/src/sandbox.rs `do_eval` (→ PR-TECHNICAL-0002).
- 2026-11-26: fexpr-pe verified via arXiv abstract page (2303.12254). Other cited sources were not inspected, so citation support for them is unverified.
- Nix build (`nix build .#vtu-lite-paper`) not run: nix not installed. vtu2docx Word output not built or checked (out of scope); its `strip_comments` should drop the review comments, but this was not tested.
- Venue page S1 fetched without TLS verification (`curl -k`).

## Resume and triage
- Last completed action and next action: all five passes complete for round 1 → next is interactive triage (paper-review-triage) when the user asks.
- Suggested triage order: PR-TECHNICAL-0001, -0002, -0005 (demonstrated errors) → PR-TECHNICAL-0007, -0010, PR-STRUCTURE-0002 (positioning/central claim) → GUIDELINES → remaining.
- Current triage group and awaiting decision: none.
- Deferred IDs and suggested revisit point: none.
- Highest allocated ID per owner, including removed findings: LANGUAGE 0003, STYLE 0005, STRUCTURE 0005, TECHNICAL 0012, GUIDELINES 0004.

## Previous rounds
None.
<!-- paper-review:end -->
