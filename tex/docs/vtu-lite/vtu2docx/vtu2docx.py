#!/usr/bin/env python3
"""Convert the vtu-lite LaTeX paper into the MCSE journal Word template.

The paper's prose is converted to real Word text.  Anything that relies on
LaTeX-specific typography (display and inline mathematics, code listings,
the grammar table and the coloured note boxes) is typeset by the very same
LaTeX preamble the paper uses, cropped to its bounding box and embedded as a
600 dpi PNG.

Only the Python standard library, a TeX distribution with lualatex and
poppler's pdftocairo are required.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import unicodedata
import zipfile
from dataclasses import dataclass
from pathlib import Path
from xml.sax.saxutils import escape as xml_escape

EMU_PER_PT = 12700
TWIPS_PER_CM = 567
TEXT_WIDTH_TWIPS = 9639  # A4 (21cm) minus 2+2cm margins
TEXT_WIDTH_PT = TEXT_WIDTH_TWIPS / 20.0
LISTING_WIDTH_CM = 13.6
LISTING_WIDTH_PT = LISTING_WIDTH_CM / 2.54 * 72.0
PNG_DPI = 600

COLORS = {
    "cinterop": "1A7F37",
    "csym": "0B6E99",
    "csf": "8250DF",
    "chost": "CF222E",
    "cildset": "A94A2B",
}

CODE_ENVS = ("ildcode", "schemecode", "schemerepl")
MONO_MACROS = {
    "code": None,
    "ild": None,
    "ildmono": None,
    "pathcode": None,
    "nolinkurl": None,
    "url": None,
    "texttt": None,
    "ildsym": COLORS["csym"],
    "interopword": COLORS["cinterop"],
    "ildsf": COLORS["csf"],
    "cpsword": COLORS["cildset"],
    "ildhost": COLORS["chost"],
}
SANS_MACROS = {"ildset": COLORS["cildset"]}
REF_NAMES = {
    "section": ("Section", "Sections"),
    "subsection": ("Section", "Sections"),
    "subsubsection": ("Section", "Sections"),
    "figure": ("Figure", "Figures"),
    "table": ("Table", "Tables"),
    "listing": ("Listing", "Listings"),
    "equation": ("Equation", "Equations"),
}


# --------------------------------------------------------------------------
# small helpers
# --------------------------------------------------------------------------


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def find_math_end(src: str, i: int, closing: str) -> int:
    """Return the index of the closing delimiter, honouring escapes."""
    j = i
    while j < len(src):
        if src.startswith(closing, j):
            return j
        c = src[j]
        if c == "\\":
            j += 2
            continue
        j += 1
    raise ValueError(f"unterminated math starting at {i}: {src[i:i + 60]!r}")


def read_group(src: str, i: int) -> tuple[str, int]:
    """Read a balanced {..} group; src[i] must be '{'."""
    assert src[i] == "{", (i, src[i : i + 20])
    depth = 1
    j = i + 1
    while j < len(src):
        c = src[j]
        if c == "\\":
            j += 2
            continue
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return src[i + 1 : j], j + 1
        j += 1
    raise ValueError(f"unbalanced group at {i}: {src[i : i + 60]!r}")


def read_optional(src: str, i: int) -> tuple[str | None, int]:
    if i >= len(src) or src[i] != "[":
        return None, i
    depth = 0
    j = i + 1
    while j < len(src):
        c = src[j]
        if c == "\\":
            j += 2
            continue
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
        elif c == "]" and depth == 0:
            return src[i + 1 : j], j + 1
        j += 1
    return None, i


def read_cmd(src: str, i: int) -> tuple[str, int]:
    """Read a control sequence starting at src[i] == '\\'."""
    j = i + 1
    if j >= len(src):
        return "", j
    if src[j].isalpha() or src[j] == "@":
        k = j
        while k < len(src) and (src[k].isalpha() or src[k] == "@"):
            k += 1
        name = src[j:k]
        if k < len(src) and src[k] == "*":
            k += 1
        return name, k
    if src[j] == "\\":
        return "\\", j + 1
    return src[j], j + 1


def skip_ws(src: str, i: int) -> int:
    while i < len(src) and src[i] in " \t\n\r":
        i += 1
    return i


def split_top(s: str, sep: str) -> list[str]:
    """Split on a literal separator at brace depth zero."""
    out, depth, start, i = [], 0, 0, 0
    while i < len(s):
        c = s[i]
        if c == "\\":
            i += 2
            continue
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
        elif depth == 0 and s.startswith(sep, i):
            out.append(s[start:i])
            i += len(sep)
            start = i
            continue
        i += 1
    out.append(s[start:])
    return out


# --------------------------------------------------------------------------
# LaTeX source handling
# --------------------------------------------------------------------------


class Source:
    def __init__(self, main: Path, search: list[Path], blind_override: bool | None = None):
        self.main = main
        self.search = [main.parent] + [p for p in search if p != main.parent]
        self.warnings: list[str] = []
        text = self.expand_inputs(read_text(main))
        self.full = self.strip_comments(text)
        self.toggles = self.collect_toggles(self.full)
        if blind_override is not None:
            self.toggles["blindreview"] = blind_override
        body = self.extract_body(self.full)
        self.body = self.process_conditionals(body)
        self.preamble = self.expand_inputs(self.full.split("\\begin{document}")[0])
        pyear = re.search(r"\\newcommand\{\\journalyear\}\{(\d{4})\}", self.full)
        self.year = pyear.group(1) if pyear else ""

    def resolve(self, name: str) -> Path:
        name = name.strip()
        candidates = []
        p = Path(name)
        if p.is_absolute():
            candidates.append(p)
        else:
            for d in self.search:
                candidates.append(d / name)
                candidates.append(d / (name + ".tex"))
        for c in candidates:
            if c.is_file():
                return c
        raise FileNotFoundError(f"cannot resolve LaTeX input {name!r} (searched {self.search})")

    def expand_inputs(self, text: str, seen: set | None = None) -> str:
        seen = set() if seen is None else seen
        pattern = re.compile(r"\\(?:input|include)(?![A-Za-z@])\s*(?:\{([^}]*)\}|([^\s{}%]+))")

        def repl(m: re.Match) -> str:
            name = (m.group(1) or m.group(2) or "").strip()
            path = self.resolve(name)
            if path in seen:
                self.warnings.append(f"recursive \\input ignored: {path}")
                return ""
            return "\n" + self.expand_inputs(read_text(path), seen | {path}) + "\n"

        return pattern.sub(repl, text)

    @staticmethod
    def strip_comments(text: str) -> str:
        out, in_code = [], 0
        begin_re = re.compile(r"\\begin\{(%s)\}" % "|".join(CODE_ENVS))
        end_re = re.compile(r"\\end\{(%s)\}" % "|".join(CODE_ENVS))
        for line in text.split("\n"):
            if begin_re.search(line):
                in_code += 1
            if in_code == 0:
                line = re.sub(r"(?<!\\)%.*$", "", line)
            out.append(line)
            if end_re.search(line):
                in_code = max(0, in_code - 1)
        return "\n".join(out)

    @staticmethod
    def collect_toggles(text: str) -> dict[str, bool]:
        toggles: dict[str, bool] = {}
        for name in re.findall(r"\\newif\\if([A-Za-z@]+)", text):
            toggles[name] = False
        for name in re.findall(r"\\([A-Za-z@]+)true\b", text):
            toggles[name] = True
        for name in re.findall(r"\\([A-Za-z@]+)false\b", text):
            toggles[name] = False
        return toggles

    @staticmethod
    def extract_body(text: str) -> str:
        m = re.search(r"\\begin\{document\}(.*?)\\end\{document\}", text, re.S)
        if not m:
            raise ValueError("no \\begin{document} ... \\end{document} found")
        return m.group(1)

    def process_conditionals(self, body: str) -> str:
        out: list[str] = []
        stack: list[dict] = []
        i = 0
        n = len(body)

        def active() -> bool:
            return all(f["active"] for f in stack)

        while i < n:
            if body[i] == "\\":
                name, j = read_cmd(body, i)
                base = name.rstrip("*")
                if base.startswith("if") and len(base) > 2:
                    cond = self.toggles.get(base[2:], True)
                    stack.append({"active": cond, "taken": cond, "parent": active()})
                    i = j
                    continue
                if base == "else" and stack:
                    f = stack[-1]
                    f["active"] = f["parent"] and not f["taken"]
                    f["taken"] = True
                    i = j
                    continue
                if base == "fi" and stack:
                    stack.pop()
                    i = j
                    continue
                if re.fullmatch(r"[A-Za-z@]+(true|false)", base):
                    i = j
                    continue
            if active():
                out.append(body[i])
            i += 1
        if stack:
            self.warnings.append("unbalanced \\if ... \\fi in document body")
        return "".join(out)


# --------------------------------------------------------------------------
# BibTeX
# --------------------------------------------------------------------------

ACCENTS = {
    '"': "\u0308",
    "'": "\u0301",
    "`": "\u0300",
    "^": "\u0302",
    "~": "\u0303",
    "=": "\u0304",
    ".": "\u0307",
    "c": "\u0327",
    "v": "\u030c",
    "u": "\u0306",
    "H": "\u030b",
}
SUPERSCRIPTS = dict(zip("0123456789+-=()n", "\u2070\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079\u207a\u207b\u207c\u207d\u207e\u207f"))
SPECIAL_LETTERS = {
    "ss": "\u00df",
    "aa": "\u00e5",
    "AA": "\u00c5",
    "ae": "\u00e6",
    "AE": "\u00c6",
    "oe": "\u0153",
    "OE": "\u0152",
    "o": "\u00f8",
    "O": "\u00d8",
    "l": "\u0142",
    "L": "\u0141",
    "i": "\u0131",
    "j": "\u0237",
}


def detex_bib(s: str) -> str:
    s = s.replace("\\$", "\x00DOLLAR\x00")
    for name, ch in SPECIAL_LETTERS.items():
        s = re.sub(r"(?<!\\)\{\\%s\}" % re.escape(name), ch, s)
        s = re.sub(r"(?<!\\)\\%s(?![A-Za-z])" % re.escape(name), ch, s)
    s = re.sub(r"\\text[a-zA-Z]*\s*\{([^{}]*)\}", r"\1", s)
    s = re.sub(r"\\(?:emph|textit|textbf|textrm|textsf|texttt|mathrm|mathit)\s*\{([^{}]*)\}", r"\1", s)
    s = re.sub(r"\\([`'\"^~=.]|[cvuH])\s*\{?([A-Za-z])\}?", lambda m: m.group(2) + ACCENTS.get(m.group(1), ""), s)

    def sup(m: re.Match) -> str:
        inner = m.group(1) or m.group(2) or ""
        return "".join(SUPERSCRIPTS.get(c, c) for c in inner)

    s = re.sub(r"\^\s*\{([^{}]*)\}|\^\s*(\S)", sup, s)
    s = s.replace("$", "")
    s = s.replace("\\&", "&").replace("\\%", "%").replace("\\#", "#").replace("\\_", "_")
    s = s.replace("~", " ").replace("---", "\u2014").replace("--", "\u2013")
    s = s.replace("{", "").replace("}", "")
    s = s.replace("\x00DOLLAR\x00", "$")
    s = re.sub(r"\s+", " ", s).strip()
    return unicodedata.normalize("NFC", s)


def parse_bib_fields(body: str) -> dict[str, str]:
    fields: dict[str, str] = {}
    i, n = 0, len(body)
    while i < n:
        while i < n and (body[i].isspace() or body[i] == ","):
            i += 1
        if i >= n:
            break
        m = re.match(r"([A-Za-z][A-Za-z0-9_-]*)\s*=", body[i:])
        if not m:
            break
        name = m.group(1).lower()
        i += m.end()
        while i < n and body[i].isspace():
            i += 1
        if i < n and body[i] == "{":
            val, i = read_group(body, i)
        elif i < n and body[i] == '"':
            j = i + 1
            depth = 0
            while j < n and (body[j] != '"' or depth):
                if body[j] == "{":
                    depth += 1
                elif body[j] == "}":
                    depth -= 1
                j += 1
            val, i = body[i + 1 : j], j + 1
        else:
            j = i
            while j < n and body[j] not in ",":
                j += 1
            val, i = body[i:j].strip(), j
        fields[name] = val
    return fields


def parse_bib(path: Path) -> dict[str, dict]:
    text = read_text(path)
    entries: dict[str, dict] = {}
    i = 0
    head = re.compile(r"@([A-Za-z]+)\s*\{\s*([^,\s]+)\s*,")
    while True:
        m = head.search(text, i)
        if not m:
            break
        etype, key = m.group(1).lower(), m.group(2)
        j, depth = m.end(), 1
        while j < len(text) and depth:
            c = text[j]
            if c == "{":
                depth += 1
            elif c == "}":
                depth -= 1
            j += 1
        fields = parse_bib_fields(text[m.end() : j - 1])
        fields["_type"] = etype
        entries[key] = fields
        i = j
    return entries


def split_authors(field: str) -> list[str]:
    return [a.strip() for a in split_top(field, " and ") if a.strip()]


def author_last_name(author: str) -> str:
    stripped = author.strip().strip("{}").strip()
    parts = split_top(stripped, ",")
    if len(parts) > 1:
        return detex_bib(parts[0]).strip()
    words = [w for w in re.split(r"\s+", detex_bib(stripped)) if w]
    return words[-1] if words else stripped


def format_author(author: str) -> str:
    raw = author.strip()
    if raw.startswith("{") and raw.endswith("}"):
        return detex_bib(raw)
    parts = split_top(raw, ",")
    if len(parts) > 1:
        last = detex_bib(parts[0]).strip()
        firsts = detex_bib(parts[1]).strip()
    else:
        words = [w for w in re.split(r"\s+", detex_bib(raw).strip()) if w]
        if not words:
            return raw
        last, firsts = words[-1], " ".join(words[:-1])
    initials = []
    for tok in re.split(r"\s+", firsts):
        tok = tok.strip()
        if not tok:
            continue
        if re.fullmatch(r"(?:[A-Z]\.)+", tok):
            initials.append(tok)
        else:
            initials.append(tok[0].upper() + ".")
    if initials:
        return f"{last}, {' '.join(initials)}"
    return last


def format_authors(field: str) -> str:
    authors = split_authors(field)
    if not authors:
        return ""
    names = [format_author(a) for a in authors]
    if len(names) == 1:
        return names[0]
    if len(names) == 2:
        return f"{names[0]} and {names[1]}"
    return ", ".join(names[:-1]) + ", and " + names[-1]


def format_reference(entry: dict) -> str:
    t = entry.get("_type", "misc")
    authors = format_authors(entry.get("author", entry.get("editor", "")))
    year = detex_bib(entry.get("year", "n.d."))
    title = detex_bib(entry.get("title", "")).rstrip(".")
    pieces = []
    if authors:
        pieces.append(f"{authors} {year}.")
    else:
        org = detex_bib(entry.get("organization", entry.get("institution", "")))
        pieces.append(f"{org} {year}." if org else f"{year}.")
    if title:
        pieces.append(f"{title}.")
    pages = detex_bib(entry.get("pages", ""))
    if t == "article":
        journal = detex_bib(entry.get("journal", ""))
        volume = detex_bib(entry.get("volume", ""))
        number = detex_bib(entry.get("number", entry.get("issue", "")))
        bits = journal
        if volume:
            bits += f" {volume}"
        if number:
            bits += f", {number}"
        if pages:
            bits += f", {pages}"
        pieces.append(bits.strip().rstrip(",") + ".")
    elif t in ("inproceedings", "conference"):
        book = detex_bib(entry.get("booktitle", "Proceedings"))
        tail = f"In {book}"
        if pages:
            tail += f", {pages}"
        tail += "."
        pieces.append(tail)
        pub = detex_bib(entry.get("publisher", ""))
        addr = detex_bib(entry.get("address", ""))
        if pub and addr:
            pieces.append(f"{pub}, {addr}.")
        elif pub:
            pieces.append(f"{pub}.")
    elif t in ("book", "manual"):
        edition = detex_bib(entry.get("edition", ""))
        if edition:
            pieces.append(f"{edition} ed.")
        pub = detex_bib(entry.get("publisher", ""))
        addr = detex_bib(entry.get("address", ""))
        if pub and addr:
            pieces.append(f"{pub}, {addr}.")
        elif pub:
            pieces.append(f"{pub}.")
    elif t == "inbook" or t == "incollection":
        book = detex_bib(entry.get("booktitle", ""))
        tail = f"In {book}"
        if pages:
            tail += f", {pages}"
        tail += "."
        pieces.append(tail)
        pub = detex_bib(entry.get("publisher", ""))
        if pub:
            pieces.append(f"{pub}.")
    elif t in ("phdthesis", "mastersthesis"):
        kind = "Ph.D. dissertation" if t == "phdthesis" else "Master's thesis"
        pieces.append(f"{kind}.")
        school = detex_bib(entry.get("school", ""))
        addr = detex_bib(entry.get("address", ""))
        if school and addr:
            pieces.append(f"{school}, {addr}.")
        elif school:
            pieces.append(f"{school}.")
    elif t == "techreport":
        num = detex_bib(entry.get("number", ""))
        inst = detex_bib(entry.get("institution", ""))
        pieces.append(f"Technical Report {num}." if num else "Technical report.")
        if inst:
            pieces.append(f"{inst}.")
    note = detex_bib(entry.get("note", ""))
    if note and note.rstrip(".").lower() not in " ".join(pieces).lower():
        pieces.append(note.rstrip(".") + ".")
    doi = detex_bib(entry.get("doi", ""))
    url = detex_bib(entry.get("url", ""))
    if doi:
        pieces.append(f"DOI: https://doi.org/{doi}.")
    elif url.lower().startswith("https://doi.org/"):
        pieces.append(f"DOI: {url}.")
    elif url:
        pieces.append(f"{url}.")
    return " ".join(p.strip() for p in pieces if p.strip())


def compress_numbers(nums: list[int]) -> str:
    if not nums:
        return ""
    nums = sorted(set(nums))
    runs, start, prev = [], nums[0], nums[0]
    for n in nums[1:]:
        if n == prev + 1:
            prev = n
            continue
        runs.append((start, prev))
        start = prev = n
    runs.append((start, prev))
    chunks = []
    for a, b in runs:
        if b - a >= 2:
            chunks.append(f"{a}\u2013{b}")
        elif b - a == 1:
            chunks.extend([str(a), str(b)])
        else:
            chunks.append(str(a))
    return ", ".join(chunks)


# --------------------------------------------------------------------------
# document model
# --------------------------------------------------------------------------


@dataclass
class Job:
    kind: str
    source: str
    width_pt: float | None = None
    env: str = "ildcode"
    jid: int = 0


@dataclass
class Rendered:
    png: bytes
    width_pt: float
    height_pt: float
    depth_pt: float


def merge_style(st: dict, **kw) -> dict:
    out = {k: v for k, v in st.items() if v}
    out.update({k: v for k, v in kw.items() if v is not None})
    return out


# --------------------------------------------------------------------------
# converter
# --------------------------------------------------------------------------


class Converter:
    def __init__(self, src: Source, template: Path, drop_notes: bool):
        self.src = src
        self.template = template
        self.drop_notes = drop_notes
        self.labels: dict[str, tuple[str, str]] = {}
        self.blocks: list[dict] = []
        self.jobs: list[Job] = []
        self.elems: list[dict] = []
        self.footnotes: list[dict] = []
        self.cited: list[str] = []
        self.bib: dict[str, dict] = {}
        self.ref_numbers: dict[str, int] = {}
        self.images: dict[int, Rendered] = {}
        self.media: list[tuple[str, bytes]] = []
        self.media_index = 0
        self.warnings = src.warnings
        self.title_prefix_done = False
        self.counters = {"section": [0, 0, 0], "figure": 0, "table": 0, "listing": 0, "equation": 0}

    # -- inline parsing ----------------------------------------------------

    def inline(self, raw: str, style: dict | None = None) -> list[tuple]:
        style = dict(style or {})
        runs: list[tuple] = []
        buf: list[str] = []
        i, n = 0, len(raw)

        def flush() -> None:
            if not buf:
                return
            text = "".join(buf)
            buf.clear()
            text = re.sub(r"[ \t\n\r]+", " ", text)
            if not text:
                return
            if text == " " and (not runs or runs[-1][0] == "text" and runs[-1][1].endswith(" ")):
                return
            runs.append(("text", text, dict(style)))

        def add_text(text: str, st: dict | None = None) -> None:
            if not text:
                return
            runs.append(("text", text, dict(st if st is not None else style)))

        while i < n:
            c = raw[i]

            if c == "$":
                if raw.startswith("$$", i):
                    j = find_math_end(raw, i + 2, "$$")
                    flush()
                    runs.append(("math", self.job("display", raw[i + 2 : j])))
                    i = j + 2
                else:
                    j = find_math_end(raw, i + 1, "$")
                    flush()
                    runs.append(("math", self.job("inline", raw[i + 1 : j])))
                    i = j + 1
                continue

            if raw.startswith("\\(", i):
                j = find_math_end(raw, i + 2, "\\)")
                flush()
                runs.append(("math", self.job("inline", raw[i + 2 : j])))
                i = j + 2
                continue

            if raw.startswith("\\[", i):
                j = find_math_end(raw, i + 2, "\\]")
                flush()
                runs.append(("math", self.job("display", raw[i + 2 : j])))
                i = j + 2
                continue

            if c == "{":
                group, i = read_group(raw, i)
                flush()
                runs.extend(self.inline(group, style))
                continue

            if c == "}":
                i += 1
                continue

            if c == "~":
                buf.append("\u00a0")
                i += 1
                continue

            if raw.startswith("---", i):
                buf.append("\u2014")
                i += 3
                continue
            if raw.startswith("--", i):
                buf.append("\u2013")
                i += 2
                continue
            if raw.startswith("``", i):
                buf.append("\u201c")
                i += 2
                continue
            if raw.startswith("''", i):
                buf.append("\u201d")
                i += 2
                continue
            if c == "`":
                buf.append("\u2018")
                i += 1
                continue

            if c != "\\":
                buf.append(c)
                i += 1
                continue

            name, j = read_cmd(raw, i)
            if name and name[-1] == "*":
                name = name[:-1]
            if name and name[0].isalpha():
                j = skip_ws(raw, j)

            def arg() -> str:
                nonlocal j, i
                j = skip_ws(raw, j)
                if j < n and raw[j] == "{":
                    val, j = read_group(raw, j)
                    i = j
                    return val
                return ""

            def opt() -> str | None:
                nonlocal j, i
                j = skip_ws(raw, j)
                val, j2 = read_optional(raw, j)
                if val is not None:
                    j = j2
                    i = j
                return val

            i = j

            if name == "\\":
                flush()
                runs.append(("br",))
                continue
            if name == " " or name == "," or name == ";" or name == ":" or name == "!":
                buf.append(" ")
                continue
            if name in ("&", "%", "#", "_", "$", "{", "}"):
                buf.append(name)
                continue
            if name in ("ldots", "dots", "textellipsis"):
                buf.append("\u2026")
                continue
            if name in ("bfseries", "itshape", "slshape", "ttfamily", "rmfamily", "sffamily",
                        "normalfont", "selectfont", "normalsize", "small", "footnotesize",
                        "scriptsize", "large", "Large", "centering", "raggedright", "noindent",
                        "indent", "hfill", "vfill", "newline", "linebreak", "nobreakspace",
                        "ignorespaces", "unskip", "relax", "protect"):
                flush()
                if name == "bfseries":
                    style["bold"] = True
                elif name in ("itshape", "slshape"):
                    style["italic"] = True
                elif name == "ttfamily":
                    style["mono"] = True
                elif name == "sffamily":
                    style["sans"] = True
                elif name == "normalfont":
                    for k in ("bold", "italic", "mono", "sans", "smallcaps"):
                        style.pop(k, None)
                continue
            if name == "par":
                flush()
                continue
            if name in ("emph", "textit", "textsl"):
                group = arg()
                flush()
                runs.extend(self.inline(group, merge_style(style, italic=True)))
                continue
            if name == "textsc":
                group = arg()
                flush()
                runs.extend(self.inline(group, merge_style(style, smallcaps=True)))
                continue
            if name in ("textbf",):
                group = arg()
                flush()
                runs.extend(self.inline(group, merge_style(style, bold=True)))
                continue
            if name in ("textrm", "textnormal", "textup", "mbox", "hbox", "text"):
                group = arg()
                flush()
                st = dict(style)
                if name in ("textrm", "textnormal", "textup"):
                    st.pop("mono", None)
                    st.pop("sans", None)
                runs.extend(self.inline(group, st))
                continue
            if name in MONO_MACROS:
                group = arg()
                flush()
                st = merge_style(style, mono=True)
                if MONO_MACROS[name]:
                    st["color"] = MONO_MACROS[name]
                runs.extend(self.wrap_text(group, st))
                continue
            if name in SANS_MACROS:
                group = arg()
                flush()
                st = merge_style(style, sans=True, bold=True)
                if SANS_MACROS[name]:
                    st["color"] = SANS_MACROS[name]
                runs.extend(self.wrap_text(group, st))
                continue
            if name == "ildconst":
                first, second = arg(), arg()
                flush()
                st = merge_style(style, mono=True, color=COLORS["chost"])
                runs.extend(self.wrap_text("#" + first, st))
                runs.extend(self.wrap_text(second, merge_style(st, sub=True)))
                continue
            if name == "lang":
                flush()
                add_text("ILD ")
                continue
            if name.startswith("cite"):
                while opt() is not None:
                    pass
                keys = [k.strip() for k in arg().split(",") if k.strip()]
                flush()
                runs.append(("cite", keys))
                continue
            if name in ("cref", "Cref", "autoref", "ref", "eqref", "pageref"):
                keys = [k.strip() for k in arg().split(",") if k.strip()]
                flush()
                runs.append(("ref", name, keys))
                continue
            if name == "label":
                arg()
                continue
            if name == "index":
                arg()
                continue
            if name == "footnote":
                group = arg()
                flush()
                fid = len(self.footnotes) + 1
                fn_runs = self.inline(group, {"size": 20})
                self.footnotes.append({"id": fid, "runs": fn_runs})
                runs.append(("fn", fid))
                continue
            if name == "fontsize":
                size_src = arg()
                arg()
                try:
                    style["size"] = int(size_src) * 2
                except ValueError:
                    pass
                continue
            if name in ("hspace", "vspace", "hskip", "vskip", "kern", "raisebox", "scalebox",
                        "resizebox", "rule", "color", "textwidth", "linewidth", "columnwidth",
                        "setlength", "addtolength", "setcounter", "addtocounter", "refstepcounter",
                        "Description", "includegraphics", "lstinputlisting", "caption", "begin", "end"):
                if name in ("raisebox", "scalebox", "resizebox"):
                    arg()
                    if name != "raisebox":
                        arg()
                    arg()
                elif name in ("hspace", "vspace", "hskip", "vskip", "kern", "rule"):
                    arg()
                    if name == "rule":
                        arg()
                elif name in ("color", "Description", "caption", "includegraphics"):
                    arg()
                elif name in ("textwidth", "linewidth", "columnwidth"):
                    pass
                elif name in ("setlength", "addtolength", "setcounter", "addtocounter", "refstepcounter"):
                    arg()
                    arg()
                continue
            if name == "textcolor":
                color = arg()
                group = arg()
                flush()
                st = merge_style(style)
                if color in COLORS:
                    st["color"] = COLORS[color]
                elif re.fullmatch(r"[0-9A-Fa-f]{6}", color):
                    st["color"] = color.upper()
                runs.extend(self.inline(group, st))
                continue
            if name == "ensuremath":
                flush()
                runs.append(("math", self.job("inline", arg())))
                continue
            if name in ("sqrt", "frac", "mathrm", "mathit", "mathbf", "mathcal", "overline",
                        "underline", "vec", "hat", "bar", "tilde", "textsuperscript"):
                group = arg()
                flush()
                runs.extend(self.inline(group, style))
                continue

            if name in self.src.toggles:
                continue
            if name.endswith("true") or name.endswith("false"):
                continue

            self.warn_once(f"unknown inline command \\{name}")
            continue

        flush()
        return self.merge_text_runs(runs)

    def wrap_text(self, group: str, style: dict) -> list[tuple]:
        """Turn a macro argument into a single run, preserving nested markup
        only where it does not change the macro's own font/colour."""
        if re.search(r"\\(?:cite|cref|Cref|ref|label|footnote)\b", group):
            return self.inline(group, style)
        text = detex_inline_text(group)
        return [("text", text, dict(style))] if text else []

    @staticmethod
    def merge_text_runs(runs: list[tuple]) -> list[tuple]:
        out: list[tuple] = []
        for r in runs:
            if r[0] == "text":
                if not r[1]:
                    continue
                if out and out[-1][0] == "text" and out[-1][2] == r[2]:
                    out[-1] = ("text", out[-1][1] + r[1], r[2])
                else:
                    out.append(r)
            elif r[0] == "br" and out and out[-1][0] == "text" and out[-1][1].endswith(" "):
                out[-1] = ("text", out[-1][1].rstrip(" "), out[-1][2])
                out.append(r)
            else:
                out.append(r)
        cleaned: list[tuple] = []
        for r in out:
            if r[0] == "text":
                r = ("text", re.sub(r"  +", " ", r[1]), r[2])
            cleaned.append(r)
        return cleaned

    def warn_once(self, msg: str) -> None:
        if msg not in self.warnings:
            self.warnings.append(msg)

    # -- jobs --------------------------------------------------------------

    def job(self, kind: str, source: str, width_pt: float | None = None, env: str = "ildcode") -> int:
        self.jobs.append(Job(kind=kind, source=source, width_pt=width_pt, env=env, jid=len(self.jobs)))
        return len(self.jobs) - 1

    # -- block parsing -----------------------------------------------------

    def parse(self) -> None:
        body = self.src.body
        self.blocks = self.parse_blocks(body)
        self.attach_captions()
        self.number_blocks()

    def parse_blocks(self, text: str, base: int = 0) -> list[dict]:
        blocks: list[dict] = []
        buf: list[str] = []
        i, n = 0, len(text)

        def flush() -> None:
            raw = "".join(buf)
            buf.clear()
            raw = raw.strip()
            if re.search(r"[^\s{}]", raw):
                blocks.append({"type": "para", "raw": raw})

        while i < n:
            if text[i] == "\\":
                m = re.match(r"\\(section|subsection|subsubsection)\*?", text[i:])
                if m and (m.end() + i >= n or text[i + m.end()] in " {["):
                    flush()
                    j = i + m.end()
                    starred = text[i + len(m.group(0)) - 1] == "*"
                    j = skip_ws(text, j)
                    title, j = read_group(text, j)
                    j = skip_ws(text, j)
                    label = None
                    lm = re.match(r"\\label\s*\{([^}]*)\}", text[j:])
                    if lm:
                        label = lm.group(1)
                        j += lm.end()
                    level = {"section": 1, "subsection": 2, "subsubsection": 3}[m.group(1)]
                    blocks.append({"type": "heading", "level": level, "raw": title,
                                   "label": label, "numbered": not starred})
                    i = j
                    continue

                m = re.match(r"\\begin\{([^}]+)\}", text[i:])
                if m:
                    env = m.group(1)
                    inner, end = self.find_env(text, i, env)
                    flush()
                    blocks.append({"type": "env", "env": env, "raw": inner})
                    i = end
                    continue

                m = re.match(r"\\lstcaption\s*", text[i:])
                if m:
                    flush()
                    j = i + m.end()
                    caption, j = read_group(text, j)
                    j = skip_ws(text, j)
                    lm = re.match(r"\\label\s*\{([^}]*)\}", text[j:])
                    label = None
                    if lm:
                        label = lm.group(1)
                        j += lm.end()
                    blocks.append({"type": "lstcaption", "raw": caption, "label": label})
                    i = j
                    continue

                m = re.match(r"\\keywords\s*", text[i:])
                if m:
                    flush()
                    j = i + m.end()
                    kw, j = read_group(text, j)
                    blocks.append({"type": "keywords", "raw": kw})
                    i = j
                    continue

                m = re.match(r"\\(bibliographystyle|bibliography)\s*(\{[^}]*\})?", text[i:])
                if m:
                    flush()
                    if m.group(1) == "bibliography":
                        blocks.append({"type": "bib", "raw": m.group(2) or "{}"})
                    i += m.end()
                    continue

            blank = re.match(r"\n[ \t]*\n", text[i:])
            if blank:
                flush()
                i += blank.end()
                continue
            buf.append(text[i])
            i += 1
        flush()
        return blocks

    @staticmethod
    def find_env(text: str, start: int, env: str) -> tuple[str, int]:
        begin, end = "\\begin{%s}" % env, "\\end{%s}" % env
        body_start = start + len(begin)
        depth, j = 1, body_start
        while True:
            b = text.find(begin, j)
            e = text.find(end, j)
            if e == -1:
                raise ValueError(f"unterminated \\begin{{{env}}}")
            if b != -1 and b < e:
                depth += 1
                j = b + len(begin)
            else:
                depth -= 1
                if depth == 0:
                    return text[body_start:e], e + len(end)
                j = e + len(end)

    def attach_captions(self) -> None:
        merged: list[dict] = []
        for b in self.blocks:
            if b["type"] == "lstcaption" and merged and merged[-1]["type"] == "env" \
                    and merged[-1].get("env") in ("listingbox",) + CODE_ENVS:
                target = merged[-1]
                target["caption"] = b["raw"]
                target["label"] = b["label"]
            else:
                merged.append(b)
        self.blocks = merged

    def register_label(self, label: str | None, kind: str, num: str) -> None:
        if not label:
            return
        if label in self.labels:
            self.warn_once(f"duplicate label {label}")
        self.labels[label] = (kind, num)

    def number_blocks(self) -> None:
        sec = [0, 0, 0]
        for b in self.blocks:
            t = b["type"]
            if t == "heading":
                level = b["level"]
                if b["numbered"]:
                    sec[level - 1] += 1
                    for k in range(level, 3):
                        sec[k] = 0
                    b["num"] = ".".join(str(x) for x in sec[:level])
                else:
                    b["num"] = None
                self.register_label(b.get("label"), "section", b.get("num") or "")
            elif t == "env":
                env = b["env"]
                inner = b["raw"]
                label = b.get("label") or self.first_label(inner)
                if env in ("cfighere", "ctab"):
                    self.counters["table"] += 1
                    b.update(type="table", num=str(self.counters["table"]), label=label)
                    self.register_label(label, "table", b["num"])
                elif env in ("listingbox",) + CODE_ENVS:
                    self.counters["listing"] += 1
                    b.update(type="listing", num=str(self.counters["listing"]), label=label)
                    if label:
                        self.register_label(label, "listing", b["num"])
                elif env in ("equation", "equation*", "displaymath"):
                    if env == "equation*":
                        b.update(type="equation", num=None)
                    else:
                        self.counters["equation"] += 1
                        b.update(type="equation", num=str(self.counters["equation"]))
                        self.register_label(label, "equation", b["num"])
                    b["label"] = label
                elif env in ("figure", "figure*"):
                    self.counters["figure"] += 1
                    b.update(type="figure", num=str(self.counters["figure"]), label=label)
                    self.register_label(label, "figure", b["num"])
                elif env in ("note", "notefixed", "commentbox", "reviewbox"):
                    b.update(type="note")
                elif env == "abstract":
                    b.update(type="abstract")
                elif env == "itemize":
                    b.update(type="itemize")
                elif env == "enumerate":
                    b.update(type="enumerate")
                elif env == "center":
                    b.update(type="center")
                else:
                    b.update(type="para", raw=inner)
            elif t == "lstcaption":
                b.update(type="para", raw=b["raw"])

    @staticmethod
    def first_label(text: str) -> str | None:
        m = re.search(r"\\label\s*\{([^}]*)\}", text)
        return m.group(1) if m else None

    # -- numbering resolution ---------------------------------------------

    def ref_text(self, cmd: str, keys: list[str]) -> str:
        if cmd in ("ref", "pageref"):
            out = []
            for k in keys:
                kindnum = self.labels.get(k)
                if not kindnum or not kindnum[1]:
                    out.append("??")
                    self.warn_once(f"undefined reference {k}")
                else:
                    out.append(kindnum[1])
            return ", ".join(out)
        if cmd == "eqref":
            out = []
            for k in keys:
                kindnum = self.labels.get(k)
                if not kindnum:
                    out.append("(??)")
                    self.warn_once(f"undefined reference {k}")
                else:
                    out.append(f"({kindnum[1]})")
            return ", ".join(out)

        groups: dict[str, list[str]] = {}
        order: list[str] = []
        for k in keys:
            kindnum = self.labels.get(k)
            if not kindnum:
                self.warn_once(f"undefined reference {k}")
                groups.setdefault("section", []).append("??")
                if "section" not in order:
                    order.append("section")
                continue
            kind = kindnum[0]
            if kind not in groups:
                groups[kind] = []
                order.append(kind)
            groups[kind].append(kindnum[1])

        parts = []
        for kind in order:
            nums = groups[kind]
            single, plural = REF_NAMES.get(kind, ("?", "?"))
            name = single if len(nums) == 1 else plural
            if len(nums) == 1:
                parts.append(f"{name} {nums[0]}")
            else:
                parts.append(f"{name} " + ", ".join(nums[:-1]) + " and " + nums[-1])
        text = ", ".join(parts)
        if cmd == "Cref" and text:
            text = text[0].upper() + text[1:]
        return text

    def cite_text(self, keys: list[str]) -> str:
        nums = []
        for k in keys:
            if k not in self.ref_numbers:
                self.warn_once(f"citation {k} not found in bibliography")
                continue
            nums.append(self.ref_numbers[k])
        return "[" + compress_numbers(nums) + "]"

    def load_bibliography(self) -> None:
        cited: list[str] = []
        for b in self.blocks:
            for raw in self.walk_raw(b):
                for m in re.finditer(r"\\cite[a-zA-Z]*\*?(?:\[[^\]]*\])*\{([^}]*)\}", raw):
                    for k in m.group(1).split(","):
                        k = k.strip()
                        if k and k not in cited:
                            cited.append(k)

        bib_path = None
        for b in self.blocks:
            if b["type"] == "bib":
                name = b["raw"].strip("{}").strip()
                try:
                    bib_path = self.src.resolve(name + ".bib")
                except FileNotFoundError:
                    try:
                        bib_path = self.src.resolve(name)
                    except FileNotFoundError:
                        self.warnings.append(f"bibliography file not found: {name}")
        if bib_path is None:
            try:
                bib_path = self.src.resolve("refs.bib")
            except FileNotFoundError:
                return
        self.bib = parse_bib(bib_path)
        ordered = sorted(cited, key=lambda k: (author_last_name(self.bib.get(k, {}).get("author", "")).lower(), k))
        self.ref_numbers = {k: i + 1 for i, k in enumerate(ordered)}
        self.cited = ordered

    def walk_raw(self, b: dict):
        if "raw" in b:
            yield b["raw"]
        for key in ("caption",):
            if b.get(key):
                yield b[key]

    # -- emission ----------------------------------------------------------

    def emit(self) -> None:
        for b in self.blocks:
            t = b["type"]
            if t == "heading":
                self.emit_heading(b)
            elif t == "para":
                runs = self.inline(b["raw"].strip())
                if runs:
                    self.elems.append({"type": "para", "runs": runs})
            elif t == "abstract":
                runs = [("text", "Abstract: ", {"bold": True, "italic": True, "size": 20})]
                runs += self.inline(b["raw"].strip(), {"italic": True, "size": 20})
                self.elems.append({"type": "para", "runs": runs, "size": 20})
            elif t == "keywords":
                runs = [("text", "Keywords: ", {"bold": True, "italic": True, "size": 20})]
                runs += self.inline(b["raw"].strip(), {"italic": True, "size": 20})
                self.elems.append({"type": "para", "runs": runs, "size": 20})
            elif t == "center":
                self.emit_center(b["raw"])
            elif t == "itemize":
                self.emit_items(b["raw"], 0, "itemize")
            elif t == "enumerate":
                self.emit_items(b["raw"], 0, "enumerate")
            elif t == "equation":
                self.emit_equation(b)
            elif t == "listing":
                self.emit_listing(b)
            elif t == "table":
                self.emit_table(b)
            elif t == "note":
                if not self.drop_notes:
                    self.emit_note(b)
            elif t == "figure":
                self.emit_figure(b)
            elif t == "bib":
                self.emit_references()

    def emit_heading(self, b: dict) -> None:
        style = {"bold": True}
        runs = self.inline(b["raw"], style)
        if b.get("num"):
            runs = [("text", b["num"] + " ", {"bold": True})] + runs
        self.elems.append({"type": "heading", "level": b["level"], "runs": runs})

    def emit_center(self, raw: str) -> None:
        parts: list[str] = []
        i, n = 0, len(raw)
        while i < n:
            if raw[i].isspace():
                i += 1
                continue
            if raw[i] == "{":
                group, i = read_group(raw, i)
                for piece in split_top(group, "\\par"):
                    if piece.strip():
                        parts.append(piece.strip())
                continue
            if raw[i] == "\\":
                name, j = read_cmd(raw, i)
                if name in ("vspace", "hspace", "vskip", "hskip"):
                    _, j = read_group(raw, skip_ws(raw, j))
                elif name == "fontsize":
                    _, j = read_group(raw, skip_ws(raw, j))
                    _, j = read_group(raw, skip_ws(raw, j))
                elif name == "rule":
                    _, j = read_group(raw, skip_ws(raw, j))
                    _, j = read_group(raw, skip_ws(raw, j))
                i = j
                continue
            i += 1
        if not parts:
            return
        title_runs = self.inline(parts[0], {"bold": True, "size": 28})
        self.elems.append({"type": "title", "runs": title_runs})
        for p in parts[1:]:
            runs = self.inline(p, {})
            if runs:
                self.elems.append({"type": "authors", "runs": runs})

    def emit_items(self, raw: str, level: int, kind: str) -> None:
        items = split_items(raw)
        for idx, item in enumerate(items):
            if not item.strip():
                continue
            sub = self.parse_blocks(item)
            label = f"{idx + 1}." if kind == "enumerate" else "\u2022"
            first_para = True
            for sb in sub:
                if sb["type"] == "para" and first_para:
                    runs = [("text", label + "\t", {})] + self.inline(sb["raw"])
                    self.elems.append({"type": "bullet", "level": level, "runs": runs, "kind": kind})
                    first_para = False
                elif sb["type"] == "para":
                    self.elems.append({"type": "bullet", "level": level, "runs": self.inline(sb["raw"]), "kind": kind, "cont": True})
                elif sb["type"] == "env" and sb["env"] in ("itemize", "enumerate"):
                    self.emit_items(sb["raw"], level + 1, sb["env"])
                else:
                    self.emit_one(sb)
            if first_para:
                self.elems.append({"type": "bullet", "level": level, "runs": [("text", label, {})], "kind": kind})

    def emit_one(self, b: dict) -> None:
        saved = self.blocks
        try:
            self.blocks = [b]
            self.emit_single(b)
        finally:
            self.blocks = saved

    def emit_single(self, b: dict) -> None:
        t = b["type"]
        if t == "env":
            env = b["env"]
            if env in ("itemize", "enumerate"):
                self.emit_items(b["raw"], 0, env)
            elif env in ("note", "notefixed", "commentbox", "reviewbox"):
                if not self.drop_notes:
                    self.emit_note(b)
            elif env in ("cfighere", "ctab"):
                self.emit_table(b)
            elif env in ("listingbox", "ildcode", "schemecode", "schemerepl"):
                self.emit_listing(b)
            elif env in ("equation", "equation*", "displaymath"):
                self.emit_equation(b)
            else:
                runs = self.inline(b["raw"].strip())
                if runs:
                    self.elems.append({"type": "para", "runs": runs})
        else:
            self.emit_single_block(b)

    def emit_single_block(self, b: dict) -> None:
        t = b["type"]
        if t == "para":
            runs = self.inline(b["raw"].strip())
            if runs:
                self.elems.append({"type": "para", "runs": runs})
        elif t == "heading":
            self.emit_heading(b)

    def emit_equation(self, b: dict) -> None:
        src = strip_latex_labels(b["raw"])
        jid = self.job("display", self.resolve_refs_in_math(src))
        self.elems.append({"type": "equation", "jid": jid, "num": b.get("num")})

    def emit_listing(self, b: dict) -> None:
        env = b.get("env", "ildcode")
        inner = b["raw"]
        m = None if env in CODE_ENVS else re.search(
            r"\\begin\{(%s)\}(.*?)\\end\{\1\}" % "|".join(CODE_ENVS), inner, re.S)
        if m:
            env = m.group(1)
            code = m.group(2)
        else:
            code = inner
        code = code.strip("\n")
        jid = self.job("listing", code, width_pt=LISTING_WIDTH_PT, env=env)
        caption_runs = []
        if b.get("caption"):
            caption_runs = [("text", f"Listing {b['num']}: ", {"bold": True})]
            caption_runs += self.inline(b["caption"])
        self.elems.append({"type": "listing", "jid": jid, "caption": caption_runs})

    def emit_table(self, b: dict) -> None:
        env = b["env"]
        inner = b["raw"]
        if env == "cfighere":
            args = []
            j = 0
            for _ in range(4):
                j = skip_ws(inner, j)
                val, j = read_group(inner, j)
                args.append(val)
            name, _counter, caption, _desc = args
            content = inner[j:]
        elif env == "ctab":
            args = []
            j = 0
            for _ in range(3):
                j = skip_ws(inner, j)
                val, j = read_group(inner, j)
                args.append(val)
            name, caption, _desc = args
            content = inner[j:]
        else:
            self.warn_once(f"unsupported table environment {env}")
            return
        content = strip_latex_labels(content)
        content, notes = extract_footnotes(content, len(self.footnotes) + 1)
        fn_runs = []
        for note in notes:
            fid = len(self.footnotes) + 1
            self.footnotes.append({"id": fid, "runs": self.inline(note, {"size": 20})})
            fn_runs.append(("fn", fid))
        content = self.resolve_refs_anywhere(content)
        jid = self.job("table", content)
        caption_runs = [("text", f"Table {b['num']}: ", {"bold": True})]
        caption_runs += self.inline(caption)
        self.elems.append({"type": "table", "jid": jid, "caption": caption_runs, "fn": fn_runs})

    def emit_note(self, b: dict) -> None:
        env = b["env"]
        content = self.resolve_refs_anywhere(b["raw"])
        jid = self.job("note", "\\begin{%s}\n%s\n\\end{%s}" % (env, content, env), width_pt=TEXT_WIDTH_PT)
        self.elems.append({"type": "note", "jid": jid})

    def emit_figure(self, b: dict) -> None:
        inner = b["raw"]
        m = re.search(r"\\includegraphics\s*(?:\[[^\]]*\])?\s*\{([^}]*)\}", inner)
        caption = ""
        cm = re.search(r"\\caption\s*\{", inner)
        if cm:
            caption, _ = read_group(inner, cm.end() - 1)
        if not m:
            self.warn_once("figure without \\includegraphics skipped")
            return
        path = self.src.resolve(m.group(1))
        if path.suffix.lower() == ".png":
            data = path.read_bytes()
            w_pt, h_pt = png_size(data)
            jid = self.job("raster", "")
            self.jobs[jid].extra = (data, w_pt, h_pt)  # type: ignore[attr-defined]
        elif path.suffix.lower() == ".pdf":
            jid = self.job("pdfimage", "")
            self.jobs[jid].extra = (str(path),)  # type: ignore[attr-defined]
        else:
            self.warn_once(f"unsupported figure format {path.suffix} ({path.name})")
            return
        caption_runs = [("text", f"Figure {b['num']}: ", {"bold": True})]
        caption_runs += self.inline(caption) if caption else []
        self.elems.append({"type": "figure", "jid": jid, "caption": caption_runs})

    def emit_references(self) -> None:
        if any(e.get("type") == "reference" for e in self.elems):
            return
        self.elems.append({"type": "heading", "level": 1, "runs": [("text", "References", {"bold": True})], "unnumbered": True})
        for key in self.cited:
            entry = self.bib.get(key, {})
            text = format_reference(entry) if entry else key
            self.elems.append({"type": "reference", "num": self.ref_numbers[key], "raw": text})

    def emit_references_if_needed(self) -> None:
        if not any(e.get("type") == "reference" for e in self.elems) and self.cited:
            self.emit_references()

    def resolve_refs_anywhere(self, text: str) -> str:
        def repl(m: re.Match) -> str:
            cmd, keys = m.group(1), [k.strip() for k in m.group(2).split(",") if k.strip()]
            return "\\mbox{%s}" % self.ref_text(cmd, keys)

        return re.sub(r"\\(cref|Cref|autoref|ref|eqref)\s*\{([^}]*)\}", repl, text)

    def resolve_refs_in_math(self, text: str) -> str:
        return self.resolve_refs_anywhere(text)

    # -- image rendering ---------------------------------------------------

    def build_preamble(self) -> str:
        pre = self.src.preamble
        pre = re.sub(r"\\documentclass\s*(\[[^\]]*\])?\s*\{[^}]*\}", "", pre, count=1)
        head = "\\documentclass[12pt,a4paper]{article}\n"
        tail = "\n\\usepackage[active,tightpage]{preview}\n\\pagestyle{empty}\n"
        return head + pre + tail

    def render_jobs(self, workdir: Path, cache: Path | None) -> None:
        if not self.jobs:
            return
        batches: dict[str, list[Job]] = {"tex": [], "pdf": [], "raster": []}
        for job in self.jobs:
            if job.kind in ("raster",):
                batches["raster"].append(job)
            elif job.kind == "pdfimage":
                batches["pdf"].append(job)
            else:
                batches["tex"].append(job)
        if batches["tex"]:
            self.render_tex_batch(batches["tex"], workdir, cache)
        for job in batches["pdf"]:
            self.render_pdf_image(job, workdir)
        for job in batches["raster"]:
            self.render_raster(job, workdir)

    def job_tex(self, job: Job) -> str:
        if job.kind == "inline":
            return ("\\begin{preview}\\setbox0=\\hbox{\\(%s\\)}%%\n"
                    "\\typeout{VTUDEPTH %d \\the\\dp0}%%\n\\box0\\end{preview}"
                    % (job.source, job.jid))
        if job.kind == "display":
            return "\\begin{preview}\\setbox0=\\hbox{$\\displaystyle %s$}\\box0\\end{preview}" % job.source
        if job.kind == "listing":
            return ("\\begin{preview}\\begin{minipage}{%s cm}\\begin{%s}\n%s\n\\end{%s}"
                    "\\end{minipage}\\end{preview}"
                    % (LISTING_WIDTH_CM, job.env, job.source, job.env))
        if job.kind == "table":
            return "\\begin{preview}%s\\end{preview}" % job.source
        if job.kind == "note":
            return ("\\begin{preview}\\begin{minipage}{%s cm}\n%s\n\\end{minipage}\\end{preview}"
                    % (round(TEXT_WIDTH_PT / 72 * 2.54, 3), job.source))
        raise ValueError(f"unknown job kind {job.kind}")

    def render_tex_batch(self, jobs: list[Job], workdir: Path, cache: Path | None) -> None:
        pending: list[Job] = []
        for job in jobs:
            cached = self.load_cached(job, cache)
            if cached:
                self.images[job.jid] = cached
            else:
                pending.append(job)
        if not pending:
            return

        preamble = self.build_preamble()
        doc = [preamble, "\\begin{document}"]
        for job in pending:
            doc.append("%% vtu job %d" % job.jid)
            doc.append(self.job_tex(job))
        doc.append("\\end{document}\n")
        tex_path = workdir / "images.tex"
        tex_path.write_text("\n".join(doc), encoding="utf-8")

        log_path = workdir / "images.log"
        cmd = ["lualatex", "-interaction=nonstopmode", "-halt-on-error", "-file-line-error", "images.tex"]
        for _ in range(2):
            proc = subprocess.run(cmd, cwd=workdir, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            if proc.returncode != 0:
                log = log_path.read_text(encoding="utf-8", errors="replace") if log_path.exists() else proc.stdout.decode("utf-8", "replace")
                tail = "\n".join(log.splitlines()[-60:])
                raise RuntimeError(f"lualatex failed while rendering images:\n{tail}")
        pdf_path = workdir / "images.pdf"

        info = subprocess.run(["pdfinfo", str(pdf_path)], stdout=subprocess.PIPE, check=True)
        pages = int(re.search(r"^Pages:\s+(\d+)", info.stdout.decode(), re.M).group(1))
        if pages != len(pending):
            raise RuntimeError(f"expected {len(pending)} rendered pages, got {pages}")

        log_text = log_path.read_text(encoding="utf-8", errors="replace") if log_path.exists() else ""
        depths: dict[int, float] = {}
        for m in re.finditer(r"^VTUDEPTH (\d+) (-?\d+(?:\.\d+)?)pt", log_text, re.M):
            depths[int(m.group(1))] = float(m.group(2))

        for page, job in enumerate(pending, start=1):
            png_prefix = workdir / f"page-{page}"
            png_path = workdir / f"page-{page}.png"
            subprocess.run(["pdftocairo", "-png", "-r", str(PNG_DPI), "-singlefile", "-f", str(page), "-l", str(page),
                            str(pdf_path), str(png_prefix)], check=True)
            png = png_path.read_bytes()
            px_w, px_h = png_pixels(png)
            if not px_w or not px_h:
                raise RuntimeError(f"cannot determine page {page} size")
            w, h = px_w * 72.0 / PNG_DPI, px_h * 72.0 / PNG_DPI
            depth = depths.get(job.jid, 0.0) if job.kind == "inline" else 0.0
            rendered = Rendered(png=png, width_pt=w, height_pt=h, depth_pt=depth)
            self.images[job.jid] = rendered
            self.save_cached(job, rendered, cache)

    def cache_key(self, job: Job) -> str:
        payload = json.dumps({"kind": job.kind, "src": job.source, "env": job.env, "w": job.width_pt,
                              "dpi": PNG_DPI,
                              "pre": hashlib.sha256(self.build_preamble().encode()).hexdigest()})
        return hashlib.sha256(payload.encode()).hexdigest()

    def load_cached(self, job: Job, cache: Path | None) -> Rendered | None:
        if cache is None:
            return None
        d = cache / self.cache_key(job)
        meta = d / "meta.json"
        if not meta.exists():
            return None
        try:
            data = json.loads(meta.read_text())
            return Rendered(png=(d / "img.png").read_bytes(),
                            width_pt=data["w"], height_pt=data["h"], depth_pt=data["d"])
        except Exception:
            return None

    def save_cached(self, job: Job, rendered: Rendered, cache: Path | None) -> None:
        if cache is None:
            return
        d = cache / self.cache_key(job)
        d.mkdir(parents=True, exist_ok=True)
        (d / "img.png").write_bytes(rendered.png)
        (d / "meta.json").write_text(json.dumps({"w": rendered.width_pt, "h": rendered.height_pt, "d": rendered.depth_pt}))

    def render_pdf_image(self, job: Job, workdir: Path) -> None:
        path = Path(job.extra[0])  # type: ignore[attr-defined]
        base = workdir / f"pdf-{job.jid}"
        info = subprocess.run(["pdfinfo", str(path)], stdout=subprocess.PIPE, check=True).stdout.decode()
        size = re.search(r"^Page size:\s+([0-9.]+) x ([0-9.]+) pts", info, re.M)
        box = re.search(r"^Page size:\s+([0-9.]+) x ([0-9.]+) pts", info, re.M)
        if not size and not box:
            raise RuntimeError(f"cannot read pdf size of {path}")
        w, h = float(size.group(1)), float(size.group(2))
        subprocess.run(["pdftocairo", "-png", "-r", str(PNG_DPI), "-singlefile", "-f", "1", "-l", "1",
                        str(path), str(base)], check=True)
        self.images[job.jid] = Rendered(png=Path(str(base) + ".png").read_bytes(),
                                        width_pt=w, height_pt=h, depth_pt=0.0)

    def render_raster(self, job: Job, workdir: Path) -> None:
        data, w, h = job.extra  # type: ignore[attr-defined]
        self.images[job.jid] = Rendered(png=data, width_pt=w, height_pt=h, depth_pt=0.0)

    # -- docx --------------------------------------------------------------

    def write_docx(self, out: Path) -> None:
        writer = DocxWriter(self.template, self)
        writer.save(out)


# --------------------------------------------------------------------------
# small text utilities
# --------------------------------------------------------------------------


def detex_inline_text(s: str) -> str:
    s = s.replace("\\$", "\x00DOLLAR\x00")
    s = re.sub(r"\\([&%#_{}$])", r"\1", s)
    s = re.sub(r"\\[A-Za-z@]+\*?\s*", "", s)
    s = re.sub(r"\\[ ,;:!]", " ", s)
    s = s.replace("~", " ").replace("---", "\u2014").replace("--", "\u2013")
    s = s.replace("``", "\u201c").replace("''", "\u201d")
    s = s.replace("$", "")
    s = s.replace("{", "").replace("}", "")
    s = s.replace("\x00DOLLAR\x00", "$")
    return re.sub(r"\s+", " ", s).strip()


def strip_latex_labels(text: str) -> str:
    return re.sub(r"\\label\s*\{[^}]*\}", "", text)


def extract_footnotes(text: str, start_id: int = 1) -> tuple[str, list[str]]:
    notes: list[str] = []
    out: list[str] = []
    i = 0
    while i < len(text):
        m = re.match(r"\\footnote\s*\{", text[i:])
        if m:
            group, j = read_group(text, i + m.end() - 1)
            notes.append(group)
            i = j
            out.append("\\textsuperscript{%d}" % (start_id + len(notes) - 1))
            continue
        out.append(text[i])
        i += 1
    return "".join(out), notes


def split_items(raw: str) -> list[str]:
    items: list[str] = []
    current: list[str] = []
    depth = 0
    env_depth = 0
    i = 0
    while i < len(raw):
        if raw[i] == "\\":
            m = re.match(r"\\begin\{([^}]*)\}", raw[i:])
            if m:
                env_depth += 1
                current.append(raw[i:i + m.end()])
                i += m.end()
                continue
            m = re.match(r"\\end\{([^}]*)\}", raw[i:])
            if m:
                env_depth -= 1
                current.append(raw[i:i + m.end()])
                i += m.end()
                continue
            m = re.match(r"\\item\b", raw[i:])
            if m and depth == 0 and env_depth == 0:
                items.append("".join(current))
                current = []
                i += m.end()
                continue
            name, j = read_cmd(raw, i)
            current.append(raw[i:j])
            i = j
            continue
        c = raw[i]
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
        current.append(c)
        i += 1
    items.append("".join(current))
    return [x.strip() for x in items if x.strip()]


def png_pixels(data: bytes) -> tuple[int, int]:
    if data[:8] != b"\x89PNG\r\n\x1a\n":
        return 0, 0
    return int.from_bytes(data[16:20], "big"), int.from_bytes(data[20:24], "big")


def png_size(data: bytes) -> tuple[float, float]:
    w, h = png_pixels(data)
    return w * 72.0 / 96.0, h * 72.0 / 96.0


# --------------------------------------------------------------------------
# OOXML writing
# --------------------------------------------------------------------------


def transparent_png() -> bytes:
    import struct
    import zlib

    def chunk(tag: bytes, data: bytes) -> bytes:
        return struct.pack(">I", len(data)) + tag + data + struct.pack(">I", zlib.crc32(tag + data) & 0xFFFFFFFF)

    ihdr = struct.pack(">IIBBBBB", 1, 1, 8, 6, 0, 0, 0)
    return (b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", ihdr)
            + chunk(b"IDAT", zlib.compress(b"\x00\x00\x00\x00\x00"))
            + chunk(b"IEND", b""))


def w_rpr(style: dict) -> str:
    parts = []
    if style.get("mono"):
        font = "Courier New"
    elif style.get("sans"):
        font = "Arial"
    else:
        font = "Times New Roman"
    parts.append(f'<w:rFonts w:ascii="{font}" w:hAnsi="{font}" w:cs="{font}"/>')
    if style.get("bold"):
        parts.append("<w:b/><w:bCs/>")
    if style.get("italic"):
        parts.append("<w:i/><w:iCs/>")
    if style.get("smallcaps"):
        parts.append("<w:smallCaps/>")
    if style.get("color"):
        parts.append(f'<w:color w:val="{style["color"]}"/>')
    if style.get("size"):
        parts.append(f'<w:sz w:val="{style["size"]}"/><w:szCs w:val="{style["size"]}"/>')
    if style.get("sup"):
        parts.append('<w:vertAlign w:val="superscript"/>')
    if style.get("sub"):
        parts.append('<w:vertAlign w:val="subscript"/>')
    return "<w:rPr>" + "".join(parts) + "</w:rPr>"


def w_text_run(text: str, style: dict) -> str:
    return f'<w:r>{w_rpr(style)}<w:t xml:space="preserve">{xml_escape(text)}</w:t></w:r>'


class DocxWriter:
    def __init__(self, template: Path, conv: Converter):
        self.conv = conv
        with zipfile.ZipFile(template) as z:
            self.parts = {name: z.read(name) for name in z.namelist()}
        self.new_media: list[tuple[str, bytes]] = []
        self.new_rels: list[tuple[str, str, str]] = []
        self.next_rid = 100
        self.next_docpr = 1
        self.spacer_rid: str | None = None
        self.body: list[str] = []

    def add_image(self, png: bytes) -> tuple[str, int]:
        self.conv.media_index += 1
        idx = self.conv.media_index
        name = f"media/vtuimg{idx:03d}.png"
        self.new_media.append((name, png))
        rid = f"rId{self.next_rid}"
        self.next_rid += 1
        self.new_rels.append((rid, "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image", name))
        docpr = self.next_docpr
        self.next_docpr += 1
        return rid, docpr

    def ensure_spacer(self) -> str:
        if not self.spacer_rid:
            rid, _ = self.add_image(transparent_png())
            self.spacer_rid = rid
        return self.spacer_rid

    @staticmethod
    def pic_xml(rid: str, cx: int, cy: int, docpr: int, name: str) -> str:
        return f'''<a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">
<a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">
<pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
<pic:nvPicPr><pic:cNvPr id="{docpr}" name="{name}{docpr}"/><pic:cNvPicPr/></pic:nvPicPr>
<pic:blipFill><a:blip r:embed="{rid}"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>
<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="{cx}" cy="{cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>
</pic:pic></a:graphicData></a:graphic>'''

    def inline_drawing_run(self, rid: str, docpr: int, cx: int, cy: int, name: str = "Picture") -> str:
        return f'''<w:r><w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0">
<wp:extent cx="{cx}" cy="{cy}"/>
<wp:docPr id="{docpr}" name="{name} {docpr}"/>
<wp:cNvGraphicFramePr><a:graphicFrameLocks xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" noChangeAspect="1"/></wp:cNvGraphicFramePr>
{self.pic_xml(rid, cx, cy, docpr, name)}
</wp:inline></w:drawing></w:r>'''

    def anchored_drawing_run(self, rid: str, docpr: int, cx: int, cy: int, off_y: int) -> str:
        return f'''<w:r><w:drawing><wp:anchor distT="0" distB="0" distL="0" distR="0" simplePos="0" relativeHeight="251658240" behindDoc="0" locked="0" layoutInCell="1" allowOverlap="1">
<wp:simplePos x="0" y="0"/>
<wp:positionH relativeFrom="character"><wp:posOffset>0</wp:posOffset></wp:positionH>
<wp:positionV relativeFrom="line"><wp:posOffset>{off_y}</wp:posOffset></wp:positionV>
<wp:extent cx="{cx}" cy="{cy}"/>
<wp:effectExtent l="0" t="0" r="0" b="0"/>
<wp:wrapNone/>
<wp:docPr id="{docpr}" name="Math {docpr}"/>
<wp:cNvGraphicFramePr><a:graphicFrameLocks xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" noChangeAspect="1"/></wp:cNvGraphicFramePr>
{self.pic_xml(rid, cx, cy, docpr, "math")}
</wp:anchor></w:drawing></w:r>'''

    def drawing_run(self, jid: int, max_width_pt: float | None = None) -> str:
        rendered = self.conv.images[jid]
        rid, docpr = self.add_image(rendered.png)
        w, h = rendered.width_pt, rendered.height_pt
        if max_width_pt and w > max_width_pt:
            h = h * max_width_pt / w
            w = max_width_pt
        return self.inline_drawing_run(rid, docpr, int(round(w * EMU_PER_PT)), int(round(h * EMU_PER_PT)))

    def math_runs(self, jid: int, hline_pt: float) -> str:
        rendered = self.conv.images[jid]
        rid, docpr = self.add_image(rendered.png)
        w, h, d = rendered.width_pt, rendered.height_pt, rendered.depth_pt
        cx, cy = int(round(w * EMU_PER_PT)), int(round(h * EMU_PER_PT))
        off_y = int(round((hline_pt - (h - d)) * EMU_PER_PT))
        spacer = self.ensure_spacer()
        sdocpr = self.next_docpr
        self.next_docpr += 1
        sx = int(round(w * EMU_PER_PT))
        sy = int(round(hline_pt * EMU_PER_PT))
        anchor = self.anchored_drawing_run(rid, docpr, cx, cy, off_y)
        space = self.inline_drawing_run(spacer, sdocpr, sx, sy, "Spacer")
        return anchor + space

    def runs_xml(self, runs: list[tuple], default_style: dict | None = None, hline_pt: float | None = None) -> str:
        default_style = default_style or {}
        out = []
        for r in runs:
            kind = r[0]
            if kind == "text":
                out.append(w_text_run(r[1], r[2]))
            elif kind == "br":
                out.append(f"<w:r>{w_rpr(default_style)}<w:br/></w:r>")
            elif kind == "math":
                if hline_pt is not None:
                    out.append(self.math_runs(r[1], hline_pt))
                else:
                    out.append(self.drawing_run(r[1], max_width_pt=TEXT_WIDTH_PT))
            elif kind == "cite":
                out.append(w_text_run(self.conv.cite_text(r[1]), default_style))
            elif kind == "ref":
                out.append(w_text_run(self.conv.ref_text(r[1], r[2]), default_style))
            elif kind == "fn":
                out.append(f'<w:r><w:rPr><w:vertAlign w:val="superscript"/></w:rPr><w:footnoteReference w:id="{r[1]}"/></w:r>')
            elif kind == "img":
                out.append(self.drawing_run(r[1], max_width_pt=TEXT_WIDTH_PT))
        return "".join(out)

    @staticmethod
    def ppr(style: dict | None = None, first_line: int = 567, jc: str = "both", keep: bool = False,
            outline: int | None = None, tabs: str = "", left: int | None = None, hanging: int | None = None,
            no_indent: bool = False) -> str:
        style = style or {}
        parts = []
        if keep:
            parts.append("<w:keepNext/>")
        if tabs:
            parts.append(tabs)
        parts.append('<w:spacing w:before="0" w:after="0" w:line="240" w:lineRule="auto"/>')
        if no_indent:
            parts.append('<w:ind w:firstLine="0"/>')
        elif hanging:
            parts.append(f'<w:ind w:left="{left or 0}" w:hanging="{hanging}"/>')
        else:
            parts.append(f'<w:ind w:firstLine="{first_line}"/>')
        parts.append(f'<w:jc w:val="{jc}"/>')
        if outline is not None:
            parts.append(f'<w:outlineLvl w:val="{outline}"/>')
        if style:
            parts.append(w_rpr(style))
        return "<w:pPr>" + "".join(parts) + "</w:pPr>"

    def inline_line_height(self, runs: list[tuple]) -> float | None:
        heights = []
        for r in runs:
            if r[0] == "math" and self.conv.jobs[r[1]].kind == "inline":
                heights.append(self.conv.images[r[1]].height_pt)
        if not heights:
            return None
        return max(14.0, max(heights))

    def paragraph(self, runs: list[tuple], style: dict | None = None, **ppr_kw) -> str:
        style = style or {}
        hline = self.inline_line_height(runs)
        return "<w:p>" + self.ppr(style, **ppr_kw) + self.runs_xml(runs, style, hline) + "</w:p>"

    def build_body(self) -> str:
        g = self.conv
        for e in g.elems:
            t = e["type"]
            if t == "title":
                self.body.append(self.paragraph(e["runs"], {"size": 28, "bold": True}, first_line=0, jc="center"))
            elif t == "authors":
                self.body.append(self.paragraph(e["runs"], {}, first_line=0, jc="center"))
            elif t == "para":
                size = e.get("size")
                style = {"size": size} if size else {}
                runs = e["runs"]
                self.body.append(self.paragraph(runs, style))
            elif t == "heading":
                level = e["level"]
                style = {"bold": True}
                runs = []
                for r in e["runs"]:
                    if r[0] == "text":
                        runs.append(("text", r[1], merge_style(r[2], bold=True)))
                    else:
                        runs.append(r)
                self.body.append(self.paragraph(runs, style, keep=True, outline=level - 1))
            elif t == "bullet":
                level = e["level"]
                left = 567 * (level + 1)
                self.body.append(self.paragraph(e["runs"], {}, no_indent=False, left=left, hanging=283))
            elif t == "equation":
                tabs = ('<w:tabs><w:tab w:val="center" w:pos="%d"/><w:tab w:val="right" w:pos="%d"/></w:tabs>'
                        % (TEXT_WIDTH_TWIPS // 2, TEXT_WIDTH_TWIPS))
                ppr = self.ppr({}, first_line=0, tabs=tabs)
                xml = "<w:p>" + ppr + '<w:r><w:tab/></w:r>'
                xml += self.drawing_run(e["jid"], max_width_pt=TEXT_WIDTH_PT - 45)
                xml += '<w:r><w:tab/></w:r>'
                if e.get("num"):
                    xml += w_text_run(f"({e['num']})", {})
                xml += "</w:p>"
                self.body.append(xml)
            elif t == "table":
                caption_runs = list(e["caption"]) + list(e.get("fn", []))
                self.body.append(self.paragraph(caption_runs, {}, first_line=0, jc="right", keep=True))
                self.body.append(self.paragraph([("img", e["jid"])], {}, first_line=0, jc="center"))
            elif t == "listing":
                self.body.append(self.paragraph([("img", e["jid"])], {}, first_line=0, jc="center", keep=True))
                if e["caption"]:
                    self.body.append(self.paragraph(e["caption"], {}, first_line=0, jc="center"))
            elif t == "note":
                self.body.append(self.paragraph([("img", e["jid"])], {}, first_line=0, jc="center"))
            elif t == "figure":
                self.body.append(self.paragraph([("img", e["jid"])], {}, first_line=0, jc="center", keep=True))
                if e["caption"]:
                    self.body.append(self.paragraph(e["caption"], {}, first_line=0, jc="center"))
            elif t == "reference":
                runs = [("text", f"[{e['num']}] ", {}), ("text", e["raw"], {})]
                self.body.append(self.paragraph(runs, {}, no_indent=False, left=567, hanging=567))
        return "".join(self.body)

    def save(self, out: Path) -> None:
        body = self.build_body()
        footnotes = self.footnotes_xml()
        doc = self.parts["word/document.xml"].decode("utf-8")
        start = doc.find("<w:body>") + len("<w:body>")
        sect = doc.rfind("<w:sectPr")
        if sect == -1:
            raise RuntimeError("template document has no sectPr")
        end = doc.find("</w:body>", sect)
        sect_xml = doc[sect:end]
        new_doc = doc[:start] + body + sect_xml + doc[end:]

        self.parts["word/document.xml"] = new_doc.encode("utf-8")

        rels = self.parts["word/_rels/document.xml.rels"].decode("utf-8")
        extra = "".join(f'<Relationship Id="{rid}" Type="{typ}" Target="{target}"/>' for rid, typ, target in self.new_rels)
        rels = rels.replace("</Relationships>", extra + "</Relationships>")
        self.parts["word/_rels/document.xml.rels"] = rels.encode("utf-8")

        types = self.parts["[Content_Types].xml"].decode("utf-8")
        if 'Extension="png"' not in types:
            types = types.replace("</Types>", '<Default Extension="png" ContentType="image/png"/></Types>')
        self.parts["[Content_Types].xml"] = types.encode("utf-8")

        self.parts["word/footnotes.xml"] = footnotes.encode("utf-8")

        header = self.parts["word/header1.xml"].decode("utf-8")
        year = self.conv.src.year
        if year:
            pat = re.compile(r"(<w:t[^>]*>)([^<]*20)(</w:t>)(.*?)(<w:t[^>]*>)2(</w:t>)(.*?)(<w:t[^>]*>)4(</w:t>)", re.S)
            header, subs = pat.subn(
                lambda m: m.group(1) + m.group(2) + m.group(3) + m.group(4)
                + m.group(5) + year[2] + m.group(6) + m.group(7)
                + m.group(8) + year[3] + m.group(9), header)
            if subs == 0:
                self.conv.warn_once("could not update the year in the running header")
        self.parts["word/header1.xml"] = header.encode("utf-8")

        core = self.parts["docProps/core.xml"].decode("utf-8")
        title_text = ""
        for e in self.conv.elems:
            if e["type"] == "title":
                title_text = "".join(r[1] for r in e["runs"] if r[0] == "text")
                break
        core = re.sub(r"<dc:title>.*?</dc:title>", "<dc:title>" + xml_escape(title_text) + "</dc:title>", core, flags=re.S)
        core = re.sub(r"<dc:creator>.*?</dc:creator>", "<dc:creator></dc:creator>", core, flags=re.S)
        core = re.sub(r"<cp:lastModifiedBy>.*?</cp:lastModifiedBy>", "<cp:lastModifiedBy></cp:lastModifiedBy>", core, flags=re.S)
        self.parts["docProps/core.xml"] = core.encode("utf-8")

        for name, data in self.new_media:
            self.parts["word/" + name] = data

        out.parent.mkdir(parents=True, exist_ok=True)
        with zipfile.ZipFile(out, "w", zipfile.ZIP_DEFLATED) as z:
            for name, data in self.parts.items():
                z.writestr(name, data)

    def footnotes_xml(self) -> str:
        header = '''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:footnotes xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
<w:footnote w:type="separator" w:id="-1"><w:p><w:pPr><w:spacing w:after="0" w:line="240" w:lineRule="auto"/></w:pPr><w:r><w:separator/></w:r></w:p></w:footnote>
<w:footnote w:type="continuationSeparator" w:id="0"><w:p><w:pPr><w:spacing w:after="0" w:line="240" w:lineRule="auto"/></w:pPr><w:r><w:continuationSeparator/></w:r></w:p></w:footnote>'''
        parts = [header]
        for fn in self.conv.footnotes:
            runs = '<w:r><w:rPr><w:vertAlign w:val="superscript"/><w:sz w:val="20"/></w:rPr><w:footnoteRef/></w:r>'
            runs += '<w:r><w:rPr><w:sz w:val="20"/></w:rPr><w:t xml:space="preserve"> </w:t></w:r>'
            runs += self.runs_xml(fn["runs"], {"size": 20})
            parts.append(f'<w:footnote w:id="{fn["id"]}"><w:p><w:pPr><w:spacing w:after="0" w:line="240" w:lineRule="auto"/>'
                         f'<w:rPr><w:sz w:val="20"/></w:rPr></w:pPr>{runs}</w:p></w:footnote>')
        parts.append("</w:footnotes>")
        return "".join(parts)


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------


def verify_docx(path: Path) -> None:
    import xml.etree.ElementTree as ET

    z = zipfile.ZipFile(path)
    names = set(z.namelist())
    for name in names:
        if name.endswith(".xml") or name.endswith(".rels"):
            ET.fromstring(z.read(name))
    doc = z.read("word/document.xml").decode("utf-8")
    rels = z.read("word/_rels/document.xml.rels").decode("utf-8")
    rel_map = dict(re.findall(r'Id="([^"]+)"[^>]*Target="([^"]+)"', rels))
    for rid in set(re.findall(r'r:(?:embed|id)="([^"]+)"', doc)):
        target = rel_map.get(rid)
        if target is None:
            raise RuntimeError(f"missing relationship {rid}")
        if target.startswith("http"):
            continue
        if "word/" + target not in names:
            raise RuntimeError(f"relationship {rid} points to missing part {target}")
    if "<w:sectPr" not in doc:
        raise RuntimeError("result has no sectPr")


def main(argv: list[str]) -> int:
    ap = argparse.ArgumentParser(description="Convert the vtu-lite LaTeX paper to the MCSE docx template.")
    ap.add_argument("--paper", default="main.tex", help="main .tex file")
    ap.add_argument("--search", action="append", default=[], help="extra directory to resolve \\input and .bib files")
    ap.add_argument("--common", default=None, help="directory holding shared macros/notation/refs (added to --search)")
    ap.add_argument("--template", default=str(Path(__file__).with_name("template") / "sample-mcse-eng.docx"))
    ap.add_argument("--out", default="vtu-lite.docx")
    ap.add_argument("--cache", default=None, help="directory for cached rendered images")
    ap.add_argument("--no-blind", action="store_true", help="keep author names and the About the Author section")
    ap.add_argument("--drop-notes", action="store_true", help="omit editorial note boxes")
    ap.add_argument("--keep-temp", action="store_true")
    ap.add_argument("--dry-run", action="store_true", help="parse only, do not render images or write a docx")
    ap.add_argument("--quiet", action="store_true")
    args = ap.parse_args(argv)

    paper = Path(args.paper).resolve()
    search = [Path(p).resolve() for p in args.search]
    if args.common:
        search.insert(0, Path(args.common).resolve())

    src = Source(paper, search, blind_override=False if args.no_blind else None)
    conv = Converter(src, Path(args.template), args.drop_notes)
    conv.parse()
    conv.load_bibliography()
    conv.emit()
    conv.emit_references_if_needed()

    if args.dry_run:
        from collections import Counter
        counts = Counter(e["type"] for e in conv.elems)
        print(json.dumps({"blocks": len(conv.blocks), "jobs": len(conv.jobs),
                          "elements": counts, "footnotes": len(conv.footnotes),
                          "references": len(conv.cited), "warnings": conv.warnings}, indent=2))
        return 0

    workdir = Path(tempfile.mkdtemp(prefix="vtu2docx-"))
    try:
        cache = Path(args.cache).resolve() if args.cache else None
        if cache:
            cache.mkdir(parents=True, exist_ok=True)
        conv.render_jobs(workdir, cache)
        out = Path(args.out)
        conv.write_docx(out)
        verify_docx(out)
    finally:
        if args.keep_temp:
            print(f"kept temporary directory {workdir}", file=sys.stderr)
        else:
            shutil.rmtree(workdir, ignore_errors=True)

    if not args.quiet:
        print(f"wrote {out} ({out.stat().st_size} bytes, {len(conv.jobs)} images, {len(conv.footnotes)} footnotes)")
        for w in conv.warnings:
            print(f"warning: {w}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv[1:]))
    except Exception as exc:  # pragma: no cover - CLI error reporting
        print(f"error: {exc}", file=sys.stderr)
        sys.exit(1)
