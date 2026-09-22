#set document(
  title: [A minimal LISP-like language]
)

#import "/lib/paper.typ": paper-template
#show: paper-template

#import "/lib/ild-stuff.typ": ild-template
#show: ild-template

#import "/lib/misc.typ": abstract, review, paraphrase

#title()

#abstract[
  We define a minimal, homoiconic LISP-like language called ILD, designed for
  deep embedding in a host environment. In contrast to other LISPs, it is
  fully-immutable. Minimality is characterised by simple syntax based on
  S-expressions and just three special forms. Data structure
  operations are provided through the host (foreign function) interface.

  We demonstrate that the immutability and minimality constraint do not
  compromise expressiveness by showing that ILD's metaprogramming
  (facilitated by macro expansion) is powerful enough to implement
  standard LISP language features in ILD itself, without reliance on mutation.
]

#include "/common/ild-motivation.typ"
#include "/common/ild-lang.typ"
#include "/common/bootstrapping-basic.typ"

= Appendix

#include "/common/poly-fix-y.typ"

#bibliography("/common/refs.bib")
