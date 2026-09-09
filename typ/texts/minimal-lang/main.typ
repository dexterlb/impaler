#set document(
  title: [A minimal LISP-like language]
)

#import "/lib/paper.typ": paper-template
#show: paper-template

#import "/lib/ild-stuff.typ": ild-template
#show: ild-template

#set heading(numbering: "1.")

#title()

#include "/common/ild-motivation.typ"
#include "/common/ild-lang.typ"
#include "/common/bootstrapping-basic.typ"

= Appendix

#include "/common/poly-fix-y.typ"

#bibliography("/common/refs.bib")
