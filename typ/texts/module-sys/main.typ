#set document(
  title: [Implementing a module system in a minimal LISP-like language]
)

#import "/lib/paper.typ": paper-template
#show: paper-template

#import "/lib/ild-stuff.typ": ild-template, ild
#show: ild-template

#import "/lib/misc.typ": note

#title()

#include "/common/ild-motivation.typ"
#include "/common/ild-lang.typ"
#include "/common/host-env.typ"
#include "/common/bootstrapping-basic.typ"
#include "/texts/module-sys/bootstrapping-module-loader.typ"
#include "/common/meat.typ"

= Appendix

#include "/common/poly-fix-y.typ"
#include "/common/continuation-monad.typ"

#bibliography("/common/refs.bib")
