#set document(
  title: [Implementing a module system in a minimal LISP-like language]
)

#import "/lib/paper.typ": paper-template
#show: paper-template

#import "/lib/ild-stuff.typ": ild-template
#show: ild-template

#import "/lib/misc.typ": note

#title()

#include "/common/ild-motivation.typ"
#include "/common/ild-lang.typ"
#include "/common/bootstrapping-basic.typ"

= Bootstrapping the module loader

#note[this section is unfinished]

== An example program
#note[this section is unfinished]

```ild
(module
  (doc "this module calculates the factorial of 5")
  (exports main)
  (imports
    (builtin (macroexpand <= * + lambda))
    ("core/prelude.ild" (if))
    ("core/module-utils.ild" (fn)))
  (defs
    (!fn main () (fact 5))

    (!fn fact (x)
      (!if (<= x 0)
        1
        (* x (fact (+ x -1)))))))
```

== Complex control flow on top of CPS

#note[this section is unfinished]

The following example illustrates early return from the recursive computation
enacted by `map`:
```ild
(!fn try-map (f l)
  (call/cc (!lambda (return)
    (return (map
      (!lambda (x)
        (!if (fail? (f x))
          (return (make-fail (list 'fail-in-element x (f x))))
          (f x))) l)))))
```
If `f` returns failure for an item in the list, the subsequent items will not
be processed.

This technique can also be used to implement mechanisms like scoped try/catch,
iterative loops and other constructs that are separate features in other
languages.

== Side effects <side-effects>

#note[this section is unfinished]

= Appendix

#include "/common/poly-fix-y.typ"

#bibliography("/common/refs.bib")
