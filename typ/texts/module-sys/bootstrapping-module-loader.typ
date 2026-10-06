#import "/lib/ild-stuff.typ": ild
#import "/lib/misc.typ": note, lst

= Bootstrapping the module loader <module-loader>

#note[this section is unfinished]

== Complex control flow on top of CPS

#note[this section is unfinished]

The following example illustrates early return from the recursive computation
enacted by #ild("map"):
#lst(caption: [Early return from #ild("map")])[
```ild
(!fn try-map (f l)
  (call/cc (!lambda (return)
    (return (map
      (!lambda (x)
        (!if (fail? (f x))
          (return (make-fail (list 'fail-in-element x (f x))))
          (f x))) l)))))
```
]
If #ild("f") returns failure for an item in the list, the subsequent items will not
be processed.

This technique can also be used to implement mechanisms like scoped try/catch,
iterative loops and other constructs that are separate features in other
languages.
