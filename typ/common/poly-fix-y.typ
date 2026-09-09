#import "/lib/ild-stuff.typ": ildsym

== Polyvariate Y-combinator <poly-fix-Y>
Instead of relying on a host implementation of $ildsym("poly-fix")$, we can
quite elegantly define it as such:

```ild
(!lambda l
  ((!lambda (x) (x x))
    (!lambda (p)
      (map (!lambda (li) (!lambda args (apply (apply li (p p)) args))) l)))))))
```
