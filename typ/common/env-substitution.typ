#import "/lib/ild-stuff.typ": ildpair, ildsetabstr
#import "/lib/misc.typ": cases

== Environment substitution <env-substitution>
Formally, parameter substitution in a binding environment $e [ x / v ]$ can be defined as:
$
  e [ x / v ] = cases(
    ildpair(ildpair(x, v), ()) & e = (),
    ildpair(ildpair(x, v), e_1) & e = ildpair(ildpair(x, w), e_1),
    ildpair(ildpair(y, w), e_1 [ x / v ]) & e = ildpair(ildpair(y, w), e_1) and y != x,
  )
$
