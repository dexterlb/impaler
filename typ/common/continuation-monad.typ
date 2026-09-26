#import "/lib/ild-stuff.typ": ildmono, contmonad, ret, bind, bindop, mdo
#import "/lib/misc.typ": note, optref

== Continuations <continuation-monad>
To model sequential execution in our CPS semantics, we use a continuation monad
#cite(<wadler>, supplement: [Section 3]):

$contmonad(A, W)$ is the set of computations of type $(W -> A) -> A$, where A
is a set of "answers".

The unit computation is:
$ ret(x) = lambda c (c x) $
The bind operation is defined as:
$ (bindop) : contmonad(A, W) -> (W -> contmonad(A, U)) -> contmonad(A, U) $
$ (phi bindop f) = lambda c (phi (lambda x (f x c))) $

Throughout this paper we use the standard monadic $ildmono("do")$-notation as
sugar for $bindop$:
$ mdo(bind(x_1, m_1), bind(x_2, m_2), ..., bind(x_n, m_n), e) $
stands for the nested binds:
$ m_1 bindop (lambda x_1 (m_2 bindop (lambda x_2 (dots.h m_n bindop (lambda x_n (e)) dots.h)))). $

When the answer set is not relevant, we write $contmonad(V)$ instead of $contmonad(A, V)$.

#note[
The do-notation cases handwave a big-step in each bind, this should probably be explained
]

The continuation monad lets us reason about first-class continuations#optref(<first-class-continuations>) and side effects#optref(<side-effects>).
