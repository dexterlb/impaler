# poc-interpreter

The interpreter can currently evaluate all ILD expressions, albeit very slowly.

Partial evaluation is still not supported.

To run it, you can use the nix flake (`nix run '.#poc-interpreter'`). It should
also be possible to run/build via cabal, but this is currently untested.

Evaluate a simple expression:
```
$ nix run '.#poc-interpreter' -- --expr '(add 3 4)'
```

Evaluate the module-loader and use it to load a module that calculates the factorial of 5:
```
$ nix run '.#poc-interpreter' -- --rootDir code --expr '((eval (get-env) (read-source "core/bootstrap/module_loader.l")) "demos/fact.l" (quote main))'
```
