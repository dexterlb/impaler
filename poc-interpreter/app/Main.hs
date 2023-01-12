module Main (main) where

import Utils.Parsing (ps)
import AST (AST)

sampleAST :: AST
sampleAST = ps $ "(foo bar (baz qux))"

main :: IO ()
main = putStrLn $ show sampleAST
