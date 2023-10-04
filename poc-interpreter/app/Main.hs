module Main (main) where

import Sandbox

main :: IO ()
main = do
    putStrLn "I am now calculating the factorial of 5, which may take between 4 and 40 minutes"
    demo "demos/fact.l"
