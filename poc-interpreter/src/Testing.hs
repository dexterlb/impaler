{-# OPTIONS_GHC -Wno-partial-fields #-}

module Testing
( loadExprTests
, runExprTest
, ExprTest(..)
, TestResult(..)
)
where

import Data.Text (Text)

import Values
import AST
import Evaluator

loadExprTests :: FilePath -> IO [ExprTest]
loadExprTests = error "not implemented"

data ExprTest = ExprTest
    { input :: AST
    , expectedOutput :: AST
    , name :: Text
    }

data TestResult v m
    = TestOK
    | TestFail
        { expectedVal :: Value v m
        , actualVal :: Value v m
        }
    | TestNoVal
        { expectedVal :: Value v m
        }
    deriving stock (Show)

instance Eq (TestResult v m) where
    TestOK == TestOK = True
    (TestFail _ _) == (TestFail _ _) = True
    (TestNoVal _) == (TestNoVal _) = True
    _ == _ = False

runExprTest :: (Computation v m) => Env v m -> ExprTest -> TestResult v m
runExprTest env (ExprTest { input, expectedOutput })
    | (Just v) <- returnedVal, weaklyEqual v expectedVal = TestOK
    | (Just v) <- returnedVal = TestFail { actualVal = v, expectedVal = expectedVal }
    | otherwise = TestNoVal { expectedVal = expectedVal }
    where
        returnedVal = resultOf $ eval env yieldResult inputVal
        inputVal = astToVal input
        expectedVal = astToVal expectedOutput
