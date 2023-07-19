{-# OPTIONS_GHC -Wno-partial-fields #-}

module Testing
( runExprTest
, ExprTests(..)
, ExprTest(..)
, TestResult(..)
)
where

import Data.Text (Text)

import Values
import AST
import Evaluator

import Utils.Parsing (Parseable, Parser, parser)
import qualified Utils.Parsing as P

newtype ExprTests = ExprTests [ExprTest]

instance Parseable ExprTests where
    parser = P.braces $ do
        _ <- P.literal "quote"
        P.braces $ do
            ExprTests <$> P.many (parser :: Parser ExprTest)

data ExprTest = ExprTest
    { input :: AST
    , expectedOutput :: AST
    , name :: Text
    }

instance Parseable ExprTest where
    parser = P.braces $ do
        name <- P.braces $ do
            _ <- P.literal "test"
            P.quotedString '"'
        input <- P.braces $ do
            _ <- P.literal "input"
            parser :: Parser AST
        output <- P.braces $ do
            _ <- P.literal "output"
            parser :: Parser AST
        pure $ ExprTest
                { input = input
                , name = name
                , expectedOutput = output
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
