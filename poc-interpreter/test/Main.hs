{-# LANGUAGE TemplateHaskell #-}
module Main
    ( main
    , sandboxDemo
    , exprTestMain )
where

import PseudoMacros

import Sandbox
import Testing

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath ((</>), takeDirectory)
import Test.Hspec

import Utils.Parsing (parseFile)

sandboxDemo :: IO ()
sandboxDemo = demo "demos/fact.l"

-- TODO: instead of reading the files at runtime,
-- use embedDir from here: https://hackage.haskell.org/package/file-embed-0.0.15.0/docs/Data-FileEmbed.html
-- this will make it work under nix, etc

main :: IO ()
main = sandboxDemo

exprTestMain :: IO ()
exprTestMain = do
    let dir = (takeDirectory $ $__FILE__) </> "expr-tests"
    testFiles <- listDirectory dir
    let testPaths = map (dir </>) testFiles
    groups <- loadExprTestGroups testPaths
    hspec $ do
        mapM_ specExprTestGroup groups

data ExprTestGroup = ExprTestGroup
    { name :: Text
    , tests :: [ExprTest]
    }

loadExprTestGroups :: [FilePath] -> IO [ExprTestGroup]
loadExprTestGroups = mapM loadExprTestGroup

loadExprTestGroup :: FilePath -> IO ExprTestGroup
loadExprTestGroup path = do
    (ExprTests tests) <- parseFile path
    pure $ ExprTestGroup
        { name = T.pack path
        , tests = tests
        }

specExprTestGroup :: ExprTestGroup -> Spec
specExprTestGroup (ExprTestGroup { name, tests }) = do
    describe (T.unpack name) $ mapM_ specExprTest tests

specExprTest :: ExprTest -> Spec
specExprTest test@(ExprTest{ name }) = do
    it (T.unpack name) $ do
        (runExprTest env test) `shouldBe` TestOK
    where
        env = sandboxEnvWithoutSources
