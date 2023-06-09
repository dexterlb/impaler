import Sandbox
import Testing

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Utils.Parsing (parseFile)

main :: IO ()
main = do
    let dir = "./test/expr-tests"
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
        (runExprTest sampleEnv test) `shouldBe` TestOK
