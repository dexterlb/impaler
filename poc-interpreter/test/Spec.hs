import Sandbox
import Testing

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory)
import Test.Hspec

main :: IO ()
main = do
    testFiles <- listDirectory "."
    groups <- loadExprTestGroups testFiles
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
    tests <- loadExprTests path
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
