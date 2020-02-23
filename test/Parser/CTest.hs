module Parser.CTest where
import           BenchUtils
import           Control.Monad (unless)
import           Data.Either   (fromLeft, isRight)
import           Parser.C
import           Utils

cParserTests :: BenchTest
cParserTests = benchTestGroup "C parser" [testAdd]

testParse :: String -> BenchTest
testParse name = benchTestCase name $ do
  c <- parseC name
  unless (isRight c) $ error "Parse failure"

testAdd :: BenchTest
testAdd = testParse "test/Code/C/add.c"

