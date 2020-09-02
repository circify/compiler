module Analyze.Analyze where
import           Analyze.Example
import           BenchUtils


analyzeTests :: BenchTest
analyzeTests = benchTestGroup "Analysis tests" [ analyzeTest "test/Code/C/ops.c" ]

analyzeTest :: FilePath -> BenchTest
analyzeTest path = benchTestCase "Analyze" $ do
  analyzeAST path
  return ()



