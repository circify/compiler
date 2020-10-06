{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module BenchUtils where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import qualified Control.Exception
import           Data.Proxy
import           Data.Typeable

{- | Asserts that a specific exception is raised by a given action.
 - Stolen from testpack. Thanks!
 - -}
assertRaises
  :: forall e a
   . (Show a, Control.Exception.Exception e, Show e)
  => String
  -> IO a
  -> IO ()
assertRaises msg action = do
  r <- Control.Exception.try @e action
  case r of
    Left e -> return ()
    Right _ ->
      assertFailure
        $  msg
        ++ "\nReceived no exception, but was expecting exception"
        ++ (show $ typeOf $ Proxy @e)


-- Thanks to CRAIG DISSELKOEN for this benchmarking/testing framework setup.
-- He wrote the code in this file.

data BenchTest = BenchTest {
    getTest  :: TestTree
  --, getBench :: Benchmark
}

-- Gipeda can't deal with double-quotes in names of benchmarks.
-- We replace them with single-quotes.
replaceDoubleQuotes :: String -> String
replaceDoubleQuotes = map ifDoubleThenSingle
 where
  ifDoubleThenSingle :: Char -> Char
  ifDoubleThenSingle '"' = '\''
  ifDoubleThenSingle c   = c

benchTestCase :: String -> IO () -> BenchTest
benchTestCase name act =
  BenchTest { getTest = testCase name act }
  --, getBench = bench (replaceDoubleQuotes name) $ nfIO act

benchTestProperty :: Testable a => String -> a -> BenchTest
benchTestProperty name prop =
  BenchTest { getTest = testProperty name prop }
  --, getBench = bench (replaceDoubleQuotes name) $ nfIO $ return ()

--benchGoldenVsString :: String -> FilePath -> IO ByteString -> BenchTest
--benchGoldenVsString name path act = BenchTest {
--    getTest = goldenVsString name path act
--  , getBench = bench (replaceDoubleQuotes name) $ nfIO act
--}

benchTestGroup :: String -> [BenchTest] -> BenchTest
benchTestGroup name bts =
  BenchTest { getTest = testGroup name $ map getTest bts }
  --, getBench = bgroup name $ map getBench bts
