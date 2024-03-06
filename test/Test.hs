module Main (main) where

import           Control.Monad        (void)
import           Test.EndToEnd (genEndToEndTests)
import           Test.HUnit           (Test (TestList), runTestTT)
-- import           Test.SemanticCheck.SemanticCheck (semanticCheckTests)

main :: IO ()
main = do
  endToEndTests <- genEndToEndTests
  void $ runTestTT $ TestList
    [ endToEndTests
    -- , semanticCheckTests
    ]
