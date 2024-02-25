module Main (main) where

import           Control.Monad                    (void)
import           Test.HUnit                       (Test (TestList), runTestTT)
import           Test.SemanticCheck.SemanticCheck (semanticCheckTests)

main :: IO ()
main = void $ runTestTT $ TestList
  [ semanticCheckTests
  ]
