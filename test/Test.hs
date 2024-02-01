module Main (main) where

import           Control.Monad               (void)
import           SemanticCheck.SemantikCheck (semanticCheckTests)
import           Test.HUnit                  (Test (TestList), runTestTT)

main :: IO ()
main = void $ runTestTT $ TestList
  [ semanticCheckTests
  ]
