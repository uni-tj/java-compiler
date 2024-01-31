module SemanticCheck.SemantikCheck (semanticCheckTests) where

import           SemanticCheck.MethodResolution (methodResolutionTests)
import           Test.HUnit                     (Test, (~:))

semanticCheckTests :: Test
semanticCheckTests = "SemantikCheck" ~:
  [ methodResolutionTests
  ]
