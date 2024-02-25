module Test.SemanticCheck.SemanticCheck (semanticCheckTests) where

import           Test.HUnit                          (Test, (~:))
import           Test.SemanticCheck.MethodResolution (methodResolutionTests)
import           Test.SemanticCheck.ReturnInjection  (returnInjectionTests)

semanticCheckTests :: Test
semanticCheckTests = "SemantikCheck" ~:
  [ methodResolutionTests
  , returnInjectionTests
  ]
