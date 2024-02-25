module Test.SemanticCheck.ReturnInjection (returnInjectionTests) where

import           Prelude                    hiding (EQ, GT, LT)
import           SemanticCheck.Typecheck    (typecheck)
import           Test.HUnit                 (Test, (~:), (~?=))
import           Test.SemanticCheck.Helpers
import qualified Types.AST                  as AST
import           Types.Core                 (AccessModifier (..), Type (..))
import qualified Types.TAST                 as TAST

returnInjectionTests :: Test
returnInjectionTests = "MethodResolution" ~:
  [ "injectReturn" ~: injectReturn
  ]

injectReturn :: Test
injectReturn = typecheck
  [ main
    [ AST.Method False Public True returnType "emptyMethod" []
      $ AST.Block pos []
    ]
    []
  ]
  ~?=
  [ mainT
    [ TAST.Method Public returnType True "emptyMethod" []
      $ TAST.Block [TAST.Return Nothing]
    ]
    []
  ]
  where returnType = Void
