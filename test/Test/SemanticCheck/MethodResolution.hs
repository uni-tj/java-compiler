module Test.SemanticCheck.MethodResolution (methodResolutionTests) where

import           Prelude                    hiding (EQ, GT, LT)
import           SemanticCheck.Typecheck    (typecheck)
import           Test.HUnit                 (Test, (~:), (~?=))
import           Test.SemanticCheck.Helpers
import qualified Types.AST                  as AST
import           Types.Core                 (AccessModifier (..), Type (..))
import qualified Types.TAST                 as TAST

methodResolutionTests :: Test
methodResolutionTests = "MethodResolution" ~:
  [ "nameToThis" ~: nameToThis
  , "nameToClass" ~: nameToClass
  , "specificFirst" ~: specificFirst
  , "specificLast" ~: specificLast
  , "specificInSub" ~: specificInSub
  , "specificInSuper" ~: specificInSuper
  , "staticOnThisInConstructor" ~: staticOnThisInConstructor
  ]

nameToThis :: Test
nameToThis = typecheck
  [ main
    [ noPars
    , AST.Method False Public mstatic Void "test" [] $ block1 $ methodCall Nothing "noPars" [] ]
    []
  ]
  ~?=
  [ mainT
    [ noParsT
    , TAST.Method Public Void mstatic "test" [] $ block1T $ methodCallT (TAST.This $ Instance "Main") "Main" noParsT [] ]
    []
  ]
  where
    mstatic = False

nameToClass :: Test
nameToClass = typecheck
  [ main
    [ noPars
    , AST.Method False Public mstatic Void "test" [] $ block1 $ methodCall Nothing "noPars" [] ]
    []
  ]
  ~?=
  [ mainT
    [ noParsT
    , TAST.Method Public Void mstatic "test" [] $ block1T $ methodCallT (TAST.ClassRef (Class "Main") "Main") "Main" noParsT [] ]
    []
  ]
  where
    mstatic = True

specificFirst :: Test
specificFirst = typecheck
  [ main []
    [ methodCall (Just $ classRef "B") "foo" [ new "B" [] ]
    ],
    a [],
    b [specific, general]
  ]
  ~?=
  [ mainT []
    [ methodCallT (classRefT "B") "B" specificT [newT "B" (defCrT Nothing) []]
    ],
    aT [],
    bT [specificT, generalT]
  ]
specificLast :: Test
specificLast = typecheck
  [ main []
    [ methodCall (Just $ classRef "B") "foo" [ new "B" [] ]
    ],
    a [],
    b [general, specific]
  ]
  ~?=
  [ mainT []
    [ methodCallT (classRefT "B") "B" specificT [newT "B" (defCrT Nothing) []]
    ],
    aT [],
    bT [generalT, specificT]
  ]

specificInSub :: Test
specificInSub = typecheck
  [ main []
    [ methodCall (Just $ classRef "B") "foo" [ new "B" [] ]
    ],
    a [general],
    b [specific]
  ]
  ~?=
  [ mainT []
    [ methodCallT (classRefT "B") "B" specificT [newT "B" (defCrT Nothing) []]
    ],
    aT [generalT],
    bT [specificT]
  ]
specificInSuper :: Test
specificInSuper = typecheck
  [ main []
    [ methodCall (Just $ classRef "B") "foo" [ new "B" [] ]
    ],
    a [specific],
    b [general]
  ]
  ~?=
  [ mainT []
    [ methodCallT (classRefT "B") "A" specificT [newT "B" (defCrT Nothing) []]
    ],
    aT [specificT],
    bT [generalT]
  ]

staticOnThisInConstructor :: Test
staticOnThisInConstructor = typecheck
  [ main
    [ noPars ]
    [ methodCall (Just $ AST.This pos) "noPars" [] ]
  ]
  ~?=
  [ mainT
    [ noParsT ]
    [ methodCallT (TAST.This $ Instance "Main") "Main" noParsT [] ]
  ]
