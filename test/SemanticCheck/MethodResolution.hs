module SemanticCheck.MethodResolution (methodResolutionTests) where

import           Control.Composition     ((.*), (.**))
import           Data.Function.Slip      (slipr)
import           Data.List               (singleton)
import           Data.Maybe              (fromJust)
import           Prelude                 hiding (EQ, GT, LT)
import           SemanticCheck.Typecheck (typecheck)
import           SemanticCheck.Util
import           Test.HUnit              (Test, (~:), (~=?), (~?=))
import qualified Types.AST               as AST
import           Types.Core              (AccessModifier (..), BinOperator (..),
                                          ClassName, Identifier, MethodName,
                                          Type (..), UnOparator (..))
import qualified Types.TAST              as TAST

pos :: AST.Position
pos = AST.Position { AST.start = (0,0), AST.end = (0,0) }

defCrT :: Maybe ClassName -> TAST.Constructor
defCrT cextends = TAST.Constructor Public [] $ TAST.Block [ TAST.SuperCall (fromJust cextends) [] ]

classRef :: ClassName -> AST.Expr
classRef cname = AST.Name pos cname
classRefT :: ClassName -> TAST.Expr
classRefT cname = TAST.ClassRef (Class cname) cname

main :: [AST.Method] -> [AST.Stmt] -> AST.Class
main ms stmts = AST.Class Public "Main" Nothing [] ms [cons]
  where cons = AST.Constructor Public "Main" [] $ AST.Block pos (AST.SuperCall pos [] : stmts)
mainT :: [TAST.Method] -> [TAST.Stmt] -> TAST.Class
mainT ms stmts = TAST.Class Public "Main" (Just "java/lang/Object") [] ms [cons]
  where cons = TAST.Constructor Public [] $ TAST.Block (TAST.SuperCall "java/lang/Object" [] : stmts)

a :: [AST.Method] -> AST.Class
a methods = AST.Class Public "A" Nothing [] methods []
aT :: [TAST.Method] -> TAST.Class
aT methods = TAST.Class Public "A" cextends [] methods [defCrT cextends]
  where cextends = Just "java/lang/Object"

b :: [AST.Method] -> AST.Class
b methods = AST.Class Public "B" (Just "A") [] methods []
bT :: [TAST.Method] -> TAST.Class
bT methods = TAST.Class Public "B" cextends [] methods [defCrT cextends]
  where cextends = Just "A"

noPars :: AST.Method
noPars = AST.Method False Public True Void "noPars" [] $ AST.Block pos []
noParsT :: TAST.Method
noParsT = TAST.Method Public Void True "noPars" [] $ TAST.Block []

specific :: AST.Method
specific = AST.Method False Public True Void "foo" [(Instance "B", "v")] body
  where body = AST.Block pos []
specificT :: TAST.Method
specificT = TAST.Method Public Void True "foo" [(Instance "B", "v")] body
  where body = TAST.Block []

general :: AST.Method
general = AST.Method False Public True Void "foo" [(Instance "A", "v")] body
  where body = AST.Block pos []
generalT :: TAST.Method
generalT = TAST.Method Public Void True "foo" [(Instance "A", "v")] body
  where body = TAST.Block []

methodCall = AST.StmtOrExprAsStmt pos .** AST.MethodCall
methodCallT :: TAST.Expr -> ClassName -> TAST.Method -> [TAST.Expr] -> TAST.Stmt
methodCallT expr' cname m args'
  = TAST.StmtOrExprAsStmt
  $ TAST.MethodCall (typee m) expr' cname (static m) (name m)
  $ zip (ptypes m) args'

new = AST.StmtOrExprAsExpr pos .* AST.New
newT :: ClassName -> TAST.Constructor -> [TAST.Expr] -> TAST.Expr
newT cname cr args' = TAST.StmtOrExprAsExpr $ TAST.New (Instance cname) cname $ zip (ptypes cr) args'

block1 :: AST.Stmt -> AST.Stmt
block1 stmt = AST.Block pos [stmt]
block1T :: TAST.Stmt -> TAST.Stmt
block1T stmt = TAST.Block [stmt]

methodResolutionTests :: Test
methodResolutionTests = "MethodResolution" ~:
  [ "nameToThis" ~: nameToThis
  , "nameToClass" ~: nameToClass
  , "specificFirst" ~: specificFirst
  , "specificLast" ~: specificLast
  , "specificInSub" ~: specificInSub
  , "specificInSuper" ~: specificInSuper
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
