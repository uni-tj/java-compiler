module Test.SemanticCheck.Helpers (pos, defCrT, classRef, classRefT, main, mainT, a , aT, b, bT, noPars, noParsT, specific, specificT, general, generalT, methodCall, methodCallT, new, newT, block1, block1T, emptyBlock, emptyBlockT) where

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
defCrT cextends = TAST.Constructor Public [] $ TAST.Block [ TAST.SuperCall (fromJust cextends) [], TAST.Return Nothing ]

classRef :: ClassName -> AST.Expr
classRef cname = AST.Name pos cname
classRefT :: ClassName -> TAST.Expr
classRefT cname = TAST.ClassRef (Class cname) cname

main :: [AST.Method] -> [AST.Stmt] -> AST.Class
main ms stmts = AST.Class Public "Main" Nothing [] ms [cons]
  where cons = AST.Constructor Public "Main" [] $ AST.Block pos (AST.SuperCall pos [] : stmts ++ [AST.Return pos Nothing])
mainT :: [TAST.Method] -> [TAST.Stmt] -> TAST.Class
mainT ms stmts = TAST.Class Public "Main" (Just "java/lang/Object") [] ms [cons]
  where cons = TAST.Constructor Public [] $ TAST.Block (TAST.SuperCall "java/lang/Object" [] : stmts ++ [TAST.Return Nothing])

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
noParsT = TAST.Method Public Void True "noPars" [] $ TAST.Block [TAST.Return Nothing]

specific :: AST.Method
specific = AST.Method False Public True Void "foo" [(Instance "B", "v")] emptyBlock
specificT :: TAST.Method
specificT = TAST.Method Public Void True "foo" [(Instance "B", "v")] emptyBlockT

general :: AST.Method
general = AST.Method False Public True Void "foo" [(Instance "A", "v")] emptyBlock
generalT :: TAST.Method
generalT = TAST.Method Public Void True "foo" [(Instance "A", "v")] emptyBlockT

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
block1 stmt = AST.Block pos [stmt, AST.Return pos Nothing]
block1T :: TAST.Stmt -> TAST.Stmt
block1T stmt = TAST.Block [stmt, TAST.Return Nothing]

emptyBlock :: AST.Stmt
emptyBlock = AST.Block pos [ AST.Return pos Nothing ]
emptyBlockT :: TAST.Stmt
emptyBlockT = TAST.Block [ TAST.Return Nothing ]
