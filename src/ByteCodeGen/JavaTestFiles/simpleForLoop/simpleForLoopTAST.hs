{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ByteCodeGen.JavaTestFiles.SimpleForLoop.SimpleForLoopTAST where

import Types.TAST
import Types.Core

testAst :: Class
testAst = Types.TAST.Class 
  { caccess = Public
  , cname = "simpleforLoopClass"
  , cextends = ""
  , cfields = []
  , cmethods = 
    [ Method 
      { maccess = Public
      , mtype = Void
      , mstatic = True
      , mname = "simpleForLoop"
      , mparams = []
      , mbody = 
        Block
          [ 
            LocalVarDecl Types.Core.Int "i" (Just (Literal Types.Core.Int (IntLit 0))), -- i = 0
            While -- Expr(Binary) Stmt(Block)
              -- Binary Type BinOperator Expr Expr
              (Binary Types.Core.Bool
                Types.Core.LT -- less than -> i < 5
                  (Name Types.Core.Int "i") -- i
                  (Literal Types.Core.Int (IntLit 5))
              ) -- 5
              (Block 
                [
                  StmtOrExprAsStmt
                  -- Assign (Maybe Expr) LocalOrFieldName Expr
                  (Assign
                      Nothing -- No target object, assuming "i" is a local variable
                      "i" -- Local variable or field to be assigned
                      -- Binary Type BinOperator Expr Expr
                      (Binary Types.Core.Int
                          Add 
                          (Name Types.Core.Int "i")
                          (Literal Types.Core.Int (IntLit 1))))
                          ,
                  -- -- New for tests
                  Types.TAST.If 
                    (Literal Types.Core.Bool (BoolLit True)) -- Expr
                      (LocalVarDecl Types.Core.Int "j" (Just (Literal Types.Core.Int (IntLit 0)))) -- Stmt
                      (Just  -- Else
                        (LocalVarDecl Types.Core.Int "q" (Just (Literal Types.Core.Int (IntLit 0))))
                    ), -- Maybe Stmt
                  -- -- End new for tests
                  LocalVarDecl Types.Core.Int "z" (Just (Literal Types.Core.Int (IntLit 10)))
                ]
              ),
            LocalVarDecl Types.Core.Int "w" (Just (Literal Types.Core.Int (IntLit 100)))
          ]
      }
    ]
  }
