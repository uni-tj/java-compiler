{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ByteCodeGen.JavaTestFiles.SimpleForLoop.SimpleForLoopTAST where

import Types.TAST
import Types.Core

testAst :: Class
testAst = Types.TAST.Class 
  { cvisibility = Public
  , cname = "simpleforLoopClass"
  , cextends = ""
  , cfields = []
  , cmethods = 
    [ Method 
      { mvisibility = Public
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
                Types.Core.LT -- lower than -> i < 5
                  (Name Types.Core.Int "i") -- i
                  (Literal Types.Core.Int (IntLit 5))
              ) -- 5
              (Block 
                [ StmtOrExprAsStmt
                  -- Assign (Maybe Expr) LocalOrFieldName Expr
                  (Assign
                      Nothing -- No target object, assuming "i" is a local variable
                      "i" -- Local variable or field to be assigned
                      -- Binary Type BinOperator Expr Expr
                      (Binary Types.Core.Int
                          Add 
                          (Name Types.Core.Int "i")
                          (Literal Types.Core.Int (IntLit 1))))
                ]
              )
          ]
      }
    ]
  }
