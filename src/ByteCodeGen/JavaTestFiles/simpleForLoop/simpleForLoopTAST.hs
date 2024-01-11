{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ByteCodeGen where

import Types.TAST
import Types.Core

test :: Class
test = Types.TAST.Class 
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
        Block Void 
          [ 
            LocalVarDecl Types.Core.Int Types.Core.Int "i" (Just (Literal Types.Core.Int (IntLit Types.Core.Int 0))), -- i = 0
            While Types.Core.Bool 
              -- Binary Type BinOperator Expr Expr
              (Binary Types.Core.Bool
                Types.Core.LT -- lower than -> i < 5
                  (Name Types.Core.Int "i") -- i
                  (Literal Types.Core.Int (IntLit Types.Core.Int 5))) -- 5
              (Block Void 
              [ StmtOrExprAsStmt Void 
                -- Assign Type Expr Expr
                (Assign Void 
                  (Name Types.Core.Int "i") 
                  -- Binary Type BinOperator Expr Expr
                  (Binary Types.Core.Int 
                    Add 
                      (Name Types.Core.Int "i") 
                      (Literal Types.Core.Int (IntLit Types.Core.Int 1))))
              ]
              )
          ]
      }
    ]
  }
