module ByteCodeGen.JavaTestFiles.GGT.GgtTAST where

import Types.TAST
import Types.Core

ggtErw :: Program
ggtErw =
  [ Types.TAST.Class
      { caccess = Public
      , cname = "ggtErw"
      , cextends = ""
      , cfields = []
      , cmethods = [
            ggTMethod, 
            mainMethod
        ]
      }
  ]

ggTMethod :: Method
ggTMethod =
  Method
    { maccess = Types.Core.Package
    , mtype = Types.Core.Int
    , mstatic = True
    , mname = "ggT"
    , mparams = [(Types.Core.Int, "a"), (Types.Core.Int, "b")]
    , mbody =
        If
            (Binary Types.Core.Bool Types.Core.LT 
                (LocalVar Types.Core.Int "a") 
                (Literal Types.Core.Int (IntLit 0)))
            (Return (Just (Literal Types.Core.Int (IntLit (-1)))))
        (Just (
            If
                (Binary Types.Core.Bool Types.Core.EQ 
                    (LocalVar Types.Core.Int "a") 
                    (LocalVar Types.Core.Int "b"))
                (Return (Just (LocalVar Types.Core.Int "a")))
            (Just (
                If (Binary Types.Core.Bool Types.Core.GT 
                        (LocalVar Types.Core.Int "a") 
                        (LocalVar Types.Core.Int "b"))
                    (Return (Just (StmtOrExprAsExpr (Types.Core.Int) 
                        (MethodCall 
                            (This (Types.Core.Class "ggtErw")) 
                            "ggT" 
                            [
                                (Types.Core.Int, Binary Types.Core.Int Types.Core.Sub 
                                    (LocalVar Types.Core.Int "a") 
                                    (LocalVar Types.Core.Int "b")), 
                                (Types.Core.Int, LocalVar Types.Core.Int "b")
                            ]
                        )
                    )))
                (Just (
                    Return (Just (StmtOrExprAsExpr (Types.Core.Int) 
                        (MethodCall (This (Types.Core.Class "ggtErw")) 
                        "ggT" 
                        [
                            (Types.Core.Int, Binary Types.Core.Int Types.Core.Sub 
                                (LocalVar Types.Core.Int "b") 
                                (LocalVar Types.Core.Int "a")), 
                            (Types.Core.Int, LocalVar Types.Core.Int "a")]))))
                ))
            ))
        )
    }

mainMethod :: Method
mainMethod =
  Method
    { maccess = Types.Core.Public
    , mtype = Types.Core.Void
    , mstatic = True
    , mname = "main"
    , mparams = [(Types.Core.StringArr, "args")]
    , mbody =
        Block
          [ 
            LocalVarDecl Types.Core.Int "number" (Just (StmtOrExprAsExpr (Types.Core.Int) 
                (MethodCall 
                    (This (Types.Core.Class "ggtErw")) 
                    "ggT" 
                    [
                        (Types.Core.Int, Literal Types.Core.Int (IntLit 3)), 
                    (Types.Core.Int, Literal Types.Core.Int (IntLit 6))
                    ]
                )
            )),
            StmtOrExprAsStmt (MethodCall 
                (This (Types.Core.Class "System.out")) 
                "println" 
                [
                    (Types.Core.Int, 
                    LocalVar Types.Core.Int "number")
                ]
            )
          ]
    }

