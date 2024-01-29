module ByteCodeGen.JavaTestFiles.Classes.ClassesTAST where

import Control.Monad (when)
import Types.Core
import Types.TAST

classes :: Program
classes =
  [ Types.TAST.Class
      { caccess = Public,
        cname = "Add",
        cextends = Nothing,
        cfields = [],
        cconstructors =
          [ Constructor
              { crparams = [],
                crbody =
                  Block
                    [ SuperCall "java/lang/Object" [],
                      Return Nothing
                    ],
                craccess = Public
              }
          ],
        cmethods =
          [ Method
              { maccess = Public,
                mtype = Void,
                mstatic = True,
                mname = "add",
                mparams = [(Int, "a"), (Int, "b")],
                mbody =
                  Block
                    [ ( Return
                          ( Just
                              (Binary Int Mul (LocalVar Int "a") (LocalVar Int "a"))
                          )
                      )
                    ]
              }
          ]
      },
    Types.TAST.Class
      { caccess = Public,
        cname = "ggt",
        cextends = Nothing,
        cfields = [],
        cconstructors =
          [ Constructor
              { crparams = [],
                crbody =
                  Block
                    [ SuperCall "java/lang/Object" [],
                      Return Nothing
                    ],
                craccess = Public
              }
          ],
        cmethods =
          [ Method
              { maccess = Public,
                mtype = Int,
                mstatic = True,
                mname = "ggT",
                mparams = [(Int, "a"), (Int, "b")],
                mbody =
                  Block
                    [ LocalVarDecl (Instance "Add") "instance" (Just (StmtOrExprAsExpr (New (Types.Core.Instance "Add") "Add" []))),
                      Return
                        ( Just
                            ( StmtOrExprAsExpr (MethodCall Types.Core.Int (LocalVar (Instance "Class1") "") "instance" True "add" [(Types.Core.Int, Literal Types.Core.Int (IntLit 5))])
                            )
                        )
                    ]
              }
          ]
      }
  ]

{- classes :: Program
classes =
  [ Types.TAST.Class
      { caccess = Public,
        cname = "Class1",
        cextends = Nothing,
        cconstructors = [],
        cfields =
          [ Field
              { faccess = Types.Core.Package,
                fstatic = False,
                ftype = Types.Core.Int,
                fname = "c",
                finit = Nothing
              }
          ],
        cmethods =
          [ Method
              { maccess = Types.Core.Public,
                mtype = Types.Core.Int,
                mstatic = False,
                mname = "Class1",
                mparams = [],
                mbody =
                  Block
                    [ StmtOrExprAsStmt
                        ( FieldAssign
                            Types.Core.Int
                            (This (Instance "Class1"))
                            "Class1"
                            False
                            "c"
                            (Literal Types.Core.Int (IntLit 10))
                        )
                    ]
              },
            Method
              { maccess = Types.Core.Public,
                mtype = Types.Core.Int,
                mstatic = True,
                mname = "staticMethod",
                mparams =
                  [ (Types.Core.Int, "a"),
                    (Types.Core.Int, "b")
                  ],
                mbody =
                  Return (Just (Binary Types.Core.Int Add (LocalVar Types.Core.Int "a") (LocalVar Types.Core.Int "b")))
              },
            Method
              { maccess = Types.Core.Public,
                mtype = Types.Core.Int,
                mstatic = False,
                mname = "nonStaticMethod",
                mparams = [(Types.Core.Int, "a")],
                mbody =
                  Return
                    ( Just
                        ( Binary
                            Types.Core.Int
                            Mul
                            (LocalVar Types.Core.Int "a")
                            ( FieldAccess
                                Types.Core.Int
                                (This (Instance "Class1"))
                                "Class1"
                                False
                                "c"
                            )
                        )
                    )
              }
          ]
      },
    Types.TAST.Class
      { caccess = Public,
        cname = "Class2",
        cextends = Nothing,
        cfields = [],
        cconstructors = [],
        cmethods =
          [ Method
              { maccess = Public,
                mtype = Void,
                mstatic = True,
                mname = "main",
                mparams = [(StringArr, "args")],
                mbody =
                  Block
                    [ LocalVarDecl
                        Types.Core.Int
                        "a"
                        ( Just
                            ( StmtOrExprAsExpr (MethodCall Types.Core.Int (ClassRef (Types.Core.Class "Class1") "Class1") "Class1" True "staticMethod" [(Types.Core.Int, Literal Types.Core.Int (IntLit 5)), (Types.Core.Int, Literal Types.Core.Int (IntLit 5))])
                            )
                        ),
                      LocalVarDecl (Instance "Class1") "classAInstance" (Just (StmtOrExprAsExpr (New (Types.Core.Instance "Class1") "Class1" []))),
                      LocalVarDecl
                        Types.Core.Int
                        "b"
                        ( Just
                            ( StmtOrExprAsExpr (MethodCall Types.Core.Int (LocalVar (Instance "Class1") "classAInstance") "Class1" False "nonStaticMethod" [(Types.Core.Int, Literal Types.Core.Int (IntLit 5))])
                            )
                        ),
                      StmtOrExprAsStmt
                        ( MethodCall Types.Core.Void (ClassRef (Types.Core.Class "System.out") "System.out") "System.out" False "println" [(Types.Core.Int, LocalVar Types.Core.Int "a")]
                        ),
                      StmtOrExprAsStmt
                        ( MethodCall Types.Core.Void (ClassRef (Types.Core.Class "System.out") "System.out") "System.out" False "println" [(Types.Core.Int, LocalVar Types.Core.Int "b")]
                        )
                    ]
              }
          ]
      }
  ] -}
