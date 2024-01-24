module ByteCodeGen.JavaTestFiles.Classes.ClassesTAST where

import Types.TAST
import Types.Core

classes :: Program
classes = [
    Types.TAST.Class {
        caccess = Public, 
        cname = "ClassA", 
        cextends = "", 
        cfields = [
            Field {
                faccess = Types.Core.Package, 
                fstatic = False, 
                ftype = Types.Core.Int, 
                fname = "c", 
                finit = Nothing
            }
        ], 
        cmethods = [
            Method { 
                maccess = Types.Core.Public, 
                mtype = Types.Core.Int,
                mstatic = False,
                mname = "ClassA",
                mparams = [], 
                mbody = Block [
                    StmtOrExprAsStmt (Assign 
                    (Just (This (Instance "ClassA")))
                    "c"
                    (Literal Types.Core.Int (IntLit 10)))
                ]
            },  
            Method { 
                maccess = Types.Core.Public, 
                mtype = Types.Core.Int,
                mstatic = True,
                mname = "staticMethod",
                mparams = [
                    (Types.Core.Int, "a"), 
                    (Types.Core.Int, "b")
                ], 
                mbody = 
                    Return (Just (Binary Types.Core.Int Add (LocalVar Types.Core.Int "a") (LocalVar Types.Core.Int "b")))
            }, 
            Method { 
                maccess = Types.Core.Public, 
                mtype = Types.Core.Int,
                mstatic = False,
                mname = "nonStaticMethod",
                mparams = [(Types.Core.Int, "a")],
                mbody = Return (
                    Just (
                        Binary Types.Core.Int Mul 
                        (LocalVar Types.Core.Int "a") 
                        (FieldAccess Types.Core.Int (This (Instance "ClassA")) 
                        "c")
                    )
                )
            }
        ]
    }, 
    Types.TAST.Class { 
        caccess = Public, 
        cname = "ClassB",
        cextends = "",
        cfields = [],
        cmethods = [ 
            Method { 
                maccess = Public,
                mtype = Void,
                mstatic = True,
                mname = "main",
                mparams = [(StringArr, "args")],
                mbody = Block [ 
                    LocalVarDecl Types.Core.Int "a" (Just (
                        StmtOrExprAsExpr Types.Core.Int (MethodCall (ClassRef (Types.Core.Class "ClassA") "ClassA") "staticMethod" [(Types.Core.Int, Literal Types.Core.Int (IntLit 5)), (Types.Core.Int, Literal Types.Core.Int (IntLit 5))])
                    )), 
                    LocalVarDecl (Instance "ClassA") "classAInstance" (Just (StmtOrExprAsExpr (Types.Core.Instance "ClassA") (New "ClassA" []))),
                    LocalVarDecl Types.Core.Int "b" (Just (
                        StmtOrExprAsExpr Types.Core.Int (MethodCall (LocalVar (Instance "ClassA") "classAInstance") "nonStaticMethod" [(Types.Core.Int, Literal Types.Core.Int (IntLit 5))])
                    )), 
                    StmtOrExprAsStmt (
                        MethodCall (ClassRef (Types.Core.Class "System.out") "System.out") "println" [(Types.Core.Int, LocalVar Types.Core.Int "a")]
                    ), 
                    StmtOrExprAsStmt (
                        MethodCall (ClassRef (Types.Core.Class "System.out") "System.out") "println" [(Types.Core.Int, LocalVar Types.Core.Int "b")]
                    )
                ]
            }
        ]
    }
    ]