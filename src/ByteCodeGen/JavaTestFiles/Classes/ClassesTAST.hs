module ByteCodeGen.JavaTestFiles.Classes.ClassesTAST where

import Types.TAST
import Types.Core

classes :: Program
classes = [
    Types.TAST.Class {
        caccess = Public, 
        cname = "ClassA", 
        cextends = Nothing,
        cconstructors = [], 
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
                    StmtOrExprAsStmt (FieldAssign
                    Types.Core.Int 
                    (This (Instance "ClassA")) 
                    "ClassA"
                    False
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
                        (FieldAccess 
                            Types.Core.Int 
                            (This (Instance "ClassA"))
                            "ClassA"
                            False
                            "c"
                        )
                    )
                )
            }
        ]
    }, 
    Types.TAST.Class { 
        caccess = Public, 
        cname = "ClassB",
        cextends = Nothing,
        cfields = [],
        cconstructors = [], 
        cmethods = [ 
            Method { 
                maccess = Public,
                mtype = Void,
                mstatic = True,
                mname = "main",
                mparams = [(StringArr, "args")],
                mbody = Block [ 
                    LocalVarDecl Types.Core.Int "a" (Just (
                        StmtOrExprAsExpr (MethodCall Types.Core.Int (ClassRef (Types.Core.Class "ClassA") "ClassA") "ClassA" True "staticMethod" [(Types.Core.Int, Literal Types.Core.Int (IntLit 5)), (Types.Core.Int, Literal Types.Core.Int (IntLit 5))])
                    )), 
                    LocalVarDecl (Instance "ClassA") "classAInstance" (Just (StmtOrExprAsExpr (New (Types.Core.Instance "ClassA") "ClassA" []))),
                    LocalVarDecl Types.Core.Int "b" (Just (
                        StmtOrExprAsExpr (MethodCall Types.Core.Int (LocalVar (Instance "ClassA") "classAInstance") "ClassA" False "nonStaticMethod" [(Types.Core.Int, Literal Types.Core.Int (IntLit 5))])
                    )), 
                    StmtOrExprAsStmt (
                        MethodCall Types.Core.Void (ClassRef (Types.Core.Class "System.out") "System.out") "System.out" False "println" [(Types.Core.Int, LocalVar Types.Core.Int "a")]
                    ), 
                    StmtOrExprAsStmt (
                        MethodCall Types.Core.Void (ClassRef (Types.Core.Class "System.out") "System.out") "System.out" False "println" [(Types.Core.Int, LocalVar Types.Core.Int "b")]
                    )
                ]
            }
        ]
    }
    ]