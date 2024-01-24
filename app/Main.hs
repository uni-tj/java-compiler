module Main where

import Bytecode.ConstantPool
import Control.Monad.State (runState)
import qualified Jvm.Data.ClassFormat as CF
import Types.Core as Core
import qualified Types.TAST as TAST

-- Simple Test Class
testClass :: TAST.Class
testClass =
  TAST.Class
    { TAST.cname = "TestClass",
      TAST.cextends = "",
      TAST.cfields =
        [ TAST.Field Core.Public False Core.Int "IntField" Nothing,
          TAST.Field Core.Public False Core.Int "StringField" Nothing
        ],
      TAST.cmethods =
        []
    }

testMethodDescriptor1 :: (Bool, String)
testMethodDescriptor1 =
  let method = TAST.Method Public Core.Void False "testMethod" [] emptyBody
      expectedDescriptor = "()V"
      res = methodDescriptor method
   in (res == expectedDescriptor, res)

emptyBody :: TAST.Stmt
emptyBody = TAST.Block []

testMethodDescriptor2 :: (Bool, String)
testMethodDescriptor2 =
  let method = TAST.Method Public (Class "Class1") False "Method" [(Int, "i"), (Bool, "flag"), (Class "Class2", "obj")] emptyBody
      expectedDescriptor = "(IZLClass2;)LClass1;"
      res = methodDescriptor method
   in (res == expectedDescriptor, res)

simpleClassTAST :: TAST.Class
simpleClassTAST =
  TAST.Class
    Core.Public
    "SimpleClass"
    "java/lang/Object"
    [ TAST.Field Core.Private False Core.Int "number" Nothing,
      TAST.Field Core.Public True Core.Bool "flag" (Just (TAST.Literal Core.Bool (TAST.BoolLit False)))
    ]
    [ TAST.Method
        Core.Public
        Core.Int
        False
        "SimpleClass"
        [(Core.Int, "initialNumber")]
        ( TAST.Block
            [ TAST.StmtOrExprAsStmt
                (TAST.Assign Nothing "number" (TAST.Name Core.Int "initialNumber")),
              TAST.StmtOrExprAsStmt
                ( TAST.Assign
                    Nothing
                    "number"
                    (TAST.Literal Core.Int (TAST.IntLit (0 - 690000)))
                )
            ]
        ),
      TAST.Method
        Core.Public
        Core.Int
        False
        "getNumber"
        []
        ( TAST.Block
            [ TAST.Return (Just (TAST.Name Core.Int "number"))
            ]
        ),
      TAST.Method
        Core.Public
        Core.Void
        True
        "setFlag"
        [(Core.Bool, "newFlag")]
        ( TAST.Block
            [ TAST.StmtOrExprAsStmt (TAST.Assign Nothing "flag" (TAST.Name Core.Bool "newFlag"))
            ]
        )
    ]

main :: IO ()
main = do
  let initialCp = []
  let (_, finalCp) = runState (buildConstantPool simpleClassTAST) initialCp

  _ <-
    ( mapM_
        print
        finalCp
      )

  putStrLn $ show testMethodDescriptor1

-- putStrLn $ show testMethodDescriptor2
