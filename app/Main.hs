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
    { TAST.caccess = Core.Public,
      TAST.cname = "SimpleClass",
      TAST.cextends = "", -- Java SimpleClass does not extend another class
      TAST.cfields =
        [ TAST.Field Core.Private False Core.Int "number" Nothing,
          TAST.Field Core.Private True Core.Bool "flag" Nothing
        ],
      TAST.cmethods =
        [ TAST.Method
            Core.Public
            (Core.Instance "SimpleClass")
            False
            "SimpleClass"
            [(Core.Int, "initialNumber")]
            (TAST.Block [TAST.LocalVarDecl Core.Int "number" (Just (TAST.Literal Core.Int (TAST.IntLit 8999999)))]),
          TAST.Method
            Core.Public
            Core.Int
            False
            "getNumber"
            []
            (TAST.Return (Just (TAST.FieldAccess Core.Int (TAST.This (Core.Instance "SimpleClass")) "SimpleClass" "number"))),
          TAST.Method
            Core.Public
            Core.Void
            True
            "setFlag"
            [(Core.Bool, "newFlag")]
            (TAST.Block [TAST.LocalVarDecl Core.Void "flag" (Just (TAST.LocalVar Core.Bool "newFlag"))])
        ]
    }

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
