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
    { TAST.cvisibility = Public,
      TAST.cname = "TestClass",
      TAST.cextends = "",
      TAST.cfields =
        [ TAST.Field Core.Int "IntField" Nothing,
          TAST.Field Core.Int "StringField" Nothing
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
emptyBody = TAST.Block Core.Void []

testMethodDescriptor2 :: (Bool, String)
testMethodDescriptor2 =
  let method = TAST.Method Public (Class "Class1") False "Method" [(Int, "i"), (Bool, "flag"), (Class "Class2", "obj")] emptyBody
      expectedDescriptor = "(IZLClass2;)LClass1;"
      res = methodDescriptor method
   in (res == expectedDescriptor, res)

main :: IO ()
main = do
  let initialCp = []
  let (_, finalCp) = runState (buildConstantPool testClass) initialCp

  _ <-
    ( mapM_
        print
        finalCp
      )

  putStrLn $ show testMethodDescriptor1
  putStrLn $ show testMethodDescriptor2
