module Main (main) where

import ByteCodeGen.ByteCodeGen
import ByteCodeGen.JavaTestFiles.Classes.ClassesTAST
import ConstantPool.ConstantPool as CP
import Control.Monad (when)
import Debug.Trace (traceShowId)
import Jvm.BinaryClass (decodeClassFile, encodeClassFile)
import Jvm.Data.ClassFormat
import Types.TAST

main :: IO ()
main = do
  -- let cps = (map (\x -> (x, buildConstantPool x)) classes)
  -- -- mapM_ (\x -> mapM_ (print) (snd (buildConstantPool x))) classes
  -- let cf = codeGen cps
  --
  -- let files = zipWith (\a b -> ("./Class" ++ (show b) ++ ".class", a)) cf [1 ..]
  -- let f = (\(fp, cf) -> encodeClassFile fp cf)
  -- mapM_ print files
  -- mapM_ f files

  let test = classes
  let cps = (map (\x -> (x, buildConstantPool x)) test)
  -- mapM_ (\x -> mapM_ (print) (snd (buildConstantPool x))) classes
  let cf = zipWith (\a b -> (a, b)) (map cname test) $ codeGen cps

  let files = map (\(a, b) -> ("./" ++ (a) ++ ".class", b)) cf
  let f = (\(fp, cf) -> encodeClassFile (fp) (traceShowId cf))
  mapM_ f files

{- let a = head cf
print a
encodeClassFile "./A.class" a

-- b <- decodeClassFile "./A.class"
b <- decodeClassFile "./ClassA.class"
encodeClassFile "./WorkingClassA.class" b
print b -}
