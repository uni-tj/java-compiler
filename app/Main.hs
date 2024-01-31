module Main (main) where

import ByteCodeGen.ByteCodeGen
import ByteCodeGen.JavaTestFiles.Classes.ClassesTAST
import ConstantPool.ConstantPool as CP
import Debug.Trace (traceShowId)
import Jvm.BinaryClass (encodeClassFile)
import Types.TAST

main :: IO ()
main = do
  let tastClasses = classes
  let cps = (map (\x -> (x, buildConstantPool x)) tastClasses)
  let cf = zipWith (\a b -> ("./javaOut/" ++ (a) ++ ".class", b)) (map cname tastClasses) $ codeGen cps

  let f = (\(fp, cf) -> encodeClassFile (fp) (traceShowId cf))
  mapM_ f cf
