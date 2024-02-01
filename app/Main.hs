module Main (main) where

import ByteCodeGen.ByteCodeGen
import ConstantPool.ConstantPool (wrapConstantPool)
import Control.Monad (when, zipWithM_)
import Debug.Trace (traceShowId)
import Jvm.BinaryClass (encodeClassFile)
import Parser.IndexParser (parser)
import Scanner.Lexer (scanner)
import SemanticCheck.Typecheck (typecheck)
import Types.TAST (Class (cname))

examplePath = "Examples/Main.java"

debug = True

fromRight' :: Either String b -> b
fromRight' (Left err) = error err
fromRight' (Right succ) = succ

compile :: IO ()
compile = do
  -- putStrLn "Enter a file to compile: "
  -- filePath <- getLine
  let filePath = examplePath
  file <- readFile filePath

  let lexems = scanner file
  when debug $ do
    print "Lexer output:"
    print lexems
  let ast = fromRight' $ parser lexems
  when debug $ do
    print "Parser output:"
    print ast
  let tast = typecheck ast
  when debug $ do
    print "Typecheck output:"
    print tast
  let withPools = toSnd wrapConstantPool <$> tast
  when debug $ do
    print "Constant pool output:"
    print "Currently not available"
  let classFiles = codeGen withPools
  when debug $ do
    print "Codegen output:"
    print classFiles

  let filePaths = (\n -> "./Examples/" ++ n ++ ".class") . cname <$> tast
  zipWithM_ encodeClassFile filePaths classFiles
  where
    toSnd :: (a -> b) -> a -> (a, b)
    toSnd f x = (x, f x)

main :: IO ()
main = compile
