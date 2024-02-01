module Main (main) where

import           ByteCodeGen.ByteCodeGen
import           ConstantPool.ConstantPool (buildConstantPool)
import           Control.Monad             (zipWithM_)
import           Debug.Trace               (traceShowId)
import           Jvm.BinaryClass           (encodeClassFile)
import           Parser.IndexParser        (parser)
import           Scanner.Lexer             (scanner)
import           SemanticCheck.Typecheck   (typecheck)
import           Types.TAST                (Class (cname))

examplePath = "Examples/Main.java"

fromRight' :: Either String b -> b
fromRight' (Left err)   = error err
fromRight' (Right succ) = succ

compile :: IO ()
compile = do
  -- putStrLn "Enter a file to compile: "
  -- filePath <- getLine
  let filePath = examplePath
  file <- readFile filePath

  let lexems = scanner file
  let ast = fromRight' $ parser lexems
  let tast = typecheck ast
  let withPools = toSnd buildConstantPool <$> tast
  let classFiles = codeGen withPools

  let filePaths = (\n -> "./Examples/" ++ n ++ ".class") . cname <$> tast
  zipWithM_ encodeClassFile filePaths classFiles
  where toSnd :: (a -> b) -> a -> (a, b)
        toSnd f x = (x, f x)

main :: IO ()
main = compile
