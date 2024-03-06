module Compiler (compile, DebugMode(..)) where
import           ByteCodeGen.ByteCodeGen   (codeGen)
import           ConstantPool.ConstantPool (wrapConstantPool)
import           Control.Monad.Extra       (when, zipWithM_)
import           Data.List.Extra           (dropWhileEnd)
import           Jvm.BinaryClass           (encodeClassFile)
import           Parser.Parser             (parser)
import           Scanner.Lexer             (scanner)
import           SemanticCheck.Typecheck   (typecheck)
import           Types.TAST                (Class (cname))

directory :: FilePath -> FilePath
directory = dropWhileEnd (/= '/')

fromRight' :: Either String b -> b
fromRight' (Left err)   = error err
fromRight' (Right succ) = succ

data DebugMode = Debug | NoDebug
  deriving (Show, Eq)

compile :: FilePath -> DebugMode -> IO ()
compile path debug = do
  file <- readFile path

  let lexems = scanner file
  when (debug == Debug) $ do
    print "Lexer output:"
    print lexems
  let ast = fromRight' $ parser lexems
  when (debug == Debug) $ do
    print "Parser output:"
    print ast
  let tast = typecheck (lines file) ast
  when (debug == Debug) $ do
    print "Typecheck output:"
    print tast
  let withPools = toSnd wrapConstantPool <$> tast
  when (debug == Debug) $ do
    print "Constant pool output:"
    print "Currently not available"
  let classFiles = codeGen withPools
  when (debug == Debug) $ do
    print "Codegen output:"
    print classFiles

  let filePaths = (\n -> directory path ++ n ++ ".class") . cname <$> tast
  zipWithM_ encodeClassFile filePaths classFiles
  where
    toSnd :: (a -> b) -> a -> (a, b)
    toSnd f x = (x, f x)
