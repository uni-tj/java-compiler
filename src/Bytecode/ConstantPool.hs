module Bytecode.ConstantPool where

import Control.Monad.State
import Data.Bool (Bool (True))
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (strip)
import Data.List
import qualified Data.Text as Core
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Debug.Trace
import Jvm.Data.ClassFormat as CF
import qualified Jvm.Data.ClassFormat as TAST
import Types.Core
import Types.Core as Core
import Types.TAST as TAST

type ConstantPool = [(Int, CF.CP_Info)]

type ConstantPoolState = State ConstantPool

strLen :: String -> Int
typee :: TAST.Expr -> Core.Type
typee _ = Core.Int
-- TODO Implementation from Utils Timon

strLen str = BS.length $ TE.encodeUtf8 $ T.pack str

currIdx :: ConstantPool -> Int
currIdx [] = 1
currIdx cp = maximum $ map fst cp

insertEntry :: CF.CP_Info -> ConstantPoolState Int
insertEntry elem = do
  cp <- get
  case find ((==) elem . snd) cp of
    Just idx -> return $ fst idx
    Nothing -> do
      let idx = currIdx cp
      let newCp = (idx + 1, elem) : cp
      put newCp
      return (idx + 1)

insertUtf8 :: String -> ConstantPoolState Int
insertUtf8 str =
  insertEntry
    CF.Utf8_Info
      { tag_cp = CF.TagUtf8,
        tam_cp = strLen str,
        cad_cp = str,
        desc = ""
      }

insertInt :: Int -> ConstantPoolState Int
insertInt int =
  insertEntry
    CF.Integer_Info
      { tag_cp = TagInteger,
        numi_cp = int,
        desc = ""
      }

insertClassInfo :: String -> ConstantPoolState Int
insertClassInfo cname = do
  cIdx <- insertUtf8 cname
  insertEntry
    CF.Class_Info
      { tag_cp = CF.TagClass,
        index_cp = cIdx,
        desc = ""
      }

insertClass :: TAST.Class -> ConstantPoolState Int
insertClass (TAST.Class _ cname cextends _ _) = do
  _ <- insertClassInfo cextends
  insertClassInfo cname

convertTypeToString :: Core.Type -> String
convertTypeToString Core.Int = "I"
convertTypeToString Core.Char = "C"
convertTypeToString Core.Bool = "Z"
convertTypeToString Core.Void = "V"
convertTypeToString Core.NullType = ""
convertTypeToString (Core.Instance cname) = "L" ++ cname ++ ";"
convertTypeToString (Core.Class cname) = "L" ++ cname ++ ";"
convertTypeToString Core.StringArr = "[Ljava/lang/String;"

-- TODO Checking StringArray Issues andPrintln

constructDescriptor :: Core.Type -> [Core.Type] -> String
constructDescriptor rType argTypes =
  "("
    ++ foldr (\x accu -> convertTypeToString x ++ accu) "" argTypes
    ++ ")"
    ++ convertTypeToString rType

methodDescriptor :: TAST.Method -> String
methodDescriptor (Method _ mtype _ mname params _) =
  constructDescriptor mtype (map fst params)

insertMethod :: Int -> TAST.Method -> ConstantPoolState Int
insertMethod cIdx m@(Method _ _ _ mname _ mbody) = do
  -- Insert Method Signature and Name
  mnameIdx <- insertUtf8 mname
  sigStrIdx <- insertUtf8 (methodDescriptor m)
  ntIdx <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = mnameIdx,
          index_descr_cp = sigStrIdx,
          desc = ""
        }
  -- Insert the Methoderef with ref to the  current class
  _ <- traverseStmt mbody
  insertEntry
    CF.MethodRef_Info
      { tag_cp = CF.TagMethodRef,
        index_name_cp = cIdx,
        index_nameandtype_cp = ntIdx,
        desc = ""
      }

fieldDescriptor :: Type -> String
fieldDescriptor = convertTypeToString

insertField :: Int -> TAST.Field -> ConstantPoolState Int
insertField cIdx (TAST.Field _ _ ftype fname finit) = do
  fnameIdx <- insertUtf8 fname
  ftypeIdx <- insertUtf8 $ fieldDescriptor ftype

  ntIdx <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = fnameIdx,
          index_descr_cp = ftypeIdx,
          desc = ""
        }
  insertEntry
    CF.FieldRef_Info
      { tag_cp = CF.TagFieldRef,
        index_name_cp = cIdx,
        index_nameandtype_cp = ntIdx,
        desc = ""
      }

--- !! WIP !!
traverseStmt :: TAST.Stmt -> ConstantPoolState ()
traverseStmt (TAST.Block stmts) = mapM_ traverseStmt stmts
traverseStmt (TAST.Return maybeExpr) = maybe (return ()) traverseExpr maybeExpr
traverseStmt (TAST.While expr stmt) = traverseExpr expr >> traverseStmt stmt
traverseStmt (TAST.LocalVarDecl _ _ maybeExpr) = maybe (return ()) traverseExpr maybeExpr
traverseStmt (TAST.If expr stmt1 maybeStmt) = do
  traverseExpr expr
  traverseStmt stmt1
  maybe (return ()) traverseStmt maybeStmt
traverseStmt (TAST.StmtOrExprAsStmt stmtOrExpr) = trace "traverseStmtor Expr as stmt" $ traverseStmtOrExpr Nothing stmtOrExpr

traverseExpr :: TAST.Expr -> ConstantPoolState ()
traverseExpr (TAST.This _) = return () -- TODO How to resolve this
traverseExpr (TAST.Super _) = return () -- TODO How to resolve super
traverseExpr (TAST.Name _ name) = do
  _ <- insertClassInfo name -- TODO This needs to be a Classnam in a newer version
  return ()
traverseExpr (TAST.LocalVar ftype fname) = do
  return ()
traverseExpr (TAST.ClassRef ctype cname) = do
  _ <- insertClassInfo cname
  return ()
traverseExpr (TAST.FieldAccess ftype cname expr fname) = do
  _ <- traverseExpr expr
  _ <- insertClassInfo cname
  return ()
traverseExpr (TAST.Unary _ _ expr) = traverseExpr expr
traverseExpr (TAST.Binary _ _ expr1 expr2) = traverseExpr expr1 >> traverseExpr expr2
traverseExpr (TAST.Literal _ lit) = do
  lit_idx <- trace ("Debug: traversingLit") $ traverseLit lit
  return ()
traverseExpr (TAST.StmtOrExprAsExpr t stmtOrExpr) = traverseStmtOrExpr (Just t) stmtOrExpr

traverseStmtOrExpr :: Maybe Type -> TAST.StmtOrExpr -> ConstantPoolState ()
traverseStmtOrExpr _ (TAST.LocalAssign _ expr) = do
  trace "Debug Traverse Assign" $ traverseExpr expr
  maybe (return ()) traverseExpr maybeExpr -- TODO Need to be checked
traverseStmtOrExpr _ (TAST.FieldAssign maybeExpr cname name expr) = do
  trace "Debug Traverse Assign" $ traverseExpr expr
  maybe (return ()) traverseExpr maybeExpr -- TODO Need to be checked
traverseStmtOrExpr _ (TAST.New className exprs) = do
  insertClassInfo className
  mapM_ traverseExpr exprs

-- traverseStmtOrExpr (Just mreT) (TAST.MethodCall maybeExpr mname args) = do
--  cIdx <- (case maybeExpr of
--       Just expr ->
--         ( case typee expr of
--             (Core.Instance cname) -> insertClassInfo cname
--               return ()
--             (Core.Class cname) -> insertClassInfo cname
--               return ()
--             t -> error ("Cannot make a Method Call on Primitive Type:" ++ show t)
--         )
--       Nothing -> error "Method cannot be called without a class" -- TODO What to do here? In this case the Method should be called on this)
--       )
--   nIdx <- insertUtf8 mname
--   tIdx <- insertUtf8 (constructDescriptor mreT (map fst args))
--   ntIdx <-
--     insertEntry
--       CF.NameAndType_Info
--         { tag_cp = CF.TagNameAndType,
--           index_name_cp = nIdx,
--           index_descr_cp = tIdx,
--           desc = ""
--         }
--   insertEntry
--     CF.MethodRef_Info
--       { tag_cp = CF.TagMethodRef,
--         index_name_cp = cIdx,
--         index_nameandtype_cp = ntIdx,
--         desc = ""
--       }
--
-- mapM_ (traverseExpr . snd) args

traverseStmtOrExpr (Just mreT) (TAST.MethodCall expr cname mname args) = do
  cIdx <- insertClassInfo cname

  nIdx <- insertUtf8 mname
  tIdx <- insertUtf8 (constructDescriptor mreT (map fst args))
  ntIdx <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = nIdx,
          index_descr_cp = tIdx,
          desc = ""
        }
  insertEntry
    CF.MethodRef_Info
      { tag_cp = CF.TagMethodRef,
        index_name_cp = cIdx,
        index_nameandtype_cp = ntIdx,
        desc = ""
      }

  mapM_ (traverseExpr . snd) args

traverseLit :: TAST.Literal -> ConstantPoolState Int
traverseLit (TAST.IntLit i) = insertInt (fromIntegral i)
traverseLit (TAST.CharLit c) = insertInt (fromIntegral $ fromEnum c)
traverseLit (TAST.BoolLit b) = insertInt (if b then 1 else 0)
traverseLit TAST.Null = error "Null can not be pushed on the constant Pool"

{-
  Searching in Constant Pool
  -}
isMatchingUtf8 :: String -> CF.CP_Info -> Bool
isMatchingUtf8 str (Utf8_Info _ _ str1 _) = str == str1
isMatchingUtf8 _ _ = False

findEntry :: (CF.CP_Info -> Bool) -> ConstantPoolState [Int]
findEntry f = do
  cp <- get
  return $ map fst . filter (f . snd) $ cp

findUtf8 :: String -> ConstantPoolState [Int]
findUtf8 str = do
  findEntry (isMatchingUtf8 str)

isNameAndType :: Int -> Int -> CF.CP_Info -> Bool
isNameAndType nIdx tIdx (CF.NameAndType_Info _ pnIdx ptIdx _) = pnIdx == nIdx && ptIdx == tIdx
isNameAndType _ _ _ = False

getResult :: String -> [a] -> a
getResult msg [] = error msg
getResult _ (x : _) = x

isClass :: Int -> CF.CP_Info -> Bool
isClass cnameIdx (CF.Class_Info _ pcIdx _) = pcIdx == cnameIdx
isClass _ _ = False

findClass :: String -> ConstantPoolState Int
findClass str = do
  cname <- findUtf8 str

  cIdx <- findEntry (isClass (getResult "Could not Find Class with matching name" cname))
  return
    ( getResult
        "Could not find Matching Class"
        cIdx
    )

findMethodRef :: Int -> Int -> CF.CP_Info -> Bool
findMethodRef cIdx ntIdx (CF.MethodRef_Info _ pcIdx pntIdx _) = cIdx == pcIdx && ntIdx == pntIdx
findMethodRef _ _ _ = False

findMethodCall :: Core.Type -> TAST.StmtOrExpr -> ConstantPoolState Int
findMethodCall mRet m@(TAST.MethodCall maybeExpr mname args) = do
  let mdesc = constructDescriptor mRet (map fst args)
  tIdx <- findUtf8 mdesc
  nIdx <- findUtf8 mname
  ntIdx <-
    findEntry
      ( isNameAndType
          (getResult "Could not find Matching MethodName in the ConstantPool" nIdx)
          (getResult "Could not find Matching Descriptor in the ConstantPool" tIdx)
      )

  cIdx <- case maybeExpr of
    Just expr -> case typee expr of
      Core.Instance cname -> findClass cname
      Core.Class cname -> findClass cname
      t -> error $ "Cannot make a Method Call on Primitive Type: " ++ show t
    Nothing -> error "Method cannot be called without a class"
  mref <- findEntry (findMethodRef (getResult "Could not find NameAndType in the ConstantPool" ntIdx) cIdx)
  return $ getResult "Could not find MethodRef in the ConstantPool" mref
findMethodCall _ _ = error "It is not possible to find a Method entry for non method entries"

buildConstantPool :: TAST.Class -> ConstantPoolState ConstantPool
buildConstantPool c@(TAST.Class _ cname _ cfields cmethods) = do
  put []
  codeIdx <- trace "Test" $ insertUtf8 "Code"
  cIdx <- insertClass c
  mapM_ (insertField cIdx) cfields
  mapM_ (insertMethod cIdx) cmethods
  get
