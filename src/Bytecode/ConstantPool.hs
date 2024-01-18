module Bytecode.ConstantPool where

import Control.Monad.State
import Data.Bool (Bool (True))
import qualified Data.ByteString as BS
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Jvm.Data.ClassFormat as CF
import Types.Core
import Types.Core as Core
import Types.TAST as TAST

type ConstantPool = [(Int, CF.CP_Info)]

type ConstantPoolState = State ConstantPool

strLen :: String -> Int
strLen str = BS.length $ TE.encodeUtf8 $ T.pack str

currIdx :: ConstantPool -> Int
currIdx [] = 0
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
      return idx

insertUtf8 :: String -> ConstantPoolState Int
insertUtf8 str =
  insertEntry
    CF.Utf8_Info
      { tag_cp = CF.TagUtf8,
        tam_cp = strLen str,
        cad_cp = str,
        desc = ""
      }

insertClassInfo :: String -> ConstantPoolState Int
insertClassInfo str = do
  utf8Idx <- insertUtf8 str
  insertEntry
    CF.Class_Info
      { tag_cp = CF.TagClass,
        index_cp = utf8Idx,
        desc = ""
      }

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

methodDescriptor :: TAST.Method -> String
methodDescriptor (Method _ rtype _ _ mparams _) =
  "("
    ++ foldr (\x accu -> convertTypeToString (fst x) ++ accu) "" mparams
    ++ ")"
    ++ convertTypeToString rtype

insertMethod :: Int -> TAST.Method -> ConstantPoolState Int
insertMethod cIdx m@(Method _ _ _ mname _ _) = do
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
traverseStmt (TAST.StmtOrExprAsStmt stmtOrExpr) = traverseStmtOrExpr stmtOrExpr

traverseExpr :: TAST.Expr -> ConstantPoolState ()
traverseExpr (TAST.This _) = return ()
traverseExpr (TAST.Super _) = return ()
traverseExpr (TAST.Name _ name) = insertUtf8 name
traverseExpr (TAST.FieldAccess _ expr fieldName) = traverseExpr expr >> insertUtf8 fieldName
traverseExpr (TAST.Unary _ _ expr) = traverseExpr expr
traverseExpr (TAST.Binary _ _ expr1 expr2) = traverseExpr expr1 >> traverseExpr expr2
traverseExpr (TAST.Literal _ lit) = traverseLit lit
traverseExpr (TAST.StmtOrExprAsExpr _ stmtOrExpr) = traverseStmtOrExpr stmtOrExpr

traverseStmtOrExpr :: TAST.StmtOrExpr -> ConstantPoolState ()
traverseStmtOrExpr (TAST.Assign maybeExpr name expr) = do
  maybe (return ()) traverseExpr maybeExpr
  insertUtf8 name
  traverseExpr expr
traverseStmtOrExpr (TAST.New className exprs) = do
  insertUtf8 className
  mapM_ traverseExpr exprs
traverseStmtOrExpr (TAST.MethodCall maybeExpr methodName args) = do
  maybe (return ()) traverseExpr maybeExpr
  insertUtf8 methodName
  mapM_ (traverseExpr . snd) args

traverseLit :: TAST.Literal -> ConstantPoolState ()
traverseLit = return ()

-- !! WIP

buildConstantPool :: TAST.Class -> ConstantPoolState ConstantPool
buildConstantPool (TAST.Class _ cname _ cfields cmethods) = do
  codeIdx <- insertUtf8 "Code"
  cIdx <- insertClassInfo cname
  mapM_ (insertField cIdx) cfields
  mapM_ (insertMethod cIdx) cmethods
  get
