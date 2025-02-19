module ConstantPool.ConstantPool where

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

type MICP = (CF.Field_Infos, [(Int, CF.CP_Info)])

type ConstantPoolState = State MICP

strLen :: String -> Int
strLen str = BS.length $ TE.encodeUtf8 $ T.pack str

debugStr :: (Show a) => ConstantPool -> [(String, a)] -> String
debugStr cp xs = foldl (\acc (d, v) -> acc ++ "\n " ++ d ++ ": " ++ show v) ("ConstantPool: " ++ (unlines . map show) cp) xs

currIdx :: ConstantPool -> Int
currIdx [] = 0
currIdx cp = maximum $ map fst cp

insertEntry :: CF.CP_Info -> ConstantPoolState Int
insertEntry elem = do
  (mi, cp) <- get
  case find ((==) elem . snd) cp of
    Just idx -> return $ fst idx
    Nothing -> do
      let idx = currIdx cp
      let newCp = cp ++ [(idx + 1, elem)]
      put (mi, newCp)
      return (idx + 1)

insertFieldInfo :: CF.Field_Info -> ConstantPoolState ()
insertFieldInfo f = do
  (mi, cp) <- get
  put (mi ++ [f], cp)

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
insertClass (TAST.Class _ cname (Just cextends) _ _ _) = do
  insertClassInfo cextends
  insertClassInfo cname
insertClass (TAST.Class _ cname Nothing _ _ _) = do
  insertClassInfo "java/lang/Object"
  insertClassInfo cname

insertConstructor :: Int -> TAST.Constructor -> ConstantPoolState Int
insertConstructor cIdx (TAST.Constructor _ params body) = do
  nIdx <- insertUtf8 "<init>"
  tIdx <- insertUtf8 $ constructDescriptor Core.Void (map fst params)
  ntIdx <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = nIdx,
          index_descr_cp = tIdx,
          desc = ""
        }

  traverseStmt body
  insertEntry
    CF.MethodRef_Info
      { tag_cp = CF.TagMethodRef,
        index_name_cp = cIdx,
        index_nameandtype_cp = ntIdx,
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
insertField cIdx (TAST.Field facces fstatic ftype fname finit) = do
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

  let staticFlag = [8 | fstatic]
      accessFlag = case facces of
        Public -> 1
        Package -> 0
        Protected -> 4
        Private -> 2
      aFlag = accessFlag : staticFlag
  insertFieldInfo
    CF.Field_Info
      { tam_fi = 0,
        index_name_fi = fnameIdx,
        index_descr_fi = ftypeIdx,
        array_attr_fi = [],
        af_fi = CF.AccessFlags aFlag
      }
  insertEntry
    CF.FieldRef_Info
      { tag_cp = CF.TagFieldRef,
        index_name_cp = cIdx,
        index_nameandtype_cp = ntIdx,
        desc = ""
      }

traverseStmt :: TAST.Stmt -> ConstantPoolState ()
traverseStmt (TAST.Block stmts) = mapM_ traverseStmt stmts
traverseStmt (TAST.Return maybeExpr) = maybe (return ()) traverseExpr maybeExpr
traverseStmt (TAST.While expr stmt) = traverseExpr expr >> traverseStmt stmt
traverseStmt (TAST.LocalVarDecl _ _ maybeExpr) = maybe (return ()) traverseExpr maybeExpr
traverseStmt (TAST.If expr stmt1 maybeStmt) = do
  traverseExpr expr
  traverseStmt stmt1
  maybe (return ()) traverseStmt maybeStmt
traverseStmt (TAST.ThisCall cname args) = do
  cIdx <- insertClassInfo cname
  nIdx <- insertUtf8 "<init>"
  tIdx <- insertUtf8 $ constructDescriptor Core.Void (map fst args)
  ntIdx <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = nIdx,
          index_descr_cp = tIdx,
          desc = ""
        }
  mapM_ (traverseExpr . snd) args
  insertEntry
    CF.MethodRef_Info
      { tag_cp = CF.TagMethodRef,
        index_name_cp = cIdx,
        index_nameandtype_cp = ntIdx,
        desc = ""
      }
  return ()
traverseStmt (TAST.SuperCall cname args) = do
  cIdx <- insertClassInfo cname
  nIdx <- insertUtf8 "<init>"
  tIdx <- insertUtf8 $ constructDescriptor Core.Void (map fst args)
  ntIdx <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = nIdx,
          index_descr_cp = tIdx,
          desc = ""
        }

  mapM_ (traverseExpr . snd) args
  insertEntry
    CF.MethodRef_Info
      { tag_cp = CF.TagMethodRef,
        index_name_cp = cIdx,
        index_nameandtype_cp = ntIdx,
        desc = ""
      }
  return ()
traverseStmt (TAST.StmtOrExprAsStmt stmtOrExpr) = traverseStmtOrExpr stmtOrExpr

traverseExpr :: TAST.Expr -> ConstantPoolState ()
traverseExpr (TAST.This _) = return ()
traverseExpr (TAST.Super _) = return ()
traverseExpr (TAST.LocalVar ftype fname) = do
  return ()
traverseExpr (TAST.ClassRef ctype cname) = do
  _ <- insertClassInfo cname
  return ()
traverseExpr (TAST.FieldAccess ftype expr cname isStatic fname) = do
  cIdx <- insertClassInfo cname
  n <- insertUtf8 fname
  t <- insertUtf8 $ fieldDescriptor ftype
  nt <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = n,
          index_descr_cp = t,
          desc = ""
        }
  _ <-
    insertEntry
      CF.FieldRef_Info
        { tag_cp = CF.TagFieldRef,
          index_name_cp = cIdx,
          index_nameandtype_cp = nt,
          desc = ""
        }
  _ <- traverseExpr expr
  return ()
traverseExpr (TAST.Unary _ _ expr) = traverseExpr expr
traverseExpr (TAST.Binary _ _ expr1 expr2) = traverseExpr expr1 >> traverseExpr expr2
traverseExpr (TAST.Literal _ lit) = do
  lit_idx <- traverseLit lit
  return ()
traverseExpr (TAST.StmtOrExprAsExpr stmtOrExpr) = traverseStmtOrExpr stmtOrExpr

traverseStmtOrExpr :: TAST.StmtOrExpr -> ConstantPoolState ()
traverseStmtOrExpr (TAST.LocalAssign _ _ expr) = do
  traverseExpr expr
  (return ())
traverseStmtOrExpr (TAST.FieldAssign ftype maybeExpr cname _ fname expr) = do
  cIdx <- insertClassInfo cname
  n <- insertUtf8 fname
  t <- insertUtf8 $ fieldDescriptor ftype
  nt <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = n,
          index_descr_cp = t,
          desc = ""
        }
  i <-
    insertEntry
      CF.FieldRef_Info
        { tag_cp = CF.TagFieldRef,
          index_name_cp = cIdx,
          index_nameandtype_cp = nt,
          desc = ""
        }
  traverseExpr expr
  traverseExpr maybeExpr
  (return ())
traverseStmtOrExpr (TAST.New _ className exprs) = do
  cIdx <- insertClassInfo className
  nIdx <- insertUtf8 "<init>"
  tIdx <- insertUtf8 $ constructDescriptor Core.Void (map fst exprs)
  ntIdx <-
    insertEntry
      CF.NameAndType_Info
        { tag_cp = CF.TagNameAndType,
          index_name_cp = nIdx,
          index_descr_cp = tIdx,
          desc = ""
        }

  fref <-
    insertEntry
      CF.MethodRef_Info
        { tag_cp = CF.TagMethodRef,
          index_name_cp = cIdx,
          index_nameandtype_cp = ntIdx,
          desc = ""
        }
  mapM_ (traverseExpr . snd) exprs
traverseStmtOrExpr (TAST.MethodCall mreT expr cname isStatic mname args) = do
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
  traverseExpr expr
  mapM_ (traverseExpr . snd) args

traverseLit :: TAST.Literal -> ConstantPoolState Int
traverseLit (TAST.IntLit i) = insertInt (fromIntegral i)
traverseLit (TAST.CharLit c) = insertInt (fromIntegral $ fromEnum c)
traverseLit (TAST.BoolLit b) = insertInt (if b then 1 else 0)
traverseLit TAST.Null = do
  return (-9999999)

{-
  Searching in  the Constant Pool Constant Pool
-}
isMatchingUtf8 :: String -> CF.CP_Info -> Bool
isMatchingUtf8 str (Utf8_Info _ _ str1 _) = str == str1
isMatchingUtf8 _ _ = False

findEntry :: (CF.CP_Info -> Bool) -> ConstantPoolState [Int]
findEntry f = do
  (_, cp) <- get
  return $ map fst . filter (f . snd) $ cp

findUtf8' :: String -> ConstantPoolState [Int]
findUtf8' str = do
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

findClass' :: String -> ConstantPoolState Int
findClass' str = do
  (mi, cp) <- get
  cname <- findUtf8' str

  cIdx <- findEntry (isClass (getResult ("Could not Find UTF8 Class with matching name" ++ debugStr cp [("cname", str)]) cname))
  return
    ( getResult
        "Could not find Matching Class"
        cIdx
    )

findMethodRef :: Int -> Int -> CF.CP_Info -> Bool
findMethodRef cIdx ntIdx (CF.MethodRef_Info _ pcIdx pntIdx _) = cIdx == pcIdx && ntIdx == pntIdx
findMethodRef _ _ _ = False

findMethodCall' :: TAST.StmtOrExpr -> ConstantPoolState Int
findMethodCall' m@(TAST.MethodCall mRet maybeExpr cname isStatic mname args) = do
  let mdesc = constructDescriptor mRet (map fst args)
  (mi, cp) <- get
  tIdx <- findUtf8' mdesc
  nIdx <- findUtf8' mname
  ntIdx <-
    findEntry
      ( isNameAndType
          (getResult ("Could not find Matching MethodName in the ConstantPool" ++ debugStr cp [("MethodCall", show m), ("MethodName", mname)]) nIdx)
          (getResult ("Could not find Matching Descriptor in the ConstantPool" ++ debugStr cp [("MethodCall", show m), ("Descriptor", mdesc)]) tIdx)
      )

  cIdx <- findClass' cname
  mref <- findEntry (findMethodRef cIdx (getResult ("Could not find NameAndType in the ConstantPool" ++ debugStr cp [("MethodCall", show m), ("NameAndType Index", show ntIdx)]) ntIdx))

  return $
    getResult
      ( "Could not find MethodRef in the ConstantPool"
          ++ debugStr
            cp
            [ ("MethodCall", show m),
              ("Descriptor Index", show tIdx),
              ("MethodName Index", show nIdx),
              ("NameAndType Index", show ntIdx),
              ("Class Index", show cIdx)
            ]
      )
      mref
findMethodCall' _ = error "It is not possible to find a Method entry for non method entries"

findFieldRef :: Int -> Int -> CF.CP_Info -> Bool
findFieldRef cIdx ntIdx (CF.FieldRef_Info _ pcIdx pntIdx _) = cIdx == pcIdx && ntIdx == pntIdx
findFieldRef _ _ _ = False

findField' :: String -> String -> Type -> ConstantPoolState Int
findField' cname fname ftype = do
  (mi, cp) <- get
  let fdesc = fieldDescriptor ftype
  tIdx <- findUtf8' fdesc
  nIdx <- findUtf8' fname
  ntIdx <-
    findEntry
      ( isNameAndType
          (getResult ("Could not find Matching MethodName in the ConstantPool" ++ debugStr cp [("FieldName", fname)]) nIdx)
          (getResult ("Could not find Matching Descriptor in the ConstantPool" ++ debugStr cp [("FieldDescriptor", fdesc)]) tIdx)
      )

  cIdx <- findClass' cname
  mref <- findEntry (findFieldRef cIdx (getResult ("Could not find NameAndType in the ConstantPool" ++ debugStr cp [("NameAndType Index", show ntIdx)]) ntIdx))
  return $ getResult ("Could not find FieldRef in the ConstantPool with cname: " ++ cname ++ debugStr cp [("Class Index", show cIdx), ("NameAndType Index", show (getResult "" ntIdx)), ("Mref", show mref), ("fname", fname), ("cname ", cname), ("ftype", show ftype)]) mref

findConstructor' :: String -> [Core.Type] -> ConstantPoolState Int
findConstructor' cname params = do
  (mi, cp) <- get
  let mdesc = constructDescriptor Core.Void params
  tIdx <- findUtf8' mdesc
  nIdx <- findUtf8' "<init>"
  ntIdx <-
    findEntry
      ( isNameAndType
          (getResult ("Could not find Matching MethodName in the ConstantPool" ++ debugStr cp [("MethodName", "<init>")]) nIdx)
          (getResult ("Could not find Matching Descriptor in the ConstantPool" ++ debugStr cp [("Descriptor", mdesc)]) tIdx)
      )

  cIdx <- findClass' cname
  mref <- findEntry (findMethodRef cIdx (getResult ("Could not find NameAndType in the ConstantPool" ++ debugStr cp [("NameAndType Index", show ntIdx)]) ntIdx))
  return $ getResult ("Could not find Constructor MethodRef in the ConstantPool" ++ debugStr cp [("Class Index", show cIdx), ("NameAndType Index", show ntIdx)]) mref

findLiteral' :: TAST.Literal -> ConstantPoolState Int
findLiteral' lit = do
  (mi, cp) <- get
  i <- traverseLit lit
  return $ getResult ("Could not find Literal in the ConstantPool" ++ debugStr cp [("Literal", show lit)]) [i]

findName' :: String -> ConstantPoolState Int
findName' str = do
  (mi, cp) <- get
  i <- findUtf8' str
  return $ getResult ("Could not find Name in the ConstantPool" ++ debugStr cp [("Name", str)]) i

findDesc' :: TAST.Method -> ConstantPoolState Int
findDesc' m = do
  (mi, cp) <- get
  i <- findUtf8' $ methodDescriptor m
  return $ getResult ("Could not find Descriptor in the ConstantPool" ++ debugStr cp [("Method", show m)]) i

findAnyDesc' :: Core.Type -> [Core.Type] -> ConstantPoolState Int
findAnyDesc' mRet mPar = do
  (mi, cp) <- get
  let desc = constructDescriptor mRet mPar
  i <- findUtf8' desc
  return $ getResult ("Could not find Descriptor in the ConstantPool" ++ debugStr cp [("ReturnType", show mRet), ("Parameters", show mPar)]) i

data SearchFunctions = Func
  { findLiteral :: TAST.Literal -> Int,
    findMethodCall :: TAST.StmtOrExpr -> Int,
    findField :: String -> String -> Core.Type -> Int,
    findName :: String -> Int,
    findDesc :: TAST.Method -> Int,
    findAnyDesc :: Core.Type -> [Core.Type] -> Int,
    findClass :: String -> Int,
    findUtf8 :: String -> [Int],
    findConstructor :: String -> [Core.Type] -> Int
  }

buildConstantPool :: TAST.Class -> ConstantPoolState ()
buildConstantPool c@(TAST.Class _ cname _ cfields cmethods cconstructors) = do
  codeIdx <- insertUtf8 "Code"
  cIdx <- insertClass c
  mapM_ (insertConstructor cIdx) cconstructors
  mapM_ (insertField cIdx) cfields
  mapM_ (insertMethod cIdx) cmethods
  return ()

wrapConstantPool' :: TAST.Class -> ConstantPoolState (SearchFunctions)
wrapConstantPool' c@(TAST.Class _ cname _ cfields cmethods cconstructors) = do
  put ([], [])
  buildConstantPool c

  cp <- get

  let searchFunc =
        Func
          { findLiteral = (\x -> fst (runState (findLiteral' x) cp)),
            findMethodCall = (\x -> fst (runState (findMethodCall' x) cp)),
            findField = (\x y z -> fst (runState (findField' x y z) cp)),
            findName = (\x -> fst (runState (findName' x) cp)),
            findDesc = (\x -> fst (runState (findDesc' x) cp)),
            findAnyDesc = (\x y -> fst (runState (findAnyDesc' x y) cp)),
            findClass = (\x -> fst (runState (findClass' x) cp)),
            findUtf8 = (\x -> fst (runState (findUtf8' x) cp)),
            findConstructor = (\x y -> fst (runState (findConstructor' x y) cp))
          }

  return searchFunc

wrapConstantPool :: TAST.Class -> (SearchFunctions, MICP)
wrapConstantPool c = runState (wrapConstantPool' c) ([], [])
