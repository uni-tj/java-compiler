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
convertTypeToString (Core.Class cname) = "L" ++ cname ++ ";"
convertTypeToString (Core.StaticClass cname) = "L" ++ cname ++ ";"
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
insertField cIdx (TAST.Field ftype fname finit) = do
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

buildConstantPool :: TAST.Class -> ConstantPoolState ConstantPool
buildConstantPool (TAST.Class _ cname _ cfields cmethods) = do
  codeIdx <- insertUtf8 "Code"
  cIdx <- insertClassInfo cname
  mapM_ (insertField cIdx) cfields
  mapM_ (insertMethod cIdx) cmethods
  get
