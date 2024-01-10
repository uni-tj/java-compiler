-- Bytestring Imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Jvm.Data.ClassFormat as CF
import Types.TAST as TAST

strLen :: String -> Int
strLen = BS.length . UTF8.fromString

currIdx :: ConstantPool -> Int
currIdx [] = 0
currIdx cp = maximum $ map (\(i, _) -> i)

insertEntry :: CF.CP_Info -> ConstantPool -> (Int, ConstantPool)
insertEntry elem cp =
  ( idx,
    ( ( idx,
        elem
      )
        : cp
    )
  )
  where
    idx = currIdx

insertUtf8 :: String -> ConstantPool -> (Int, ConstantPool)
insertUtf8 str cp =
  insertEntry
    CF.CP_Info
      { tag_cp = CF.TagUtf8,
        tam_cp = strLen str,
        cad_cp = str,
        desc = ""
      }
    cp
  where
    idx = currIdx cp

insertUtf8Entries :: [String] -> ConstantPool -> ConstantPool
insertUtf8Entries [] cp = cp
insertUtf8Entries (x : xs) cp = insertUtf8Entries (snd (insertUtf8 x cp)) cp

insertClassInfo :: String -> ConstantPool -> ConstantPool
insertClassInfo str cp =
  snd $
    insertEntry
      ClassInfo
        { tag_cp = CF.TagClass,
          index_cp = utf8_idx, -- If needed the pacckage name needs to be added here
          desc = ""
        }
      cp
  where
    utf8_idx = fst $ insertUtf8 str

signatureString :: TAST.Method -> String
signatureString _ = ""

insertMethod :: TAST.Method -> ConstantPool -> ConstantPool
insertMethod m@(Method _ t _ mname mparams _) cp =
  -- Missing MMMethodref
  insertEntry
    CF.NameAndType_Info
      { tag_cp = CF.TagNameAndType,
        index_name_cp = mname,
        index_descr_cp = sigStr,
        desc = ""
      }
  where
    idx = currIdx cp
    r1 = insertUtf8Entry mname
    mname = fst r1
    r2 = insertUtf8Entry $ signatureString m
    cp' = snd r2
    sigStr = fst r2

-- insertMethods :: [TAST.Method] -> ConstantPool -> ConstantPool
-- insertMethods [] cp = cp
-- insertMethods (x : xs) cp  =

type ConstantPool = [(Int, CF.CP_Info)]

createUtf8 :: String -> ConstantPool
buildConstantPool :: Int TAST.Class -> ConstantPool
buildConstantPool idx (Class _ cname _ cfields cmethods) =
  insertMethods cmethods $ insertClassInfo name []
