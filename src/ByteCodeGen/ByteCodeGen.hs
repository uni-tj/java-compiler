{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use list comprehension" #-}

module ByteCodeGen.ByteCodeGen where

import Data.List (findIndex)
import Data.Bits

import ByteCodeGen.Jvm.Data.ClassFormat
import Types.TAST
import Types.Core

-- import ByteCodeGen.JavaTestFiles.SimpleForLoop.SimpleForLoopTAST (testAst)
-- import ByteCodeGen.JavaTestFiles.GGT.GgtTAST (ggtErw)
import ByteCodeGen.JavaTestFiles.Classes.ClassesTAST (classes)

-- Question
-- 1. What to do with chars?
-- Nur Print sonst Int
-- 2. How to calc max stack?
-- Den Baum durchgehen -> Lieber zuviel
-- 3. Doku
-- Wie kann man den Code einzeln anschauen
-- Tests schreiben und zeigen wie man Tests ausführen
-- Was hat man sich gedacht bei z.B. max stack gedacht
-- Pro Person ca. 1 Seite

-- Todo:
-- Query CP
-- Load constants for cp
-- Remove negativ numbers
-- Add putstatic 
-- Add getstatic

codeGen :: [Method_Info]
codeGen = do
  let methodObjects = concatMap createClassFile classes

  methodObjects


createClassFile :: Class -> [Method_Info]
createClassFile classInfo = do
  let cName = getNameFromClass classInfo

  let methods = getMethodsFromClass classInfo
  let methodObjects = map (\method -> createMethodObject (cName, method)) methods

  methodObjects

  -- let classFileNew = ClassFile {
  --   magic = Magic,
  --   minver = MinorVersion {numMinVer = 0},
  --   maxver = MajorVersion {numMaxVer = 49},
  --   count_cp = 12,
  --   array_cp =
  --     [ MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 2, index_nameandtype_cp = 3, desc = ""},
  --       Class_Info {tag_cp = TagClass, index_cp = 4, desc = ""},
  --       NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 5, index_descr_cp = 6, desc = ""},
  --       Utf8_Info {tag_cp = TagUtf8, tam_cp = 16, cad_cp = "java/lang/Object", desc = ""},
  --       Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "<init>", desc = ""},
  --       Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "()V", desc = ""},
  --       Class_Info {tag_cp = TagClass, index_cp = 8, desc = ""},
  --       Utf8_Info {tag_cp = TagUtf8, tam_cp = 18, cad_cp = "simpleforLoopClass", desc = ""},
  --       Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "Code", desc = ""},
  --       Utf8_Info {tag_cp = TagUtf8, tam_cp = 13, cad_cp = "simpleForLoop", desc = ""},
  --       Utf8_Info {tag_cp = TagUtf8, tam_cp = 13, cad_cp = "StackMapTable", desc = ""}
  --     ],
  --   -- TODO:
  --   acfg = AccessFlags [1, 32],
  --   this = ThisClass {index_th = 8},
  --   super = SuperClass {index_sp = 2},
  --   --
  --   count_interfaces = 0,
  --   array_interfaces = [],
  --   count_fields = 0,
  --   array_fields = [],
  --   -- Todo:
  --   count_methods = 1,
  --   --
  --   array_methods =
  --     [
  --       -- Method_Info
  --       --   { -- af_mi = AccessFlags [],
  --       --     -- index_name_mi = ,
  --       --     -- index_descr_mi = ,
  --       --     tam_mi = 1,
  --       --     array_attr_mi =
  --       --       [ AttributeCode
  --       --           { -- index_name_attr = , -- Code - Type from const pool
  --       --             -- tam_len_attr = , -- tam_code_attr + 12
  --       --             -- len_stack_attr = ,
  --       --             -- len_local_attr = , -- parameter + this if not static
  --       --             -- tam_code_attr = , -- code length -> len(array_code_attr)
  --       --             array_code_attr =
  --       --               [],
  --       --             tam_ex_attr = 0,
  --       --             array_ex_attr = [],
  --       --             tam_atrr_attr = 0,
  --       --             array_attr_attr = []
  --       --           }
  --       --       ]
  --       --   }
  --     ],
  --   count_attributes = 0,
  --   array_attributes = []
  -- }

  -- print "End"

createMethodObject:: (String, Method) -> Method_Info
createMethodObject (cName, Method methodAccess mtype methodStatic mName methodParams methodBody) = do
  let accessFlags = getAccessFlagsForMethod (methodAccess, methodStatic)
  let indexMethodName = 0 -- Todo: Query CP
  let indexDescr = 0 -- Todo: Query CP
  let methodConstructor = cName == mName
  
  let attributeCode = createAttributeCode (methodConstructor, methodStatic, methodParams, methodBody)
  
  Method_Info {
    af_mi = AccessFlags accessFlags,
    index_name_mi = indexMethodName,
    index_descr_mi = indexDescr,
    tam_mi = 1,
    array_attr_mi =
      [
        attributeCode
      ]
  }

createAttributeCode :: (Bool, Bool, [(Type, LocalName)], Stmt) -> Attribute_Info
createAttributeCode (methodConstructor, methodStatic, params, body) = do
  let indexNameAttr = 1 -- Query CP for Code

  let localVarArr = localVarGen (methodStatic, params, body)
  let methodCode = codeGenStmt (body, localVarArr)

  -- Todo: Query CP for MethodRef_Info -> java/lang/Object ... -> Is this always this case?
  -- Can be deleted?
  let constructorCode = if methodConstructor then [42, 183,333,333] else [] 

  AttributeCode {
    index_name_attr = indexNameAttr, -- Code
    tam_len_attr = length methodCode + 12, -- All bytes below
    len_stack_attr = 0, -- Todo: Calc max stack
    len_local_attr = length params + if methodStatic then 0 else 1,
    tam_code_attr = length methodCode,
    array_code_attr = constructorCode ++ methodCode,
    tam_ex_attr = 0,
    array_ex_attr = [],
    tam_atrr_attr = 0,
    array_attr_attr = []
  }

-- Function to get all local variable and store the number (index in array) of the local variable -> in order to find them later
-- Todo: "This" must be index 0, than function paras -> class.method(a,b) -> this=0 a=1 b=2, ... (localvar decs)
type LocalVarArrType = [(String, Types.Core.Type)]
localVarGen :: (Bool, [(Type, LocalName)], Stmt) -> LocalVarArrType
localVarGen (methodStatic, params, body) = do
  localVarGenMethodStatic methodStatic ++ localVarGenParams params ++ localVarGenBody body

localVarGenMethodStatic:: Bool -> LocalVarArrType
localVarGenMethodStatic methodStatic = if methodStatic then [] else [("This", Instance "This")] -- Todo: Is this right?

localVarGenParams:: [(Type, LocalName)] -> LocalVarArrType
localVarGenParams = map transform
  where
    transform (t, name) = (name, t)

localVarGenBody :: Stmt -> LocalVarArrType
localVarGenBody (Block blocks) = concatMap localVarGenBody blocks
localVarGenBody (Return _) = []
localVarGenBody (While _ stmt) = localVarGenBody stmt
localVarGenBody (LocalVarDecl localType localName _) = [(localName, localType)]
localVarGenBody (If _ stmt mStmt) = localVarGenBody stmt ++ maybe [] localVarGenBody mStmt
localVarGenBody (StmtOrExprAsStmt _) = []

codeGenStmt :: (Stmt, LocalVarArrType) -> [Int]
codeGenStmt (Block blocks, localVarArr) = concatMap (\block -> codeGenStmt (block, localVarArr)) blocks

codeGenStmt (Return mexpr, localVarArr) = do
  case mexpr of
    Just expr -> do
      let (exprCode, t) = codeGenExpr (expr, localVarArr)
      case t of
        Types.Core.Int -> exprCode ++ [172] -- ireturn
        Types.Core.Bool -> exprCode ++ [172] -- ireturn
        Types.Core.Char -> exprCode ++ [172] -- ireturn
        Types.Core.Instance instanceName -> exprCode ++ [176] -- areturn Todo: Is this right?
        Types.Core.Class className -> error "Class can not be returned"
        _ -> []
    Nothing -> []

codeGenStmt (While expr stmt, localVarArr) = do
  -- expr(ending up true/false -> 1 or 0 on stack) --- 153, 0, len(stmt) --- stmt
  let (codeExpr, t) = codeGenExpr (expr, localVarArr)
  let codeStmt = codeGenStmt (stmt, localVarArr)
  let codeWhile = 153 : splitLenInTwoBytes (length codeStmt + 3) -- 153 -> if value is 0, branch -- 3 for offset
  codeExpr ++ codeWhile ++ codeStmt

codeGenStmt (LocalVarDecl varType localName mexpr, localVarArr) = do
  let i = findIndex ((== localName) . fst) localVarArr
  case mexpr of
    Just expr ->
      case i of
        Just index ->
          case varType of
            Types.Core.Int -> do
              let (codeExpr, t) = codeGenExpr (expr, localVarArr)
              codeExpr ++ [196, 54] ++ splitLenInTwoBytes index -- 196 -> wide 54 -> istore + numer of local var
            Types.Core.Bool -> do
              let (codeExpr, t) = codeGenExpr (expr, localVarArr)
              codeExpr ++ [196, 54] ++ splitLenInTwoBytes index
            Types.Core.Char -> do
              let (codeExpr, t) = codeGenExpr (expr, localVarArr)
              codeExpr ++ [196, 54] ++ splitLenInTwoBytes index
            Types.Core.Instance className -> do
              let (codeExpr, t) = codeGenExpr (expr, localVarArr)
              codeExpr ++ [196, 58] ++ splitLenInTwoBytes index  -- Todo: 58 astore?
            Types.Core.Class className -> error "Class can not be assigt to a var"
            _ -> []
        Nothing -> []
    Nothing -> []

codeGenStmt (If expr stmt mStmt, localVarArr) =
  let (codeExpr, t) = codeGenExpr (expr, localVarArr)
      codeStmt = codeGenStmt (stmt, localVarArr)
      codeElseStmt = case mStmt of
        Just elseStmt -> codeGenStmt (elseStmt, localVarArr)
        Nothing -> []
  in if length codeElseStmt > 0 then
    -- +3 because of added goto --- [167] ++ splitLenInTwoBytes (length codeElseStmt) -> goto to overjump else when if is taken
    codeExpr ++ [153] ++ splitLenInTwoBytes (length codeStmt + 3 + 3) ++ codeStmt ++ [167] ++ splitLenInTwoBytes (length codeElseStmt + 3) ++ codeElseStmt
  else
    codeExpr ++ [153] ++ splitLenInTwoBytes (length codeStmt + 3) ++ codeStmt

codeGenStmt (StmtOrExprAsStmt stmtOrExpr, localVarArr) = codeGenStmtOrExpr (stmtOrExpr, localVarArr)

codeGenExpr :: (Expr, LocalVarArrType) -> ([Int], Type)
codeGenExpr (This thisType, localVarArr) = ([42], thisType) -- Aload_0

codeGenExpr (Super superName, localVarArr) = ([], NullType) -- Todo: Ask

codeGenExpr (LocalVar localVarType localVarName, localVarArr) = do
  let i = findIndex ((== localVarName) . fst) localVarArr
  case i of
    Just index ->
      case localVarType of
        Types.Core.Int -> ([196, 21] ++ splitLenInTwoBytes index, Types.Core.Int)  -- wide, iload
        Types.Core.Bool -> ([196, 21] ++ splitLenInTwoBytes index, Types.Core.Int)  -- wide, iload
        Types.Core.Char -> ([196, 21] ++ splitLenInTwoBytes index, Types.Core.Int ) -- wide, iload
        Types.Core.Instance instantName -> ([196, 25] ++ splitLenInTwoBytes index, Instance instantName) -- wide, aload
        Types.Core.Class className -> error "Var can not have value of type class"
        _ -> ([], localVarType)
    Nothing -> ([], localVarType)

codeGenExpr (ClassRef cType cName, localVarArr) = ([], cType)

-- Todo: Add getstatic
codeGenExpr (FieldAccess fieldType expr classname static fieldName, localVarArr) = do
  let (exprCode, t) = codeGenExpr (expr, localVarArr)
  let getFieldCode = 180 : [333, 333] -- Todo: Query constant pool

  (exprCode ++ getFieldCode, fieldType)

codeGenExpr (Unary unaryType unOparator expr, localVarArr) = do
  let (codeExpr, t) = codeGenExpr (expr, localVarArr)
  let codeUnary = codeExpr ++ codeGenUnOparator unOparator
  (codeUnary, unaryType)

codeGenExpr (Binary typeBinary binOperator expr0 expr1, localVarArr) = do
  let (codeExpr0, t) = codeGenExpr (expr0, localVarArr)
  let (codeExpr1, t) = codeGenExpr (expr1, localVarArr)
  let codeBinOperator = codeGenBinOperator binOperator
  (codeExpr0 ++ codeExpr1 ++ codeBinOperator, typeBinary)

codeGenExpr (Literal literalType literal, localVarArr) = do
  case literalType of
    Types.Core.Int -> (17 : codeGenLiteral literal, literalType)
    Types.Core.Bool -> (codeGenLiteral literal, literalType)
    Types.Core.Char -> (codeGenLiteral literal, literalType)
    _ -> ([], NullType)

codeGenExpr (StmtOrExprAsExpr stmtOrExprAsExprType stmtOrExpr, localVarArr) = (codeGenStmtOrExpr (stmtOrExpr, localVarArr), stmtOrExprAsExprType)

codeGenStmtOrExpr :: (StmtOrExpr, LocalVarArrType) -> [Int]

codeGenStmtOrExpr (LocalAssign varName expr, localVarArr) = do
  let i = findIndex ((== varName) . fst) localVarArr
  case i of
    Just index ->
      let codeStore = case t of
            Types.Core.Int -> [196, 54]
            Types.Core.Bool -> [196, 54]
            Types.Core.Char -> [196, 54]
            Types.Core.Instance instanceName -> [196, 58]
            Types.Core.Class className -> error "Can assign Class to var"
            _ -> []
      in exprCode ++ codeStore ++ splitLenInTwoBytes index
    Nothing -> []

-- Todo: Add putstatic 
codeGenStmtOrExpr (FieldAssign tagetExpr className static fieldName valueExpr, localVarArr) = do
  let (tagetExprCode, t) = codeGenExpr (tagetExpr, localVarArr)
  let (valueExprCode, t) = codeGenExpr (valueExpr, localVarArr)

  tagetExprCode ++ valueExprCode ++ [181,333,333] -- Todo: Query CP of FieldRef_Info

codeGenStmtOrExpr (New className exprs, localVarArr) = do
  -- 89 = Dup -- Todo: Query CP for Class_Info/MethodRef_Info -> Call constructor
  let newCode = [187, 333, 333] ++ [89] ++ [183, 333, 333] 
  let paraCode = concatMap (fst . (\(typ, exprPara) -> codeGenExpr (exprPara, localVarArr))) exprs
  
  paraCode ++ newCode

codeGenStmtOrExpr (MethodCall expr className static methodName paras, localVarArr) = do
  -- Todo: What if static methode of own class is called?
  let (codeExpr, tExpr) = codeGenExpr (expr, localVarArr)
  
  let codeParas = concatMap (fst . (\(typ, exprPara) -> codeGenExpr (exprPara, localVarArr))) paras

  let codeInvoke = case tExpr of
        Types.Core.Instance instanceName -> [182, 333, 333] -- Todo: Query CP for MethodRef_Info
        Types.Core.Class className -> [184, 333, 333] -- Todo: Query CP for MethodRef_Info
        _ -> []
  
  codeExpr ++ codeParas ++ codeInvoke

codeGenBinOperator:: BinOperator -> [Int]
codeGenBinOperator Add = [96]
codeGenBinOperator Sub = [100]
codeGenBinOperator Mul = [104]
codeGenBinOperator Div = [108]
codeGenBinOperator Mod = [112] -- logical int remainder
codeGenBinOperator LAnd = [126]
codeGenBinOperator LOr = [128]
codeGenBinOperator Types.Core.LT = [161, 0, 7, 3, 167, 0, 4, 4] -- IF_ICMPLT label, ICONST_0, GOTO end, label:, ICONST_1
codeGenBinOperator LTE = [163, 0, 7, 4, 167, 0, 4, 3] -- IF_ICMPGT label, ICONST_1, GOTO end, label:, ICONST_0
codeGenBinOperator Types.Core.GT = [163, 0, 7, 3, 167, 0, 4, 4] -- IF_ICMPGT label, ICONST_0, GOTO end, label:, ICONST_1
codeGenBinOperator GTE = [161, 0, 7, 4, 167, 0, 4, 3] -- IF_ICMPLT label, ICONST_1, GOTO end, label:, ICONST_0
codeGenBinOperator Types.Core.EQ = [100, 153, 0, 7, 3, 167, 0, 4, 4] -- iSub, ifeq label, ICONST_0, GOTO end, label:, ICONST_1
codeGenBinOperator NEQ = [100, 154, 0, 7, 3, 167, 0, 4, 4] -- iSub, ifne label, ICONST_0, GOTO end, label:, ICONST_1

codeGenUnOparator:: UnOparator -> [Int]
codeGenUnOparator Plus = [] -- -> Do nothing
codeGenUnOparator Minus = [116] -- Todo: -i? -> Use INEG
codeGenUnOparator LNot = [4, 130] -- Expr must already be on the stack - push 1 -- oxr Expr(1/0) and 1 -> Bit toggel

codeGenLiteral :: Literal -> [Int]
codeGenLiteral (IntLit integer) = splitLenInTwoBytes (fromIntegral integer)
codeGenLiteral (CharLit char) = [] -- Todo: What to do with char?
codeGenLiteral (BoolLit bool) = if bool then [4] else [3] -- Push directly 1 (4) or 0 (3) 
codeGenLiteral Null = []

splitLenInTwoBytes :: Int -> [Int]
splitLenInTwoBytes len = [highByte, lowByte]
  where
    highByte = (len `shiftR` 8) .&. 0xFF
    lowByte = len .&. 0xFF

getMethodsFromClass :: Types.TAST.Class -> [Method]
getMethodsFromClass = cmethods

getNameFromClass :: Types.TAST.Class -> String
getNameFromClass = cname

getAccessFromClass :: Types.TAST.Class -> AccessModifier
getAccessFromClass = caccess

getMbodyFromMethod :: Types.TAST.Method -> Stmt
getMbodyFromMethod = mbody

getAccessFlagsForMethod :: (Types.Core.AccessModifier, Bool) -> [Int]
getAccessFlagsForMethod (accessFlags, methodStatic) =
  case accessFlags of
    Types.Core.Public -> 1 : methodStaticFlag
    Types.Core.Private -> 2 : methodStaticFlag
    Types.Core.Protected -> 4 : methodStaticFlag
    _ -> methodStaticFlag -- Todo: Is this right?
  where
    methodStaticFlag = if methodStatic then [8] else []