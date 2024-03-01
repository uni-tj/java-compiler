{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module ByteCodeGen.ByteCodeGen where

import           ConstantPool.ConstantPool as CP
import           Data.Bits
import           Data.List                 (findIndex)
import           Debug.Trace
import           Jvm.Data.ClassFormat
import           Types.Core
import           Types.TAST

codeGen :: [(Class, (CP.SearchFunctions, CP.MICP))] -> [ClassFile]
codeGen classList = do
  let classFileObjects = map createClassFile classList

  classFileObjects

createClassFile :: (Class, (CP.SearchFunctions, CP.MICP)) -> ClassFile
createClassFile (classInfo, (sf, (mi, cp))) = do
  let cName = cname classInfo
  let methods = cmethods classInfo
  let constructors = cconstructors classInfo
  let accessModifier = caccess classInfo

  let methodObjects = map (\method -> createMethodObject (cName, method, sf)) methods
  let constructorObjects = map (\constructor -> createConstructorObject (cName, constructor, sf)) constructors

  let classFileNew =
        ClassFile
          { magic = Magic,
            minver = MinorVersion {numMinVer = 0},
            maxver = MajorVersion {numMaxVer = 49},
            count_cp = length cp + 1,
            array_cp = map snd cp,
            acfg = AccessFlags (getAccessFlags (accessModifier, False)),
            this = ThisClass {index_th = ((findClass sf) cName)},
            super =
              SuperClass
                { index_sp =
                    ( (findClass sf)
                        ( case cextends classInfo of
                            Just a -> a
                            _      -> "java/lang/Object"
                        )
                    )
                },
            count_interfaces = 0,
            array_interfaces = [],
            count_fields = length mi,
            array_fields = mi,
            count_methods = length constructorObjects + length methodObjects,
            array_methods = constructorObjects ++ methodObjects,
            count_attributes = 0,
            array_attributes = []
          }

  classFileNew

createMethodObject :: (String, Method, CP.SearchFunctions) -> Method_Info
createMethodObject (cName, m@(Method methodAccess mtype methodStatic mName methodParams methodBody), sf) = do
  let accessFlags = getAccessFlags (methodAccess, methodStatic)
  let indexMethodName = (findName sf) mName
  let indexDescr = (findDesc sf) m
  let attributeCode = createAttributeCode (methodStatic, methodParams, methodBody, sf)

  Method_Info
    { af_mi = AccessFlags accessFlags,
      index_name_mi = indexMethodName,
      index_descr_mi = indexDescr,
      tam_mi = 1,
      array_attr_mi =
        [ attributeCode
        ]
    }

createConstructorObject :: (String, Constructor, CP.SearchFunctions) -> Method_Info
createConstructorObject (cName, Constructor crAccess crParams crBody, sf) = do
  let accessFlags = getAccessFlags (crAccess, False)
  let indexMethodName = (findName sf) "<init>"
  let indexDescr = (findAnyDesc sf) Void $ map fst crParams
  let attributeCode = createAttributeCode (False, crParams, crBody, sf)

  Method_Info
    { af_mi = AccessFlags accessFlags,
      index_name_mi = indexMethodName,
      index_descr_mi = indexDescr,
      tam_mi = 1,
      array_attr_mi =
        [ attributeCode
        ]
    }

createAttributeCode :: (Bool, [(Type, LocalName)], Stmt, CP.SearchFunctions) -> Attribute_Info
createAttributeCode (methodStatic, params, body, sf) = do
  let indexNameAttr = (findName sf) "Code"
  let localVarArr = localVarGen (methodStatic, params, body)
  let methodCode = codeGenStmt (body, localVarArr, sf)
  let stackSize = calcMaxStackSize methodCode

  AttributeCode
    { index_name_attr = indexNameAttr,
      tam_len_attr = length methodCode + 12, -- it is always + 12 because we dont have array_ex_attr ect.
      len_stack_attr = stackSize,
      len_local_attr = length localVarArr,
      tam_code_attr = length methodCode,
      array_code_attr = methodCode,
      tam_ex_attr = 0,
      array_ex_attr = [],
      tam_atrr_attr = 0,
      array_attr_attr = []
    }

codeGenStmt :: (Stmt, LocalVarArrType, CP.SearchFunctions) -> [Int]
codeGenStmt (Block blocks, localVarArr, sf) = concatMap (\block -> codeGenStmt (block, localVarArr, sf)) blocks
codeGenStmt (Return mexpr, localVarArr, sf) = do
  case mexpr of
    Just expr -> do
      let (exprCode, t) = codeGenExpr (expr, localVarArr, sf)
      case t of
        Types.Core.Int                   -> exprCode ++ [172] -- ireturn
        Types.Core.Bool                  -> exprCode ++ [172] -- ireturn
        Types.Core.Char                  -> exprCode ++ [172] -- ireturn
        Types.Core.Instance instanceName -> exprCode ++ [176] -- areturn
        Types.Core.Class className       -> error "Class can not be returned"
        _                                -> []
    Nothing -> [177]
codeGenStmt (While expr stmt, localVarArr, sf) = do
  -- expr(ending up true/false -> 1 or 0 on stack) --- 153, 0, len(stmt) --- stmt
  let (codeExpr, t) = codeGenExpr (expr, localVarArr, sf)
  let codeStmt = codeGenStmt (stmt, localVarArr, sf)
  let codeWhile = 153 : splitLenInTwoBytes (length codeStmt + 3 + 3) -- 153 -> if value is 0, branch -- 3 for offset / 2. +3 because of goto at the end
  let whileCode = codeExpr ++ codeWhile ++ codeStmt
  whileCode ++ [167] ++ (splitLenInTwoBytes (negate (length(whileCode)))) -- code for jumping to the top of the loop
codeGenStmt (LocalVarDecl varType localName mexpr, localVarArr, sf) = do
  let i = findIndex ((== localName) . fst) localVarArr
  case mexpr of
    Just expr ->
      case i of
        Just index ->
          case varType of
            Types.Core.Int -> do
              let (codeExpr, t) = codeGenExpr (expr, localVarArr, sf)
              codeExpr ++ [196, 54] ++ splitLenInTwoBytes index -- 196 -> wide 54 -> istore + numer of local var
            Types.Core.Bool -> do
              let (codeExpr, t) = codeGenExpr (expr, localVarArr, sf)
              codeExpr ++ [196, 54] ++ splitLenInTwoBytes index
            Types.Core.Char -> do
              let (codeExpr, t) = codeGenExpr (expr, localVarArr, sf)
              codeExpr ++ [196, 54] ++ splitLenInTwoBytes index
            Types.Core.Instance className -> do
              let (codeExpr, t) = codeGenExpr (expr, localVarArr, sf)
              codeExpr ++ [196, 58] ++ splitLenInTwoBytes index -- astore
            Types.Core.Class className -> error "Class can not be assigt to a var"
            _ -> []
        Nothing -> []
    Nothing -> []
codeGenStmt (If expr stmt mStmt, localVarArr, sf) =
  let (codeExpr, t) = codeGenExpr (expr, localVarArr, sf)
      codeStmt = codeGenStmt (stmt, localVarArr, sf)
      codeElseStmt = case mStmt of
        Just elseStmt -> codeGenStmt (elseStmt, localVarArr, sf)
        Nothing       -> []
   in if length codeElseStmt > 0
        then -- +3 because of added goto --- [167] ++ splitLenInTwoBytes (length codeElseStmt) -> goto to overjump else when if is taken
          codeExpr ++ [153] ++ splitLenInTwoBytes (length codeStmt + 3 + 3) ++ codeStmt ++ [167] ++ splitLenInTwoBytes (length codeElseStmt + 3) ++ codeElseStmt ++ [0]
        else codeExpr ++ [153] ++ splitLenInTwoBytes (length codeStmt + 3) ++ codeStmt
codeGenStmt (ThisCall className paras, localVarArr, sf) = do
  let codeParas = concatMap (fst . (\(typ, exprPara) -> codeGenExpr (exprPara, localVarArr, sf))) paras

  [42] ++ codeParas ++ [183] ++ (splitLenInTwoBytes $ (findConstructor sf) className (map fst paras))
codeGenStmt (SuperCall className paras, localVarArr, sf) = do
  let codeParas = concatMap (fst . (\(typ, exprPara) -> codeGenExpr (exprPara, localVarArr, sf))) paras

  [42] ++ codeParas ++ [183] ++ (splitLenInTwoBytes $ (findConstructor sf) className (map fst paras))
codeGenStmt (StmtOrExprAsStmt stmtOrExpr, localVarArr, sf) = do
  let (codeStmtOrExpr, t) = codeGenStmtOrExpr (stmtOrExpr, localVarArr, sf)
  codeStmtOrExpr

codeGenExpr :: (Expr, LocalVarArrType, CP.SearchFunctions) -> ([Int], Type)
codeGenExpr (This thisType, localVarArr, sf) = ([42], thisType) -- Aload_0 -> This is always at position 0
codeGenExpr (Super superName, localVarArr, sf) = ([], NullType)
codeGenExpr (LocalVar localVarType localVarName, localVarArr, sf) = do
  let i = findIndex ((== localVarName) . fst) localVarArr
  case i of
    Just index ->
      case localVarType of
        Types.Core.Int -> ([196, 21] ++ splitLenInTwoBytes index, Types.Core.Int) -- wide, iload
        Types.Core.Bool -> ([196, 21] ++ splitLenInTwoBytes index, Types.Core.Int) -- wide, iload
        Types.Core.Char -> ([196, 21] ++ splitLenInTwoBytes index, Types.Core.Int) -- wide, iload
        Types.Core.Instance instantName -> ([196, 25] ++ splitLenInTwoBytes index, Instance instantName) -- wide, aload
        Types.Core.Class className -> error "Var can not have value of type class"
        _ -> ([], localVarType)
    Nothing -> ([], localVarType)
codeGenExpr (ClassRef cType cName, localVarArr, sf) = ([], cType)
codeGenExpr (FieldAccess fieldType expr classname static fieldName, localVarArr, sf) = do
  let (exprCode, t) = codeGenExpr (expr, localVarArr, sf)
  let getFieldCode = (if static then 178 else 180) : (splitLenInTwoBytes $ traceShow "0x1" $ (findField sf) classname fieldName fieldType)
  (exprCode ++ getFieldCode, fieldType)
codeGenExpr (Unary unaryType unOparator expr, localVarArr, sf) = do
  let (codeExpr, t) = codeGenExpr (expr, localVarArr, sf)
  let codeUnary = codeExpr ++ codeGenUnOparator unOparator
  (codeUnary, unaryType)
codeGenExpr (Binary typeBinary binOperator expr0 expr1, localVarArr, sf) = do
  let (codeExpr0, t) = codeGenExpr (expr0, localVarArr, sf)
  let (codeExpr1, t) = codeGenExpr (expr1, localVarArr, sf)
  let codeBinOperator = codeGenBinOperator binOperator
  (codeExpr0 ++ codeExpr1 ++ codeBinOperator, typeBinary)
codeGenExpr (Literal literalType literal, localVarArr, sf) = do
  case literalType of
    Types.Core.Int  -> (19 : codeGenLiteral literal sf, literalType)
    Types.Core.Bool -> (codeGenLiteral literal sf, literalType)
    Types.Core.Char -> (19 : codeGenLiteral literal sf, literalType)
    _               -> ([], NullType)
codeGenExpr (StmtOrExprAsExpr stmtOrExpr, localVarArr, sf) = codeGenStmtOrExpr (stmtOrExpr, localVarArr, sf)

codeGenStmtOrExpr :: (StmtOrExpr, LocalVarArrType, CP.SearchFunctions) -> ([Int], Types.Core.Type)
codeGenStmtOrExpr (LocalAssign varType varName expr, localVarArr, sf) = do
  let (exprCode, t) = codeGenExpr (expr, localVarArr, sf)

  let i = findIndex ((== varName) . fst) localVarArr
  case i of
    Just index ->
      let codeStore = case varType of
            Types.Core.Int                   -> [196, 54]
            Types.Core.Bool                  -> [196, 54]
            Types.Core.Char                  -> [196, 54]
            Types.Core.Instance instanceName -> [196, 58]
            Types.Core.Class className       -> error "Can assign Class to var"
            _                                -> []
       in (exprCode ++ codeStore ++ splitLenInTwoBytes index, varType)
    Nothing -> ([], NullType)
codeGenStmtOrExpr (a@(FieldAssign varType tagetExpr className static fieldName valueExpr), localVarArr, sf) = do
  let (tagetExprCode, t) = codeGenExpr (tagetExpr, localVarArr, sf)
  let (valueExprCode, t) = codeGenExpr (valueExpr, localVarArr, sf)
  let fieldref = splitLenInTwoBytes $ traceShow ("0x2" ++ show fieldName ++ show varType ++ show a) $ (findField sf) className fieldName varType

  if static
    then (tagetExprCode ++ valueExprCode ++ [179] ++ fieldref, varType)
    else (tagetExprCode ++ valueExprCode ++ [181] ++ fieldref, varType)
codeGenStmtOrExpr (New newType className exprs, localVarArr, sf) = do
  let newCode = [187] ++ (splitLenInTwoBytes ((findClass sf) className)) ++ [89 {-Dup-}] ++ [183] ++ (splitLenInTwoBytes $ (findConstructor sf) className $ map fst exprs) -- -> Call constructor
  let paraCode = concatMap (fst . (\(typ, exprPara) -> codeGenExpr (exprPara, localVarArr, sf))) exprs

  (paraCode ++ newCode, newType)
codeGenStmtOrExpr (m@(MethodCall methodType expr className static methodName paras), localVarArr, sf) = do
  let (codeExpr, t) = codeGenExpr (expr, localVarArr, sf)

  let codeParas = concatMap (fst . (\(typ, exprPara) -> codeGenExpr (exprPara, localVarArr, sf))) paras

  let methodref = splitLenInTwoBytes $ (findMethodCall sf) m
  let codeInvoke = if static then [184] ++ methodref else [182] ++ methodref
  (codeExpr ++ codeParas ++ codeInvoke, methodType)

codeGenBinOperator :: BinOperator -> [Int]
codeGenBinOperator Add           = [96]
codeGenBinOperator Sub           = [100]
codeGenBinOperator Mul           = [104]
codeGenBinOperator Div           = [108]
codeGenBinOperator Mod           = [112] -- logical int remainder
codeGenBinOperator LAnd          = [126]
codeGenBinOperator LOr           = [128]
codeGenBinOperator Types.Core.LT = [161, 0, 7, 3, 167, 0, 4, 4] -- IF_ICMPLT label, ICONST_0, GOTO end, label:, ICONST_1
codeGenBinOperator LTE           = [163, 0, 7, 4, 167, 0, 4, 3] -- IF_ICMPGT label, ICONST_1, GOTO end, label:, ICONST_0
codeGenBinOperator Types.Core.GT = [163, 0, 7, 3, 167, 0, 4, 4] -- IF_ICMPGT label, ICONST_0, GOTO end, label:, ICONST_1
codeGenBinOperator GTE           = [161, 0, 7, 4, 167, 0, 4, 3] -- IF_ICMPLT label, ICONST_1, GOTO end, label:, ICONST_0
codeGenBinOperator Types.Core.EQ = [100, 153, 0, 7, 3, 167, 0, 4, 4] -- iSub, ifeq label, ICONST_0, GOTO end, label:, ICONST_1
codeGenBinOperator NEQ           = [100, 154, 0, 7, 3, 167, 0, 4, 4] -- iSub, ifne label, ICONST_0, GOTO end, label:, ICONST_1

codeGenUnOparator :: UnOparator -> [Int]
codeGenUnOparator Plus  = [] -- -> Do nothing
codeGenUnOparator Minus = [116] -- -> Use INEG
codeGenUnOparator LNot  = [4, 130] -- Expr must already be on the stack - push 1 -- oxr Expr(1/0) and 1 -> Bit toggel

codeGenLiteral :: Literal -> CP.SearchFunctions -> [Int]
codeGenLiteral lit@(IntLit _) sf  = splitLenInTwoBytes $ (findLiteral sf) lit
codeGenLiteral lit@(CharLit _) sf = splitLenInTwoBytes $ (findLiteral sf) lit
codeGenLiteral (BoolLit bool) _   = if bool then [4] else [3] -- Push directly 1 (4) or 0 (3)
codeGenLiteral Null _             = []

-- Function and data structure to get all local variables and store the number (index in array) of the local variable -> in order to find them later.
type LocalVarArrType = [(String, Types.Core.Type)]

localVarGen :: (Bool, [(Type, LocalName)], Stmt) -> LocalVarArrType
localVarGen (methodStatic, params, body) = do
  localVarGenMethodStatic methodStatic ++ localVarGenParams params ++ localVarGenBody body

localVarGenMethodStatic :: Bool -> LocalVarArrType
localVarGenMethodStatic methodStatic = if methodStatic then [] else [("This", Instance "This")]

localVarGenParams :: [(Type, LocalName)] -> LocalVarArrType
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
localVarGenBody (ThisCall _ _) = []
localVarGenBody (SuperCall _ _) = []

-- Calc Stacksize - never underestimate
calcMaxStackSize :: [Int] -> Int
calcMaxStackSize = snd . foldl updateMax (0, 0)
  where
    updateMax (currentStackSize, maxStackSize) opcode =
      let newStackSize = currentStackSize + calcLenStack opcode
      in (newStackSize, max newStackSize maxStackSize)

calcLenStack :: Int -> Int
calcLenStack opcode
  | opcode == 172 = -1 -- ireturn
  | opcode == 176 = -1 -- areturn
  | opcode == 153 = -1 -- ifeq
  | opcode == 154 = -1 -- ifne
  | opcode == 54 = -1 -- istore
  | opcode == 58 = -1 -- astore
  | opcode == 42 = 1 -- aload_0 
  | opcode == 21 = 1 -- iload 
  | opcode == 25 = 1 -- aload
  | opcode == 178 = 1 -- getstatic
  | opcode == 180 = 1 -- getfield
  | opcode == 19 = 1 -- ldc_w
  | opcode == 179 = -1 -- putstatic
  | opcode == 181 = -1 -- putfield
  | opcode == 187 = 1 -- new 
  | opcode == 89 = 1 -- dup 
  | opcode == 96 = -1 -- iadd 
  | opcode == 100 = -1 -- isub 
  | opcode == 104 = -1 -- imul 
  | opcode == 108 = -1 -- idiv 
  | opcode == 112 = -1 -- irem 
  | opcode == 126 = -1 -- iand 
  | opcode == 128 = -1 -- ior 
  | opcode == 130 = -1 -- ixor 
  | opcode == 161 = -2 -- if_icmplt 
  | opcode == 163 = -2 -- if_icmpgt 
  | opcode == 3 = 1 -- iconst_0 
  | opcode == 4 = 1 -- iconst_1
  | otherwise = 0

getAccessFlags :: (Types.Core.AccessModifier, Bool) -> [Int]
getAccessFlags (accessFlags, static) =
  case accessFlags of
    Types.Core.Public    -> 1 : staticFlag
    Types.Core.Private   -> 2 : staticFlag
    Types.Core.Protected -> 4 : staticFlag
    _                    -> staticFlag
  where
    staticFlag = if static then [8] else []

splitLenInTwoBytes :: Int -> [Int]
splitLenInTwoBytes len = [highByte, lowByte]
  where
    highByte = (len `shiftR` 8) .&. 0xFF
    lowByte = len .&. 0xFF
