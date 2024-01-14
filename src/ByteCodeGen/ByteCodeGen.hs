{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ByteCodeGen.ByteCodeGen where
import ByteCodeGen.Jvm.Data.ClassFormat
import Types.TAST
import Types.Core
import ByteCodeGen.JavaTestFiles.SimpleForLoop.SimpleForLoopTAST (testAst)

main :: IO ()
main = do
  print testAst

  let classFileNew = ClassFile {
    magic = Magic,
    minver = MinorVersion {numMinVer = 0},
    maxver = MajorVersion {numMaxVer = 49},
    count_cp = 12,
    array_cp =
      [ MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 2, index_nameandtype_cp = 3, desc = ""},
        Class_Info {tag_cp = TagClass, index_cp = 4, desc = ""},
        NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 5, index_descr_cp = 6, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 16, cad_cp = "java/lang/Object", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "<init>", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "()V", desc = ""},
        Class_Info {tag_cp = TagClass, index_cp = 8, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 18, cad_cp = "simpleforLoopClass", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "Code", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 13, cad_cp = "simpleForLoop", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 13, cad_cp = "StackMapTable", desc = ""}
      ],
    -- TODO:
    acfg = AccessFlags [1, 32],
    this = ThisClass {index_th = 8},
    super = SuperClass {index_sp = 2},
    --
    count_interfaces = 0,
    array_interfaces = [],
    count_fields = 0,
    array_fields = [],
    -- Todo:
    count_methods = 1,
    --
    array_methods =
      [
        -- Method_Info
        --   { -- af_mi = AccessFlags [],
        --     -- index_name_mi = ,
        --     -- index_descr_mi = ,
        --     tam_mi = 1,
        --     array_attr_mi =
        --       [ AttributeCode
        --           { -- index_name_attr = , -- Code - Type from const pool
        --             -- tam_len_attr = , -- tam_code_attr + 12
        --             -- len_stack_attr = ,
        --             -- len_local_attr = , -- parameter + this if not static
        --             -- tam_code_attr = , -- code length -> len(array_code_attr)
        --             array_code_attr =
        --               [],
        --             tam_ex_attr = 0,
        --             array_ex_attr = [],
        --             tam_atrr_attr = 0,
        --             array_attr_attr = []
        --           }
        --       ]
        --   }
      ],
    count_attributes = 0,
    array_attributes = []
  }

  print "End"

codeGenStmt :: Stmt -> [Int]
codeGenStmt (Block blocks) = concatMap codeGenStmt blocks
codeGenStmt (Return expr)    = [0]
codeGenStmt (While expr stmt) = codeGenExpr expr ++ codeGenStmt stmt
codeGenStmt (LocalVarDecl varType localName mexpr) = [0]
codeGenStmt (If expr stmt mStmt) = [0]
codeGenStmt (StmtOrExprAsStmt stmtOrExpr) = [0]

codeGenExpr :: Expr -> [Int]
codeGenExpr (This thisName) = [0]
codeGenExpr (Super superName) = [0]
codeGenExpr (Name localOrFieldOrClassType localOrFieldOrClassName) = [0]
codeGenExpr (FieldAccess fieldTyp expr fieldName) = [0] -- expr for this or class or what ever "class a a.j"
codeGenExpr (Unary unaryType unOparator expr) = [0] -- for actions with only one input var like not or ++
-- wrong - have to check for jumpe length after code block of if or while
codeGenExpr (Binary typeBinary binOperator expr0 expr1) =
    let expr0Res = codeGenExpr expr0 in
    let expr1Res = codeGenExpr expr1 in
    let len = length expr0Res + length expr1Res in
    expr0Res ++ expr1Res ++ codeGenBinOperator binOperator len
-- xxx
codeGenExpr (Literal literalType literal) = [0] -- literal is IntLit or BoolLit
codeGenExpr (StmtOrExprAsExpr stmtOrExprAsExprType stmtOrExpr) = [0] -- StmtOrExpr is assign, new

codeGenBinOperator :: BinOperator -> Int -> [Int]
codeGenBinOperator Types.Core.LT byteLen = [161] ++ (splitLenInTwoBytes byteLen)
codeGenBinOperator Types.Core.GT byteLen = [161] ++ (splitLenInTwoBytes byteLen)
-- Pattern match for rest

splitLenInTwoBytes :: Int -> [Int]
splitLenInTwoBytes len = [highByte, lowByte]
  where
    highByte = len `div` 256
    lowByte = len `mod` 256

getAccessFlags :: Types.TAST.Class -> AccessFlags
getAccessFlags classInstance = AccessFlags (getAccessFlagsFromVisibility (cvisibility classInstance))

getAccessFlagsFromVisibility :: Types.Core.Visibility -> [Int]
getAccessFlagsFromVisibility visibility = 
  case visibility of
    Types.Core.Public -> [1,32]
    Types.Core.Package -> [1,32]