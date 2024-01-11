import Jvm.BinaryClass
import Jvm.Data.ClassFormat

main :: IO ()
main = do
  classFile <- decodeClassFile "./JavaTestFiles/simpleForLoop/simpleforLoopClass.class"
  print classFile

  let classFile = ClassFile { 
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
        Method_Info
          { -- af_mi = AccessFlags [],
            -- index_name_mi = ,
            -- index_descr_mi = ,
            tam_mi = 1,
            array_attr_mi =
              [ AttributeCode
                  { -- index_name_attr = , -- Code - Type from const pool
                    -- tam_len_attr = , -- tam_code_attr + 12
                    -- len_stack_attr = ,
                    -- len_local_attr = , -- parameter + this if not static
                    -- tam_code_attr = , -- code length -> len(array_code_attr)
                    array_code_attr =
                      [],
                    tam_ex_attr = 0,
                    array_ex_attr = [],
                    tam_atrr_attr = 0,
                    array_attr_attr = []
                  }
              ]
          }
      ],
    count_attributes = 0,
    array_attributes = []
  }
  
  print classFile
