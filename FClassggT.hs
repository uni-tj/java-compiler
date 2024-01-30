ClassFile
  { magic = Magic,
    minver = MinorVersion {numMinVer = 0},
    maxver = MajorVersion {numMaxVer = 49},
    count_cp = 24,
    array_cp =
      [ Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "Code", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 16, cad_cp = "java/lang/Object", desc = ""},
        Class_Info {tag_cp = TagClass, index_cp = 2, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "ggt", desc = ""},
        Class_Info {tag_cp = TagClass, index_cp = 4, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "<init>", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "()V", desc = ""},
        NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 6, index_descr_cp = 7, desc = ""},
        Integer_Info {tag_cp = TagInteger, numi_cp = 5, desc = ""},
        MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 5, index_nameandtype_cp = 8, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "ggT", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 5, cad_cp = "(II)I", desc = ""},
        NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 11, index_descr_cp = 12, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "Add", desc = ""},
        Class_Info {tag_cp = TagClass, index_cp = 14, desc = ""},
        MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 15, index_nameandtype_cp = 8, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 8, cad_cp = "instance", desc = ""},
        Class_Info {tag_cp = TagClass, index_cp = 17, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "add", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "(I)I", desc = ""},
        NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 19, index_descr_cp = 20, desc = ""},
        MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 18, index_nameandtype_cp = 21, desc = ""},
        MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 5, index_nameandtype_cp = 13, desc = ""}
      ],
    acfg = AccessFlags [],
    this = ThisClass {index_th = 5},
    super = SuperClass {index_sp = 3},
    count_interfaces = 0,
    array_interfaces = [],
    count_fields = 0,
    array_fields = [],
    count_methods = 2,
    array_methods =
      [ Method_Info
          { af_mi = AccessFlags [1],
            index_name_mi = 6,
            index_descr_mi = 7,
            tam_mi = 1,
            array_attr_mi =
              [ AttributeCode
                  { index_name_attr = 1,
                    tam_len_attr = 16,
                    len_stack_attr = 1000,
                    len_local_attr = 255,
                    tam_code_attr = 4,
                    array_code_attr =
                      [ 19,
                        0,
                        9, -- Load Wide ConstantPool (#9)=5 7
                        172 -- Ireturn 5
                      ],
                    tam_ex_attr = 0,
                    array_ex_attr = [],
                    tam_atrr_attr = 0,
                    array_attr_attr = []
                  }
              ]
          },
        Method_Info
          { af_mi = AccessFlags [1, 8],
            index_name_mi = 11,
            index_descr_mi = 12,
            tam_mi = 1,
            array_attr_mi =
              [ AttributeCode
                  { index_name_attr = 1,
                    tam_len_attr = 30,
                    len_stack_attr = 1000,
                    len_local_attr = 255,
                    tam_code_attr = 18,
                    array_code_attr =
                      [ 187,
                        0,
                        15, -- New #15
                        89, -- dup
                        183,
                        0,
                        16, -- Invokespecial #16
                        196,
                        58,
                        0,
                        2, -- wide istore v2
                        19,
                        0,
                        9, -- Load wide constant
                        182,
                        0,
                        22, -- invokevirtual #22
                        172 -- ireturn
                      ],
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
