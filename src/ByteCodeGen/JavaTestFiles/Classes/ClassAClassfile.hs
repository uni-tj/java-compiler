XXX

ClassFile {
    magic = Magic, 
    minver = MinorVersion {numMinVer = 0}, 
    maxver = MajorVersion {numMaxVer = 65}, 
    count_cp = 18, 
    array_cp = [
        MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 2, index_nameandtype_cp = 3, desc = ""},
        Class_Info {tag_cp = TagClass, index_cp = 4, desc = ""},
        NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 5, index_descr_cp = 6, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 16, cad_cp = "java/lang/Object", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "<init>", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "()V", desc = ""},
        FieldRef_Info {tag_cp = TagFieldRef, index_name_cp = 8, index_nameandtype_cp = 9, desc = ""},
        Class_Info {tag_cp = TagClass, index_cp = 10, desc = ""},
        NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 11, index_descr_cp = 12, desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "ClassA", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 1, cad_cp = "c", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 1, cad_cp = "I", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "Code", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 12, cad_cp = "staticMethod", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 5, cad_cp = "(II)I", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 15, cad_cp = "nonStaticMethod", desc = ""},
        Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "(I)I", desc = ""}
    ], 
    acfg = AccessFlags [1,32], 
    this = ThisClass {index_th = 8}, -- ClassA
    super = SuperClass {index_sp = 2}, -- java/lang/Object
    count_interfaces = 0, 
    array_interfaces = [], 
    count_fields = 1, 
    array_fields = [
        Field_Info {
            af_fi = AccessFlags [], 
            index_name_fi = 11, -- c
            index_descr_fi = 12, 
            tam_fi = 0, 
            array_attr_fi = []
        }
    ], 
    count_methods = 3, 
    array_methods = [
        Method_Info {
            af_mi = AccessFlags [1], 
            index_name_mi = 5, 
            index_descr_mi = 6, 
            tam_mi = 1, 
            array_attr_mi = [
                AttributeCode {
                    index_name_attr = 13, 
                    tam_len_attr = 23, 
                    len_stack_attr = 2, 
                    len_local_attr = 1, 
                    tam_code_attr = 11, 
                    array_code_attr = [
                        42, -- aload_0
                        183,0,1, -- invokespecial
                        42, -- aload_0
                        16, 10, -- bipush
                        181,0,7, -- putfield 7 = FieldRef_Info c
                        177 -- return
                    ], 
                    tam_ex_attr = 0, 
                    array_ex_attr = [], 
                    tam_atrr_attr = 0, 
                    array_attr_attr = []
                }
            ]
        },
        Method_Info {
            af_mi = AccessFlags [1,8], 
            index_name_mi = 14, 
            index_descr_mi = 15, 
            tam_mi = 1, 
            array_attr_mi = [
                AttributeCode {
                    index_name_attr = 13, 
                    tam_len_attr = 16, 
                    len_stack_attr = 2, 
                    len_local_attr = 2, 
                    tam_code_attr = 4, 
                    array_code_attr = [
                        26, -- iload_0
                        27, -- iload_1
                        96, -- iadd
                        172 -- ireturn
                    ], 
                    tam_ex_attr = 0, 
                    array_ex_attr = [], 
                    tam_atrr_attr = 0, 
                    array_attr_attr = []
                }
            ]
        },
        Method_Info {
            af_mi = AccessFlags [1], 
            index_name_mi = 16, 
            index_descr_mi = 17, 
            tam_mi = 1, 
            array_attr_mi = [
                AttributeCode {
                    index_name_attr = 13, 
                    tam_len_attr = 19, 
                    len_stack_attr = 2, 
                    len_local_attr = 2, 
                    tam_code_attr = 7, 
                    array_code_attr = [
                        27, -- iload_1 (this is 0)
                        42, -- aload_0 (this)
                        180,0,7, -- getfield FieldRef_Info c
                        104, -- imul
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