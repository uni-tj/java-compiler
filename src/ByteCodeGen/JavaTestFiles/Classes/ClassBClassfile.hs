XX

ClassFile {
    magic = Magic, 
    minver = MinorVersion {numMinVer = 0}, 
    maxver = MajorVersion {numMaxVer = 65}, 
    count_cp = 35, 
    array_cp = 
        [
            MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 2, index_nameandtype_cp = 3, desc = ""},
            Class_Info {tag_cp = TagClass, index_cp = 4, desc = ""},
            NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 5, index_descr_cp = 6, desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 16, cad_cp = "java/lang/Object", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "<init>", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "()V", desc = ""},
            MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 8, index_nameandtype_cp = 9, desc = ""},
            Class_Info {tag_cp = TagClass, index_cp = 10, desc = ""},
            NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 11, index_descr_cp = 12, desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "ClassA", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 12, cad_cp = "staticMethod", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 5, cad_cp = "(II)I", desc = ""},
            MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 8, index_nameandtype_cp = 3, desc = ""},
            MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 8, index_nameandtype_cp = 15, desc = ""},
            NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 16, index_descr_cp = 17, desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 15, cad_cp = "nonStaticMethod", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "(I)I", desc = ""},
            FieldRef_Info {tag_cp = TagFieldRef, index_name_cp = 19, index_nameandtype_cp = 20, desc = ""},
            Class_Info {tag_cp = TagClass, index_cp = 21, desc = ""},
            NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 22, index_descr_cp = 23, desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 16, cad_cp = "java/lang/System", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 3, cad_cp = "out", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 21, cad_cp = "Ljava/io/PrintStream;", desc = ""},
            MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 25, index_nameandtype_cp = 26, desc = ""},
            Class_Info {tag_cp = TagClass, index_cp = 27, desc = ""},
            NameAndType_Info {tag_cp = TagNameAndType, index_name_cp = 28, index_descr_cp = 29, desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 19, cad_cp = "java/io/PrintStream", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 7, cad_cp = "println", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "(I)V", desc = ""},
            Class_Info {tag_cp = TagClass, index_cp = 31, desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 6, cad_cp = "ClassB", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "Code", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 4, cad_cp = "main", desc = ""},
            Utf8_Info {tag_cp = TagUtf8, tam_cp = 22, cad_cp = "([Ljava/lang/String;)V", desc = ""}
        ], 
        acfg = AccessFlags [1,32], 
        this = ThisClass {index_th = 30}, 
        super = SuperClass {index_sp = 2}, 
        count_interfaces = 0, 
        array_interfaces = [], 
        count_fields = 0, 
        array_fields = [], 
        count_methods = 2, 
        array_methods = [
            Method_Info {
                af_mi = AccessFlags [1], 
                index_name_mi = 5, 
                index_descr_mi = 6, 
                tam_mi = 1, 
                array_attr_mi = [
                    AttributeCode {
                        index_name_attr = 32, 
                        tam_len_attr = 17, 
                        len_stack_attr = 1, 
                        len_local_attr = 1, 
                        tam_code_attr = 5, 
                        array_code_attr = [
                            42,
                            183,0,1,
                            177
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
                index_name_mi = 33, 
                index_descr_mi = 34, 
                tam_mi = 1, 
                array_attr_mi = [
                    AttributeCode {
                        index_name_attr = 32, 
                        tam_len_attr = 47, 
                        len_stack_attr = 2, 
                        len_local_attr = 4, 
                        tam_code_attr = 35, 
                        array_code_attr = [
                            8, -- load 5
                            8,
                            184,0,7, -- invokestatic MethodRef_Info
                            60, -- istore_1
                            187,0,8, -- new Class_Info ClassA
                            89, -- duplicate -> For constructor call + store!!!
                            183,0,13, -- invokespecial MethodRef_Info -> Call constructor Class A 
                            77, -- astore_2 -> (0:this , 1:a, 2:instance)
                            44, -- aload_2
                            8, -- load 5
                            182,0,14, -- invokevirtual nonStaticMethod of Class A
                            62, -- istore_3
                            178,0,18, -- getstatic java/lang/System
                            27, -- iload_1
                            182,0,24, -- invokevirtual println
                            178,0,18, -- getstatic java/lang/System
                            29, -- iload_3
                            182,0,24, -- invokevirtual println
                            177 -- return
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