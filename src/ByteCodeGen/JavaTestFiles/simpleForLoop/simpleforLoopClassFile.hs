module ByteCodeGen where

import Jvm.BinaryClass
import Jvm.Data.ClassFormat

ClassFile {
    magic = Magic, 
    minver = MinorVersion {numMinVer = 0}, 
    maxver = MajorVersion {numMaxVer = 49}, 
    count_cp = 12, 
    array_cp = [
        MethodRef_Info {tag_cp = TagMethodRef, index_name_cp = 2, index_nameandtype_cp = 3, desc = ""},
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
    acfg = AccessFlags [1,32], 
    this = ThisClass {index_th = 7}, 
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
                    index_name_attr = 9, 
                    tam_len_attr = 17, 
                    len_stack_attr = 1, 
                    len_local_attr = 1, 
                    tam_code_attr = 5, 
                    array_code_attr = [42,183,0,1,177], 
                    tam_ex_attr = 0, 
                    array_ex_attr = [], 
                    tam_atrr_attr = 0, 
                    array_attr_attr = []
                }
            ]
        },
        Method_Info {
            af_mi = AccessFlags [8], 
            index_name_mi = 10, 
            index_descr_mi = 6, 
            tam_mi = 1, 
            array_attr_mi = [
                AttributeCode {
                    index_name_attr = 9, 
                    tam_len_attr = 26, 
                    len_stack_attr = 2, 
                    len_local_attr = 1, 
                    tam_code_attr = 14, 
                    array_code_attr = [3,59,26,8,162,0,9,132,0,1,167,255,248,177], 
                    tam_ex_attr = 0, 
                    array_ex_attr = [], 
                    tam_atrr_attr = 1, 
                    array_attr_attr = []
                }
            ]
        }
    ], 
    count_attributes = 0, 
    array_attributes = []
}