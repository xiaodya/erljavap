%% @doc 字节码命令中间状态存储结构
-record(code_str,{head=0,command=[],indexes=[],params=[]}).
%% @doc 存储完整字节码解析之后的信息
-record(bytecode,{classfile=[],last_modify,md5=[],size=0,magic=0, major_version=0, minor_version=0, constant_num=0, constants=[], accessflag=0, class_name=0,
    super_class_name, interface_num=0, interfaces=[], field_num=0, fields=[], method_num=0, methods=[], attribute_num=0, attributes=[]}).
-record(tag,{num,code,length,bytes,index1,index2}).
%% @doc U2 U2 U2 U2 len |
-record(field,{num,access_flags,name_index,descriptor_index,attributes_count,attributes}).
%% @doc U2 U2 U2 U2 len |
-record(method,{num,access_flags,name_index,descriptor_index,attributes_count,attributes}).
-record(attribute,{num,code,attr_record}).
%% @doc Java程序方法体中的代码经过Javac编译器处理后，最终变为字节码指令存储在Code属性中。当然不是所有的方法都必须有这个属性（接口中的方法或抽象方法就不存在Code属性）
%% @doc U2 U4 U2 U2 U4 U1 U2 len U2 len |
%% @doc max_stack：操作数栈深度最大值，在方法执行的任何时刻，操作数栈深度都不会超过这个值。虚拟机运行时根据这个值来分配栈帧的操作数栈深度
%% @doc max_locals：局部变量表所需存储空间，单位为Slot（参见备注四）。并不是所有局部变量占用的Slot之和，当一个局部变量的生命周期结束后，
%% @doc 其所占用的Slot将分配给其它依然存活的局部变量使用，按此方式计算出方法运行时局部变量表所需的存储空间
%% @doc code_length和code：用来存放Java源程序编译后生成的字节码指令。code_length代表字节码长度，code是用于存储字节码指令的一系列字节流
%% @doc 每一个指令是一个u1类型的单字节，当虚拟机读到code中的一个字节码（一个字节能表示256种指令，Java虚拟机规范定义了其中约200个编码对应的指令），就可以判断出该字节
%% @doc 码代表的指令，指令后面是否带有参数，参数该如何解释，虽然code_length占4个字节，但是Java虚拟机规范中限制一个方法不能超过65535条字节码指令，如果超过，Javac将拒绝编译
-record(attr_record_code,{attribute_name_index, attribute_length, max_stack, max_locals, code_length, code, code16,
    exception_table_length, exception_table, attributes_count, attributes}).
%% @doc U2 U2 U2 U2 |
-record(exception_info,{start_pc,end_pc,handler_pc,catch_type}).
%% @doc 通知虚拟机自动为静态变量赋值，只有被static关键字修饰的变量（类变量）才可以使用这项属性
%% @doc U2 U4 U2 |
-record(attr_record_constant_value,{attribute_name_index,attribute_length,constant_value_index}).
%% @doc Exceptions属性：列举出方法中可能抛出的受查异常（即方法描述时throws关键字后列出的异常），与Code属性平级，与Code属性包含的异常表不同
%% @doc U2 U4 U2 U2 | number_of_exceptions表示可能抛出number_of_exceptions种受查异常
%% @doc exception_index_table为异常索引集合，一组u2类型exception_index的集合，每一个exception_index为一个指向常量池中一CONSTANT_Class_info型常量的索引，代表该受查异常的类型
-record(attr_record_exceptions,{attribute_name_index,attribute_length,number_of_exceptions,exception_index_table}).
%% @doc InnerClasses属性：该属性用于记录内部类和宿主类之间的关系。如果一个类中定义了内部类，编译器将会为这个类与这个类包含的内部类生成InnerClasses属性
%% @doc U2 U4 U2 len |
-record(attr_record_inner_classes,{attribute_name_index,attribute_length,number_of_classes,inner_classes}).
%% @doc 每一个内部类的信息都由一个inner_classes_info表来描述，inner_classes_info表结构如下
%% @doc U2 U2 U2 U2 |
-record(inner_classes_info,{inner_class_info_index,outer_class_info_index,inner_name_index,inner_name_access_flags}).
%% @doc LineNumberTale属性：用于描述Java源码的行号与字节码行号之间的对应关系，非运行时必需属性，会默认生成至Class文件中，可以使用Javac的-g:none或-g:lines关闭或要求生成该项属性信息
%% @doc U2 U4 U2 len |
-record(attr_record_line_number_table,{attribute_name_index,attribute_length,line_number_table_length,line_number_table}).
%% @doc line_number_table是一组line_number_info类型数据的集合，其所包含的line_number_info类型数据的数量为line_number_table_length，line_number_info结构如下
%% @doc 不生成该属性的最大影响是：1，抛出异常时，堆栈将不会显示出错的行号；2，调试程序时无法按照源码设置断点
%% @doc U2 U2 | 字节码行号 Java源码行号
-record(line_number_info,{start_pc,line_number}).
%% @doc 是一组local_variable_info类型数据的集合，其所包含的local_variable_info类型数据的数量为local_variable_table_length，local_variable_info结构如下
%% @doc U2 U4 U2 len
-record(attr_record_local_variable_table,{attribute_name_index,attribute_length,local_variable_table_length,local_variable_table}).
%% @doc local_variable_table是一组local_variable_info类型数据的集合，其所包含的local_variable_info类型数据的数量为local_variable_table_length，local_variable_info结构如下
%% @doc U2 U2 U2 U2 U2 | 局部变量的生命周期开始的字节码偏移量 局部变量作用范围覆盖的长度 指向常量池中CONSTANT_Utf8_info类型常量的索引，局部变量名称
%% @doc 指向常量池中CONSTANT_Utf8_info类型常量的索引，局部变量描述符
%% @doc 局部变量在栈帧局部变量表中Slot的位置，如果这个变量的数据类型为64位类型（long或double），它占用的Slot为index和index+1这2个位置
-record(local_variable_info,{start_pc,length,name_index,descriptor_index,index}).
%% @doc 与上一个不同之处在于，它提供签名信息而不是描述符信息。此差异仅对类型使用类型变量或参数化类型的变量有意义。这样的变量将出现在两个表中，而其他类型的变量将仅出现在中LocalVariableTable。
%% @doc U2 U4 U2 len
-record(attr_record_local_variable_type_table,{attribute_name_index,attribute_length,local_variable_type_table_length,local_variable_type_table}).
%% @doc U2 U2 U2 U2 U2
-record(local_variable_type_info,{start_pc,length,name_index,signature_index,index}).
%% @odc SourceFile属性：用于记录生成这个Class文件的源码文件名称，为可选项，可以使用Javac的-g:none或-g:source关闭或要求生成该项属性信息，其结构如下
-record(attr_record_source_file,{attribute_name_index,attribute_length,source_file_index}).
%% @doc Deprecated属性和Synthetic属性：这两个属性都属于标志类型的布尔属性，只存在有和没有的区别，没有属性值的概念
%% @doc Deprecated属性表示某个类、字段或方法已经被程序作者定为不再推荐使用，可在代码中使用@Deprecated注解进行设置
%% @doc Synthetic属性表示该字段或方法不是由Java源码直接产生的，而是由编译器自行添加的（当然也可设置访问标志中的ACC_SYNTHETIC标志，所有由非用户代码产生的类、方法和字段都应当
%% @doc 至少设置Synthetic属性和ACC_SYNTHETIC标志位中的一项，唯一的例外是实例构造器<init>和类构造器<clinit>方法）这两项属性的结构为(当然attribute_length的值必须为0x00000000)
-record(attr_record_deprecated,{attribute_name_index,attribute_length}).
-record(attr_record_synthetic,{attribute_name_index,attribute_length}).
%% @doc 位于ClassFile、field_info或method_info结构的属性表中。任何类、接口、构造器方法或字段的声明如果包含了类型变量（type variable）或参数化类型，则Signature属性会为它记录泛型签名信息
%% @doc U2 U4 U2 | 指向常量池的"Signature" 为2 指向常量池的类签名、方法类型前面、字段类型签名
-record(attr_record_signature,{attribute_name_index,attribute_length,signatrue_index}).
%% @doc 为了提高验证过程的效率，在字节码规范中添加了Stack Map Table属性，以下简称栈图
%% @doc U2 U4 U2 len |
-record(attr_record_stack_map_table,{attribute_name_index,attribute_length,number_of_entries,entries}).
%% @doc entries数据结构
-record(stack_map_frame_info,{frame_type=0}).
-record(full_frame,{frame_type,offset_delta,number_of_locals,locals,number_of_stack_items,stack}).
%% @doc U2 U4 U2 len |
-record(attr_record_bootstrap_methods,{attribute_name_index,attribute_length,num_bootstrap_methods,bootstrap_methods}).
%% @doc U2 U2 U2[len] |
-record(bootstrap_methods_info,{bootstrap_method_ref,num_bootstrap_arguments,bootstrap_arguments}).

%%Constant Pool
-define(CONSTANT_UTF8_INFO_TYPE, 					16#01).
-define(CONSTANT_INTEGER_INFO_TYPE, 				16#03).     %%U1 U4
-define(CONSTANT_FLOAT_INFO_TYPE, 				    16#04).     %%U1 U4
-define(CONSTANT_LONG_INFO_TYPE, 				    16#05).     %%U1 U8
-define(CONSTANT_DOUBLE_INFO_TYPE, 				    16#06).     %%U1 U8
-define(CONSTANT_CLASS_INFO_TYPE, 				    16#07).     %%U1 U2
-define(CONSTANT_STRING_INFO_TYPE, 				    16#08).     %%U1 U2
-define(CONSTANT_FIELD_REF_INFO_TYPE, 				16#09).     %%U1 U2 U2
-define(CONSTANT_METHOD_REF_INFO_TYPE, 				16#0A).     %%U1 U2 U2
-define(CONSTANT_INTERFACE_METHOD_REF_INFO_TYPE, 	16#0B).     %%U1 U2 U2
-define(CONSTANT_NAME_AND_TYPE_INFO_TYPE, 	        16#0C).     %%U1 U2 U2
-define(CONSTANT_METHOD_HANDLE_INFO_TYPE, 	        16#0F).     %%U1 U2 U2
-define(CONSTANT_METHOD_TYPE_INFO_TYPE, 	        16#10).     %%U1 U2
-define(CONSTANT_INVOKE_DYNAMIC_INFO_TYPE, 	        16#12).     %%U1 U2 U2

%%Class Access Flags
%%Fields Access Flags
%%Methods Access Flags
-define(ACC_PUBLIC,         16#0001).
-define(ACC_PRIVATE,        16#0002).
-define(ACC_PROTECTED,      16#0004).
-define(ACC_STATIC,         16#0008).
-define(ACC_FINAL,          16#0010).
-define(ACC_SUPER,          16#0020).
-define(ACC_VOLATILE,       16#0040).
-define(ACC_TRANSIENT,      16#0080).
-define(ACC_INTERFACE,      16#0200).
-define(ACC_ABSTRACT,       16#0400).
-define(ACC_SYNTHETIC,      16#1000).
-define(ACC_ANNOTATION,     16#2000).
-define(ACC_ENUM,           16#4000).



-define(ACC_PUBLIC_FIELD,               16#0001).
-define(ACC_PRIVATE_FIELD,              16#0002).
-define(ACC_PROTECTED_FIELD,            16#0004).
-define(ACC_STATIC_FIELD,               16#0008).
-define(ACC_FINAL_FIELD,                16#0010).
-define(ACC_VOLATILE_FIELD,             16#0040).
-define(ACC_TRANSIENT_FIELD,            16#0080).
-define(ACC_SYNTHETIC_FIELD,            16#1000).
-define(ACC_ENUM_FIELD,                 16#4000).


-define(ACC_SYNCHRONIZED_METHOD,        16#0020).
-define(ACC_BRIDGE_METHOD,              16#0040).
-define(ACC_VARARGS_METHOD,             16#0080).
-define(ACC_NATIVE_METHOD,              16#0100).
-define(ACC_STRICTFP_METHOD,            16#0800).



-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).
-define(BLANK,$\ ).
-define(ENTER,$\n).
-define(TAB,$\t).