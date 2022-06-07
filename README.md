# erljavap
erl实现简单Javap功能反编译 bytecode，目的是为了学习java字节码解释和命令

An OTP application

Build
-----
$ rebar3 compile


Summary of ByteCode
-------
* 4 bytes|Magic Number
* 2+2 bytes|Version minor_version+major_version
* 2+n bytes|Constant Pool 2 bytes is contsant length . see bytecode.hrl marco define
* 2 bytes|Access Flags
* 2 bytes|This Class Name
* 2 bytes|Super Class Name
* 2+n bytes|Interfaces
* 2+n bytes|Fields
* 2+n bytes|Methods
* 2+n bytes|Attributes

Summary of OpCode
-------
* | 指令码 | 助记符 | 操作数|说明 |
* | --- | --- | --- |
* |0x00|nop|无|什么都不做|
* |0x01|aconst_null|无|将null推送至栈顶|
* |0x02|iconst_m1|无|将int型-1推送至栈顶|
* |0x03|iconst_0|无|将int常量0推送至栈顶|
* |0x04|iconst_1|无|将int常量1推送至栈顶|
* |0x05|iconst_2|无|将int常量2推送至栈顶|
* |0x06|iconst_3|无|将int常量3推送至栈顶|
* |0x07|iconst_4|无|将int常量4推送至栈顶|
* |0x08|iconst_5|无|将int常量5推送至栈顶|
* |0x09|lconst_0|无|将long常量0推送至栈顶|
* |0x0a|lconst_1|无|将long常量1推送至栈顶|
* |0x0b|fconst_0|无|将float常量0推送至栈顶|
* |0x0c|fconst_1|无|将float常量1推送至栈顶|
* |0x0d|fconst_2|无|将float常量2推送至栈顶|
* |0x0e|dconst_0|无|将double常量0推送至栈顶|
* |0x0f|dconst_1|无|将double常量1推送至栈顶|
* |0x10|bipush|byte类型操作数|将单字节的常量值(-128~127)推送至栈顶|
* |0x11|sipush|int类型操作数|将一个短整型常量值(-32768~32767)推送至栈顶|
* |0x12|ldc|常量编号|将int, float或String型常量值从常量池中推送至栈顶|
* |0x13|ldc_w|常量编号|将int, float或String型常量值从常量池中推送至栈顶（宽索引）|
* |0x14|ldc2_w|常量编号|将long或double型常量值从常量池中推送至栈顶（宽索引）|
* |0x15|iload|vindex|将位置为vindex的int类型的局部变量压入栈|
* |0x16|lload|vindex|将位置为vindex和(vindex+1)的long类型的局部变量压入栈|
* |0x17|fload|vindex|将位置为vindex的float类型的局部变量压入栈|
* |0x18|dload|vindex|将位置为vindex和(vindex+1)的double类型的局部变量压入栈|
* |0x19|aload|vindex|将位置为vindex的对象引用局部变量压入栈|
* |0x1a|iload_0|无|将位置为0的int类型的局部变量压入栈|
* |0x1b|iload_1|无|将位置为1的int类型的局部变量压入栈|
* |0x1c|iload_2|无|将位置为2的int类型的局部变量压入栈|
* |0x1d|iload_3|无|将位置为3的int类型的局部变量压入栈|
* |0x1e|lload_0|无|将位置为0和1的long类型的局部变量压入栈|
* |0x1f|lload_1|无|将位置为1和2的long类型的局部变量压入栈|
* |0x20|lload_2|无|将位置为2和3的long类型的局部变量压入栈|
* |0x21|lload_3|无|将位置为3和4的long类型的局部变量压入栈|
* |0x22|fload_0|无|将本地变量表的第一个float型本地变量推送至栈顶|
* |0x23|fload_1|无|将本地变量表的第二个float型本地变量推送至栈顶|
* |0x24|fload_2|无|将本地变量表的第三个float型本地变量推送至栈顶|
* |0x25|fload_3|无|将本地变量表的第四个float型本地变量推送至栈顶|
* |0x26|dload_0|无|将本地变量表的第一个double型本地变量推送至栈顶|
* |0x27|dload_1|无|将本地变量表的第二个double型本地变量推送至栈顶|
* |0x28|dload_2|无|将本地变量表的第三个double型本地变量推送至栈顶|
* |0x29|dload_3|无|将本地变量表的第四个double型本地变量推送至栈顶|
* |0x2a|aload_0|无|将引用类型变量推送至栈顶,非静态方法中 表示对this的操作，静态方法中表示对方法的第一参数的操作|
* |0x2b|aload_1|无|将本地变量表的第二个引用类型本地变量推送至栈顶|
* |0x2c|aload_2|无|将本地变量表的第三个引用类型本地变量推送至栈顶|
* |0x2d|aload_3|无|将本地变量表的第四个引用类型本地变量推送至栈顶|
* |0x2e|iaload|无|将int型数组指定索引的值推送至栈顶|
* |0x2f|laload|无|将long型数组指定索引的值推送至栈顶|
* |0x30|faload|无|将float型数组指定索引的值推送至栈顶|
* |0x31|daload|无|将double型数组指定索引的值推送至栈顶|
* |0x32|aaload|无|将引用型数组指定索引的值推送至栈顶|
* |0x33|baload|无|将boolean或byte型数组指定索引的值推送至栈顶|
* |0x34|caload|无|将char型数组指定索引的值推送至栈顶|
* |0x35|saload|无|将short型数组指定索引的值推送至栈顶|
* |0x36|istore|无|将栈顶int型数值存入指定本地变量|
* |0x37|lstore|无|将栈顶long型数值存入指定本地变量|
* |0x38|fstore|无|将栈顶float型数值存入指定本地变量|
* |0x39|dstore|无|将栈顶double型数值存入指定本地变量|
* |0x3a|astore|无|将栈顶引用型数值存入指定本地变量|
* |0x3b|istore_0|无|将栈顶int型数值存入第一个本地变量|
* |0x3c|istore_1|无|将栈顶int型数值存入第二个本地变量|
* |0x3d|istore_2|无|将栈顶int型数值存入第三个本地变量|
* |0x3e|istore_3|无|将栈顶int型数值存入第四个本地变量|
* |0x3f|lstore_0|无|将栈顶long型数值存入第一个本地变量|
* |0x40|lstore_1|无|将栈顶long型数值存入第二个本地变量|
* |0x41|lstore_2|无|将栈顶long型数值存入第三个本地变量|
* |0x42|lstore_3|无|将栈顶long型数值存入第四个本地变量|
* |0x43|fstore_0|无|将栈顶float型数值存入第一个本地变量|
* |0x44|fstore_1|无|将栈顶float型数值存入第二个本地变量|
* |0x45|fstore_2|无|将栈顶float型数值存入第三个本地变量|
* |0x46|fstore_3|无|将栈顶float型数值存入第四个本地变量|
* |0x47|dstore_0|无|将栈顶double型数值存入第一个本地变量|
* |0x48|dstore_1|无|将栈顶double型数值存入第二个本地变量|
* |0x49|dstore_2|无|将栈顶double型数值存入第三个本地变量|
* |0x4a|dstore_3|无|将栈顶double型数值存入第四个本地变量|
* |0x4b|astore_0|无|将栈顶引用型数值存入第一个本地变量|
* |0x4c|astore_1|无|将栈顶引用型数值存入第二个本地变量|
* |0x4d|astore_2|无|将栈顶引用型数值存入第三个本地变量|
* |0x4e|astore_3|无|将栈顶引用型数值存入第四个本地变量|
* |0x4f|iastore|无|将栈顶int型数值存入指定数组的指定索引位置|
* |0x50|lastore|无|将栈顶long型数值存入指定数组的指定索引位置|
* |0x51|fastore|无|将栈顶float型数值存入指定数组的指定索引位置|
* |0x52|dastore|无|将栈顶double型数值存入指定数组的指定索引位置|
* |0x53|aastore|无|将栈顶引用型数值存入指定数组的指定索引位置|
* |0x54|bastore|无|将栈顶boolean或byte型数值存入指定数组的指定索引位置|
* |0x55|castore|无|将栈顶char型数值存入指定数组的指定索引位置|
* |0x56|sastore|无|将栈顶short型数值存入指定数组的指定索引位置|
* |0x57|pop|无|将栈顶数值弹出 (数值不能是long或double类型的)|
* |0x58|pop2|无|将栈顶的一个（long或double类型的)或两个数值弹出（其它）|
* |0x59|dup|复制栈顶数值并将复制值压入栈顶|
* |0x5a|dup_x1|复制栈顶数值并将两个复制值压入栈顶|
* |0x5b|dup_x2|复制栈顶数值并将三个（或两个）复制值压入栈顶|
* |0x5c|dup2|复制栈顶一个（long或double类型的)或两个（其它）数值并将复制值压入栈顶|
* |0x5d|dup2_x1|<待补充>|
* |0x5e|dup2_x2|<待补充>|
* |0x5f|swap|无|将栈最顶端的两个数值互换(数值不能是long或double类型的)|
* |0x60|iadd|无|将栈顶两int型数值相加并将结果压入栈顶|
* |0x61|ladd|无|将栈顶两long型数值相加并将结果压入栈顶|
* |0x62|fadd|无|将栈顶两float型数值相加并将结果压入栈顶|
* |0x63|dadd|无|将栈顶两double型数值相加并将结果压入栈顶|
* |0x64|isub|无|将栈顶两int型数值相减并将结果压入栈顶|
* |0x65|lsub|无|将栈顶两long型数值相减并将结果压入栈顶|
* |0x66|fsub|无|将栈顶两float型数值相减并将结果压入栈顶|
* |0x67|dsub|无|将栈顶两double型数值相减并将结果压入栈顶|
* |0x68|imul|无|将栈顶两int型数值相乘并将结果压入栈顶|
* |0x69|lmul|无|将栈顶两long型数值相乘并将结果压入栈顶|
* |0x6a|fmul|无|将栈顶两float型数值相乘并将结果压入栈顶|
* |0x6b|dmul|无|将栈顶两double型数值相乘并将结果压入栈顶|
* |0x6c|idiv|无|将栈顶两int型数值相除并将结果压入栈顶|
* |0x6d|ldiv|无|将栈顶两long型数值相除并将结果压入栈顶|
* |0x6e|fdiv|无|将栈顶两float型数值相除并将结果压入栈顶|
* |0x6f|ddiv|无|将栈顶两double型数值相除并将结果压入栈顶|
* |0x70|irem|无|将栈顶两int型数值作取模运算并将结果压入栈顶|
* |0x71|lrem|无|将栈顶两long型数值作取模运算并将结果压入栈顶|
* |0x72|frem|无|将栈顶两float型数值作取模运算并将结果压入栈顶|
* |0x73|drem|无|将栈顶两double型数值作取模运算并将结果压入栈顶|
* |0x74|ineg|无|将栈顶int型数值取负并将结果压入栈顶|
* |0x75|lneg|无|将栈顶long型数值取负并将结果压入栈顶|
* |0x76|fneg|无|将栈顶float型数值取负并将结果压入栈顶|
* |0x77|dneg|无|将栈顶double型数值取负并将结果压入栈顶|
* |0x78|ishl|无|将int型数值左移位指定位数并将结果压入栈顶|
* |0x79|lshl|无|将long型数值左移位指定位数并将结果压入栈顶|
* |0x7a|ishr|无|将int型数值右（符号）移位指定位数并将结果压入栈顶|
* |0x7b|lshr|无|将long型数值右（符号）移位指定位数并将结果压入栈顶|
* |0x7c|iushr|无|将int型数值右（无符号）移位指定位数并将结果压入栈顶|
* |0x7d|lushr|无|将long型数值右（无符号）移位指定位数并将结果压入栈顶|
* |0x7e|iand|无|将栈顶两int型数值作“按位与”并将结果压入栈顶|
* |0x7f|land|无|将栈顶两long型数值作“按位与”并将结果压入栈顶|
* |0x80|ior|无|将栈顶两int型数值作“按位或”并将结果压入栈顶|
* |0x81|lor|无|将栈顶两long型数值作“按位或”并将结果压入栈顶|
* |0x82|ixor|无|将栈顶两int型数值作“按位异或”并将结果压入栈顶|
* |0x83|lxor|无|将栈顶两long型数值作“按位异或”并将结果压入栈顶|
* |0x84|iinc|无|将指定int型变量增加指定值（i++, i--, i+=2）|
* |0x85|i2l|无|将栈顶int型数值强制转换成long型数值并将结果压入栈顶|
* |0x86|i2f|无|将栈顶int型数值强制转换成float型数值并将结果压入栈顶|
* |0x87|i2d|无|将栈顶int型数值强制转换成double型数值并将结果压入栈顶|
* |0x88|l2i|无|将栈顶long型数值强制转换成int型数值并将结果压入栈顶|
* |0x89|l2f|无|将栈顶long型数值强制转换成float型数值并将结果压入栈顶|
* |0x8a|l2d|无|将栈顶long型数值强制转换成double型数值并将结果压入栈顶|
* |0x8b|f2i|无|将栈顶float型数值强制转换成int型数值并将结果压入栈顶|
* |0x8c|f2l|无|将栈顶float型数值强制转换成long型数值并将结果压入栈顶|
* |0x8d|f2d|无|将栈顶float型数值强制转换成double型数值并将结果压入栈顶|
* |0x8e|d2i|无|将栈顶double型数值强制转换成int型数值并将结果压入栈顶|
* |0x8f|d2l|无|将栈顶double型数值强制转换成long型数值并将结果压入栈顶|
* |0x90|d2f|无|将栈顶double型数值强制转换成float型数值并将结果压入栈顶|
* |0x91|i2b|无|将栈顶int型数值强制转换成byte型数值并将结果压入栈顶|
* |0x92|i2c|无|将栈顶int型数值强制转换成char型数值并将结果压入栈顶|
* |0x93|i2s|无|将栈顶int型数值强制转换成short型数值并将结果压入栈顶|
* |0x94|lcmp||比较栈顶两long型数值大小，并将结果（1，0，-1）压入栈顶|
* |0x95|fcmpl||比较栈顶两float型数值大小，并将结果（1，0，-1）压入栈顶；当其中一个数值为NaN时，将-1压入栈顶|
* |0x96|fcmpg||比较栈顶两float型数值大小，并将结果（1，0，-1）压入栈顶；当其中一个数值为NaN时，将1压入栈顶|
* |0x97|dcmpl||比较栈顶两double型数值大小，并将结果（1，0，-1）压入栈顶；当其中一个数值为NaN时，将-1压入栈顶|
* |0x98|dcmpg||比较栈顶两double型数值大小，并将结果（1，0，-1）压入栈顶；当其中一个数值为NaN时，将1压入栈顶|
* |0x99|ifeq||当栈顶int型数值等于0时跳转|
* |0x9a|ifne||当栈顶int型数值不等于0时跳转|
* |0x9b|iflt||当栈顶int型数值小于0时跳转|
* |0x9c|ifge||当栈顶int型数值大于等于0时跳转|
* |0x9d|ifgt||当栈顶int型数值大于0时跳转|
* |0x9e|ifle||当栈顶int型数值小于等于0时跳转|
* |0x9f|if_icmpeq||比较栈顶两int型数值大小，当结果等于0时跳转|
* |0xa0|if_icmpne||比较栈顶两int型数值大小，当结果不等于0时跳转|
* |0xa1|if_icmplt||比较栈顶两int型数值大小，当结果小于0时跳转|
* |0xa2|if_icmpge||比较栈顶两int型数值大小，当结果大于等于0时跳转|
* |0xa3|if_icmpgt||比较栈顶两int型数值大小，当结果大于0时跳转|
* |0xa4|if_icmple||比较栈顶两int型数值大小，当结果小于等于0时跳转|
* |0xa5|if_acmpeq||比较栈顶两引用型数值，当结果相等时跳转|
* |0xa6|if_acmpne||比较栈顶两引用型数值，当结果不相等时跳转|
* |0xa7|goto||无条件跳转|
* |0xa8|jsr||跳转至指定16位offset位置，并将jsr下一条指令地址压入栈顶|
* |0xa9|ret||返回至本地变量指定的index的指令位置（一般与jsr, jsr_w联合使用）|
* |0xaa|tableswitch||用于switch条件跳转，case值连续（可变长度指令）|
* |0xab|lookupswitch||用于switch条件跳转，case值不连续（可变长度指令）|
* |0xac|ireturn||从当前方法返回int|
* |0xad|lreturn||从当前方法返回long|
* |0xae|freturn||从当前方法返回float|
* |0xaf|dreturn||从当前方法返回double|
* |0xb0|areturn||从当前方法返回对象引用|
* |0xb1|return||从当前方法返回void|
* |0xb2|getstatic||获取指定类的静态域，并将其值压入栈顶|
* |0xb3|putstatic||为指定的类的静态域赋值|
* |0xb4|getfield||获取指定类的实例变量，并将其值压入栈顶|
* |0xb5|putfield||为指定的类的实例变量赋值|
* |0xb6|invokevirtual||调用实例方法|
* |0xb7|invokespecial||调用超类构造方法，实例初始化方法，私有方法|
* |0xb8|invokestatic||调用静态方法|
* |0xb9|invokeinterface||调用接口方法|
* |0xbb|new||创建一个对象，并将其引用值压入栈顶|
* |0xbc|newarray||创建一个指定原始类型（如int, float, char…）的数组，并将其引用值压入栈顶|
* |0xbd|anewarray||创建一个引用型（如类，接口，数组）的数组，并将其引用值压入栈顶|
* |0xbe|arraylength||获得数组的长度值并压入栈顶|
* |0xbf|athrow|无|将栈顶的异常抛出|
* |0xc0|checkcast||检验类型转换，检验未通过将抛出ClassCastException|
* |0xc1|instanceof||检验对象是否是指定的类的实例，如果是将1压入栈顶，否则将0压入栈顶|
* |0xc2|monitorenter||获得对象的锁，用于同步方法或同步块|
* |0xc3|monitorexit||释放对象的锁，用于同步方法或同步块|
* |0xc4|wide||<待补充>|
* |0xc5|multianewarray||创建指定类型和指定维度的多维数组（执行该指令时，操作栈中必须包含各维度的长度值），并将其引用值压入栈顶|
* |0xc6|ifnull||为null时跳转|
* |0xc7|ifnonnull||不为null时跳转|
* |0xc8|goto_w||无条件跳转（宽索引）|

* JVM指令助记符
* 变量到操作数栈：iload,iload_,lload,lload_,fload,fload_,dload,dload_,aload,aload_
* 操作数栈到变量：istore,istore_,lstore,lstore_,fstore,fstore_,dstore,dstor_,astore,astore_
* 常数到操作数栈：bipush,sipush,ldc,ldc_w,ldc2_w,aconst_null,iconst_ml,iconst_,lconst_,fconst_,dconst_
* 加：iadd,ladd,fadd,dadd
* 减：isub,lsub,fsub,dsub
* 乘：imul,lmul,fmul,dmul
* 除：idiv,ldiv,fdiv,ddiv
* 余数：irem,lrem,frem,drem
* 取负：ineg,lneg,fneg,dneg
* 移位：ishl,lshr,iushr,lshl,lshr,lushr
* 按位或：ior,lor
* 按位与：iand,land
* 按位异或：ixor,lxor
* 类型转换：i2l,i2f,i2d,l2f,l2d,f2d(放宽数值转换)
* i2b,i2c,i2s,l2i,f2i,f2l,d2i,d2l,d2f(缩窄数值转换)
* 创建类实便：new
* 创建新数组：newarray,anewarray,multianwarray
* 访问类的域和类实例域：getfield,putfield,getstatic,putstatic
* 把数据装载到操作数栈：baload,caload,saload,iaload,laload,faload,daload,aaload
* 从操作数栈存存储到数组：bastore,castore,sastore,iastore,lastore,fastore,dastore,aastore
* 获取数组长度：arraylength
* 检相类实例或数组属性：instanceof,checkcast
* 操作数栈管理：pop,pop2,dup,dup2,dup_xl,dup2_xl,dup_x2,dup2_x2,swap
* 有条件转移：ifeq,iflt,ifle,ifne,ifgt,ifge,ifnull,ifnonnull,if_icmpeq,if_icmpene,
if_icmplt,if_icmpgt,if_icmple,if_icmpge,if_acmpeq,if_acmpne,lcmp,fcmpl
fcmpg,dcmpl,dcmpg
* 复合条件转移：tableswitch,lookupswitch
* 无条件转移：goto,goto_w,jsr,jsr_w,ret
* 调度对象的实便方法：invokevirtual
* 调用由接口实现的方法：invokeinterface
* 调用需要特殊处理的实例方法：invokespecial
* 调用命名类中的静态方法：invokestatic
* 方法返回：ireturn,lreturn,freturn,dreturn,areturn,return
* 异常：athrow
* finally关键字的实现使用：jsr,jsr_w,ret
