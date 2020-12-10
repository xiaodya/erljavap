%%%-------------------------------------------------------------------
%%% @author xiaodya
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 9月 2020 8:42 下午
%%%-------------------------------------------------------------------
-module(opcode).
-author("xiaodya").
-include("bytecode.hrl").
%% API
-export([
    opcode/1,
    opcode_parse/4
    ]).

opcode(Code)->
    case Code of
        16#00-> nop;
        16#01-> aconst_null;
        16#02-> iconst_m1;
        16#03-> iconst_0;
        16#04-> iconst_1;
        16#05-> iconst_2;
        16#06-> iconst_3;
        16#07-> iconst_4;
        16#08-> iconst_5;
        16#09-> lconst_0;
        16#0a-> lconst_1;
        16#0b-> fconst_0;
        16#0c-> fconst_1;
        16#0d-> fconst_2;
        16#0e-> dconst_0;
        16#0f-> dconst_1;
        16#10-> bipush;
        16#11-> sipush;
        16#12-> ldc;
        16#13-> ldc_w;
        16#14-> ldc2_w;
        16#15-> iload;
        16#16-> lload;
        16#17-> fload;
        16#18-> dload;
        16#19-> aload;
        16#1a-> iload_0;
        16#1b-> iload_1;
        16#1c-> iload_2;
        16#1d-> iload_3;
        16#1e-> lload_0;
        16#1f-> lload_1;
        16#20-> lload_2;
        16#21-> lload_3;
        16#22-> fload_0;
        16#23-> fload_1;
        16#24-> fload_2;
        16#25-> fload_3;
        16#26-> dload_0;
        16#27-> dload_1;
        16#28-> dload_2;
        16#29-> dload_3;
        16#2a-> aload_0;
        16#2b-> aload_1;
        16#2c-> aload_2;
        16#2d-> aload_3;
        16#2e-> iaload;
        16#2f-> laload;
        16#30-> faload;
        16#31-> daload;
        16#32-> aaload;
        16#33-> baload;
        16#34-> caload;
        16#35-> saload;
        16#36-> istore;
        16#37-> lstore;
        16#38-> fstore;
        16#39-> dstore;
        16#3a-> astore;
        16#3b-> istore_0;
        16#3c-> istore_1;
        16#3d-> istore_2;
        16#3e-> istore_3;
        16#3f-> lstore_0;
        16#40-> lstore_1;
        16#41-> lstore_2;
        16#42-> lstore_3;
        16#43-> fstore_0;
        16#44-> fstore_1;
        16#45-> fstore_2;
        16#46-> fstore_3;
        16#47-> dstore_0;
        16#48-> dstore_1;
        16#49-> dstore_2;
        16#4a-> dstore_3;
        16#4b-> astore_0;
        16#4c-> astore_1;
        16#4d-> astore_2;
        16#4e-> astore_3;
        16#4f-> iastore;
        16#50-> lastore;
        16#51-> fastore;
        16#52-> dastore;
        16#53-> aastore;
        16#54-> bastore;
        16#55-> castore;
        16#56-> sastore;
        16#57-> pop;
        16#58-> pop2;
        16#59-> dup;
        16#5a-> dup_x1;
        16#5b-> dup_x2;
        16#5c-> dup2;
        16#5d-> dup2_x1;
        16#5e-> dup2_x2;
        16#5f-> swap;
        16#60-> iadd;
        16#61-> ladd;
        16#62-> fadd;
        16#63-> dadd;
        16#64-> isub;
        16#65-> lsub;
        16#66-> fsub;
        16#67-> dsub;
        16#68-> imul;
        16#69-> lmul;
        16#6a-> fmul;
        16#6b-> dmul;
        16#6c-> idiv;
        16#6d-> ldiv;
        16#6e-> fdiv;
        16#6f-> ddiv;
        16#70-> irem;
        16#71-> lrem;
        16#72-> frem;
        16#73-> drem;
        16#74-> ineg;
        16#75-> lneg;
        16#76-> fneg;
        16#77-> dneg;
        16#78-> ishl;
        16#79-> lshl;
        16#7a-> ishr;
        16#7b-> lshr;
        16#7c-> iushr;
        16#7d-> lushr;
        16#7e-> iand;
        16#7f-> land;
        16#80-> ior;
        16#81-> lor;
        16#82-> ixor;
        16#83-> lxor;
        16#84-> iinc;
        16#85-> i2l;
        16#86-> i2f;
        16#87-> i2d;
        16#88-> l2i;
        16#89-> l2f;
        16#8a-> l2d;
        16#8b-> f2i;
        16#8c-> f2l;
        16#8d-> f2d;
        16#8e-> d2i;
        16#8f-> d2l;
        16#90-> d2f;
        16#91-> i2b;
        16#92-> i2c;
        16#93-> i2s;
        16#94-> lcmp;
        16#95-> fcmpl;
        16#96-> fcmpg;
        16#97-> dcmpl;
        16#98-> dcmpg;
        16#99-> ifeq;
        16#9a-> ifne;
        16#9b-> iflt;
        16#9c-> ifge;
        16#9d-> ifgt;
        16#9e-> ifle;
        16#9f-> if_icmpeq;
        16#a0-> if_icmpne;
        16#a1-> if_icmplt;
        16#a2-> if_icmpge;
        16#a3-> if_icmpgt;
        16#a4-> if_icmple;
        16#a5-> if_acmpeq;
        16#a6-> if_acmpne;
        16#a7-> goto;
        16#a8-> jsr;
        16#a9-> ret;
        16#aa-> tableswitch;
        16#ab-> lookupswitch;
        16#ac-> ireturn;
        16#ad-> lreturn;
        16#ae-> freturn;
        16#af-> dreturn;
        16#b0-> areturn;
        16#b1-> return;
        16#b2-> getstatic;
        16#b3-> putstatic;
        16#b4-> getfield;
        16#b5-> putfield;
        16#b6-> invokevirtual;
        16#b7-> invokespecial;
        16#b8-> invokestatic;
        16#b9-> invokeinterface;
        16#ba-> nothing;
        16#bb-> new;
        16#bc-> newarray;
        16#bd-> anewarray;
        16#be-> arraylength;
        16#bf-> athrow;
        16#c0-> checkcast;
        16#c1-> instanceof;
        16#c2-> monitorenter;
        16#c3-> monitorexit;
        16#c4-> wide;
        16#c5-> multianewarray;
        16#c6-> ifnull;
        16#c7-> ifnonnull;
        16#c8-> goto_w
    end.

opcode_parse(<<>>,Record,Index,Str)->
    javap:debug("opcode_parse end Index:~p~n Str:~p~n",[Index,Str]),
    Str;
opcode_parse(<<OpCode,B/binary>>,Record,Index,Str)->
    javap:debug("opcode_parse:~p~n",[{Index,opcode(OpCode)}]),
    case OpCode of
        16#00-> opcode(B,Index,"nop",Record,Str);
        16#01-> opcode(B,Index,"aconst_null",Record,Str);
        16#02-> opcode(B,Index,"iconst_m1",Record,Str);
        16#03-> opcode(B,Index,"iconst_0",Record,Str);
        16#04-> opcode(B,Index,"iconst_1",Record,Str);
        16#05-> opcode(B,Index,"iconst_2",Record,Str);
        16#06-> opcode(B,Index,"iconst_3",Record,Str);
        16#07-> opcode(B,Index,"iconst_4",Record,Str);
        16#08-> opcode(B,Index,"iconst_5",Record,Str);
        16#09-> opcode(B,Index,"lconst_0",Record,Str);
        16#0a-> opcode(B,Index,"lconst_1",Record,Str);
        16#0b-> opcode(B,Index,"fconst_0",Record,Str);
        16#0c-> opcode(B,Index,"fconst_1",Record,Str);
        16#0d-> opcode(B,Index,"fconst_2",Record,Str);
        16#0e-> opcode(B,Index,"dconst_0",Record,Str);
        16#0f-> opcode(B,Index,"dconst_1",Record,Str);
        16#10-> opcode_n(B,Index,"bipush",Record,Str);
        16#11-> opcode_2_n(B,Index,"sipush",Record,Str);
        16#12-> opcode_ref(B,Index,"ldc",Record,Str);
        16#13-> opcode_ref(B,Index,"ldc_w",Record,Str);
        16#14-> opcode_2_ref(B,Index,"ldc2_w",Record,Str);
        16#15-> opcode_n(B,Index,"iload",Record,Str);
        16#16-> opcode_n(B,Index,"lload",Record,Str);
        16#17-> opcode_n(B,Index,"fload",Record,Str);
        16#18-> opcode_n(B,Index,"dload",Record,Str);
        16#19-> opcode_n(B,Index,"aload",Record,Str);
        16#1a-> opcode(B,Index,"iload_0",Record,Str);
        16#1b-> opcode(B,Index,"iload_1",Record,Str);
        16#1c-> opcode(B,Index,"iload_2",Record,Str);
        16#1d-> opcode(B,Index,"iload_3",Record,Str);
        16#1e-> opcode(B,Index,"lload_0",Record,Str);
        16#1f-> opcode(B,Index,"lload_1",Record,Str);
        16#20-> opcode(B,Index,"lload_2",Record,Str);
        16#21-> opcode(B,Index,"lload_3",Record,Str);
        16#22-> opcode(B,Index,"fload_0",Record,Str);
        16#23-> opcode(B,Index,"fload_1",Record,Str);
        16#24-> opcode(B,Index,"fload_2",Record,Str);
        16#25-> opcode(B,Index,"fload_3",Record,Str);
        16#26-> opcode(B,Index,"dload_0",Record,Str);
        16#27-> opcode(B,Index,"dload_1",Record,Str);
        16#28-> opcode(B,Index,"dload_2",Record,Str);
        16#29-> opcode(B,Index,"dload_3",Record,Str);
        16#2a-> opcode(B,Index,"aload_0",Record,Str);
        16#2b-> opcode(B,Index,"aload_1",Record,Str);
        16#2c-> opcode(B,Index,"aload_2",Record,Str);
        16#2d-> opcode(B,Index,"aload_3",Record,Str);
        16#2e-> opcode_n(B,Index,"iaload",Record,Str);
        16#2f-> opcode_n(B,Index,"laload",Record,Str);
        16#30-> opcode_n(B,Index,"faload",Record,Str);
        16#31-> opcode_n(B,Index,"daload",Record,Str);
        16#32-> opcode_n(B,Index,"aaload",Record,Str);
        16#33-> opcode_n(B,Index,"baload",Record,Str);
        16#34-> opcode_n(B,Index,"caload",Record,Str);
        16#35-> opcode_n(B,Index,"saload",Record,Str);
        16#36-> opcode_n(B,Index,"istore",Record,Str);
        16#37-> opcode_n(B,Index,"lstore",Record,Str);
        16#38-> opcode_n(B,Index,"fstore",Record,Str);
        16#39-> opcode_n(B,Index,"dstore",Record,Str);
        16#3a-> opcode_n(B,Index,"astore",Record,Str);
        16#3b-> opcode(B,Index,"istore_0",Record,Str);
        16#3c-> opcode(B,Index,"istore_1",Record,Str);
        16#3d-> opcode(B,Index,"istore_2",Record,Str);
        16#3e-> opcode(B,Index,"istore_3",Record,Str);
        16#3f-> opcode(B,Index,"lstore_0",Record,Str);
        16#40-> opcode(B,Index,"lstore_1",Record,Str);
        16#41-> opcode(B,Index,"lstore_2",Record,Str);
        16#42-> opcode(B,Index,"lstore_3",Record,Str);
        16#43-> opcode(B,Index,"fstore_0",Record,Str);
        16#44-> opcode(B,Index,"fstore_1",Record,Str);
        16#45-> opcode(B,Index,"fstore_2",Record,Str);
        16#46-> opcode(B,Index,"fstore_3",Record,Str);
        16#47-> opcode(B,Index,"dstore_0",Record,Str);
        16#48-> opcode(B,Index,"dstore_1",Record,Str);
        16#49-> opcode(B,Index,"dstore_2",Record,Str);
        16#4a-> opcode(B,Index,"dstore_3",Record,Str);
        16#4b-> opcode(B,Index,"astore_0",Record,Str);
        16#4c-> opcode(B,Index,"astore_1",Record,Str);
        16#4d-> opcode(B,Index,"astore_2",Record,Str);
        16#4e-> opcode(B,Index,"astore_3",Record,Str);
        16#4f-> opcode(B,Index,"iastore",Record,Str);
        16#50-> opcode(B,Index,"lastore",Record,Str);
        16#51-> opcode(B,Index,"fastore",Record,Str);
        16#52-> opcode(B,Index,"dastore",Record,Str);
        16#53-> opcode(B,Index,"aastore",Record,Str);
        16#54-> opcode(B,Index,"bastore",Record,Str);
        16#55-> opcode(B,Index,"castore",Record,Str);
        16#56-> opcode(B,Index,"sastore",Record,Str);
        16#57-> opcode(B,Index,"pop",Record,Str);
        16#58-> opcode(B,Index,"pop2",Record,Str);
        16#59-> opcode(B,Index,"dup",Record,Str);
        16#5a-> opcode(B,Index,"dup_x1",Record,Str);
        16#5b-> opcode(B,Index,"dup_x2",Record,Str);
        16#5c-> opcode(B,Index,"dup2",Record,Str);
        16#5d-> opcode(B,Index,"dup2_x1",Record,Str);
        16#5e-> opcode(B,Index,"dup2_x2",Record,Str);
        16#5f-> opcode(B,Index,"swap",Record,Str);
        16#60-> opcode(B,Index,"iadd",Record,Str);
        16#61-> opcode(B,Index,"ladd",Record,Str);
        16#62-> opcode(B,Index,"fadd",Record,Str);
        16#63-> opcode(B,Index,"dadd",Record,Str);
        16#64-> opcode(B,Index,"isub",Record,Str);
        16#65-> opcode(B,Index,"lsub",Record,Str);
        16#66-> opcode(B,Index,"fsub",Record,Str);
        16#67-> opcode(B,Index,"dsub",Record,Str);
        16#68-> opcode(B,Index,"imul",Record,Str);
        16#69-> opcode(B,Index,"lmul",Record,Str);
        16#6a-> opcode(B,Index,"fmul",Record,Str);
        16#6b-> opcode(B,Index,"dmul",Record,Str);
        16#6c-> opcode(B,Index,"idiv",Record,Str);
        16#6d-> opcode(B,Index,"ldiv",Record,Str);
        16#6e-> opcode(B,Index,"fdiv",Record,Str);
        16#6f-> opcode(B,Index,"ddiv",Record,Str);
        16#70-> opcode(B,Index,"irem",Record,Str);
        16#71-> opcode(B,Index,"lrem",Record,Str);
        16#72-> opcode(B,Index,"frem",Record,Str);
        16#73-> opcode(B,Index,"drem",Record,Str);
        16#74-> opcode(B,Index,"ineg",Record,Str);
        16#75-> opcode(B,Index,"lneg",Record,Str);
        16#76-> opcode(B,Index,"fneg",Record,Str);
        16#77-> opcode(B,Index,"dneg",Record,Str);
        16#78-> opcode(B,Index,"ishl",Record,Str);
        16#79-> opcode(B,Index,"lshl",Record,Str);
        16#7a-> opcode(B,Index,"ishr",Record,Str);
        16#7b-> opcode(B,Index,"lshr",Record,Str);
        16#7c-> opcode(B,Index,"iushr",Record,Str);
        16#7d-> opcode(B,Index,"lushr",Record,Str);
        16#7e-> opcode(B,Index,"iand",Record,Str);
        16#7f-> opcode(B,Index,"land",Record,Str);
        16#80-> opcode(B,Index,"ior",Record,Str);
        16#81-> opcode(B,Index,"lor",Record,Str);
        16#82-> opcode(B,Index,"ixor",Record,Str);
        16#83-> opcode(B,Index,"lxor",Record,Str);
        16#84-> opcode_2_n(B,Index,"iinc",Record,Str);
        16#85-> opcode(B,Index,"i2l",Record,Str);
        16#86-> opcode(B,Index,"i2f",Record,Str);
        16#87-> opcode(B,Index,"i2d",Record,Str);
        16#88-> opcode(B,Index,"l2i",Record,Str);
        16#89-> opcode(B,Index,"l2f",Record,Str);
        16#8a-> opcode(B,Index,"l2d",Record,Str);
        16#8b-> opcode(B,Index,"f2i",Record,Str);
        16#8c-> opcode(B,Index,"f2l",Record,Str);
        16#8d-> opcode(B,Index,"f2d",Record,Str);
        16#8e-> opcode(B,Index,"d2i",Record,Str);
        16#8f-> opcode(B,Index,"d2l",Record,Str);
        16#90-> opcode(B,Index,"d2f",Record,Str);
        16#91-> opcode(B,Index,"i2b",Record,Str);
        16#92-> opcode(B,Index,"i2c",Record,Str);
        16#93-> opcode(B,Index,"i2s",Record,Str);
        16#94-> opcode(B,Index,"lcmp",Record,Str);
        16#96-> opcode(B,Index,"fcmpg",Record,Str);
        16#97-> opcode(B,Index,"dcmpl",Record,Str);
        16#98-> opcode(B,Index,"dcmpg",Record,Str);
        16#99-> opcode(B,Index,"ifeq",Record,Str);
        16#9a-> opcode(B,Index,"ifne",Record,Str);
        16#9b-> opcode(B,Index,"iflt",Record,Str);
        16#9c-> opcode(B,Index,"ifge",Record,Str);
        16#9d-> opcode(B,Index,"ifgt",Record,Str);
        16#9e-> opcode(B,Index,"ifle",Record,Str);
        16#9f-> opcode_n(B,Index,"if_icmpeq",Record,Str);
        16#a0-> opcode_n(B,Index,"if_icmpne",Record,Str);
        16#a1-> opcode_n(B,Index,"if_icmplt",Record,Str);
        16#a2-> opcode_n(B,Index,"if_icmpge",Record,Str);
        16#a3-> opcode_n(B,Index,"if_icmpgt",Record,Str);
        16#a4-> opcode_n(B,Index,"if_icmple",Record,Str);
        16#a5-> opcode_n(B,Index,"if_acmpeq",Record,Str);
        16#a6-> opcode_n(B,Index,"if_acmpne",Record,Str);
        16#a7-> opcode_2_n(B,Index,"goto",Record,Str);
        16#a8-> opcode_2_ref(B,Index,"jsr",Record,Str);
        16#a9-> opcode_ref(B,Index,"ret",Record,Str);
        16#aa-> "tableswitch\n";
        16#ab-> "lookupswitch\n";
        16#ac-> opcode(B,Index,"ireturn",Record,Str);
        16#ad-> opcode(B,Index,"lreturn",Record,Str);
        16#ae-> opcode(B,Index,"freturn",Record,Str);
        16#af-> opcode(B,Index,"dreturn",Record,Str);
        16#b0-> opcode(B,Index,"areturn",Record,Str);
        16#b1-> opcode(B,Index,"return",Record,Str);
        16#b2-> opcode_2_ref(B,Index,"getstatic",Record,Str);
        16#b3-> opcode_2_ref(B,Index,"putstatic",Record,Str);
        16#b4-> opcode_2_ref(B,Index,"getfield",Record,Str);
        16#b5-> opcode_2_ref(B,Index,"putfield",Record,Str);
        16#b6-> opcode_2_ref(B,Index,"invokevirtual",Record,Str);
        16#b7-> opcode_2_ref(B,Index,"invokespecial",Record,Str);
        16#b8-> opcode_2_ref(B,Index,"invokestatic",Record,Str);
        16#b9-> opcode_2_ref(B,Index,"invokeinterface",Record,Str);
        16#ba-> opcode_2_ref(B,Index,"invokedynamic",Record,Str);
        16#bb-> opcode_2_ref(B,Index,"new",Record,Str);
        16#bc-> opcode_n(B,Index,"newarray",Record,Str);
        16#bd-> opcode_2_ref(B,Index,"anewarray",Record,Str);
        16#be-> opcode(B,Index,"arraylength",Record,Str);
        16#bf-> opcode(B,Index,"athrow",Record,Str);
        16#c0-> opcode_2_ref(B,Index,"checkcast",Record,Str);
        16#c1-> opcode_2_ref(B,Index,"instanceof",Record,Str);
        16#c2-> opcode(B,Index,"monitorenter",Record,Str);
        16#c3-> opcode(B,Index,"monitorexit",Record,Str);
        16#c4-> "wide\n";
        16#c5-> "multianewarray\n";
        16#c6-> opcode_2_ref(B,Index,"ifnull",Record,Str);
        16#c7-> opcode_2_ref(B,Index,"ifnonnull",Record,Str);
        16#c8-> opcode_4_ref(B,Index,"goto_w",Record,Str);
        _-> integer_to_list(Index)++": Error\n"
    end.

opcode(B,Index,Command,Record,Str)->
    opcode_parse(B,Record,Index+1,Str++[#code_str{head=Index,command = Command}]).

opcode_n(<<Byte1,B/binary>>,Index,Command,Record,Str)->
    javap:debug("opcode_n:~p~n",[{Byte1}]),
    Indexes = integer_to_list(Byte1),
    opcode_parse(B,Record,Index+2,Str++[#code_str{head=Index,command = Command,indexes = Indexes}]).

opcode_2_n(<<Byte1,Byte2,B/binary>>,Index,Command,Record,Str)->
    javap:debug("opcode_2_n:~p~n",[{Byte1,Byte2}]),
    if
        Command =:= "goto"->
            Value = (Byte1 bsl 8) bor Byte2,
            Indexes = integer_to_list(Value);
        true ->
            Indexes = integer_to_list(Byte1)++", "++integer_to_list(Byte2)
    end,
    opcode_parse(B,Record,Index+3,Str++[#code_str{head=Index,command = Command,indexes = Indexes}]).

opcode_ref(<<Byte1,B/binary>>,Index,Command,#bytecode{constants = Constants}=Record,Str)->
    javap:debug("opcode_ref:~p~n",[{Byte1}]),
    Indexes = "#"++integer_to_list(Byte1),
    Params = lists:flatten("// "++javap_util:make_tagcode_name_by_num(Byte1,Constants)++" "++javap_util:tag_get(Byte1,Constants)),
    opcode_parse(B,Record,Index+2,Str++[#code_str{head=Index,command = Command,indexes = Indexes, params = Params}]).

opcode_2_ref(<<Byte1,Byte2,B/binary>>,Index,Command,#bytecode{constants = Constants}=Record,Str)->
    javap:debug("opcode_2_ref:~p~n",[{Byte1,Byte2}]),
    Indexes = "#"++integer_to_list(Byte2),
    Params = lists:flatten("// "++javap_util:make_tagcode_name_by_num(Byte2,Constants)++" "++javap_util:tag_get(Byte2,Constants)),
    opcode_parse(B,Record,Index+3,Str++[#code_str{head=Index,command = Command,indexes = Indexes, params = Params}]).

opcode_4_ref(<<Byte1,Byte2,Byte3,Byte4,B/binary>>,Index,Command,#bytecode{constants = Constants}=Record,Str)->
    javap:debug("opcode_4_ref:~p~n",[{Byte1,Byte2,Byte3,Byte4}]),
    Indexes = lists:flatten("#"++integer_to_list(Byte1)++"#"++integer_to_list(Byte2)++"#"++integer_to_list(Byte3)++"#"++integer_to_list(Byte4)),
    opcode_parse(B,Record,Index+5,Str++[#code_str{head=Index,command = Command,indexes = Indexes}]).



