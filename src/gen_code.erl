%%%-------------------------------------------------------------------
%%% @author xiaodya
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 9月 2020 10:51 下午
%%%-------------------------------------------------------------------
-module(gen_code).
-author("xiaodya").
-include("bytecode.hrl").
-define(DATA_DIR, "../../../../../log/").
%% API
-export([
    gen_code/1,
    write_to_file/2
]).

gen_code(LogFile) ->
    [OutFileStr, _] = string:tokens(LogFile, "."),
    File = ?DATA_DIR ++ LogFile,
    OutFileName = OutFileStr ++ ".txt",
    OutFile = ?DATA_DIR ++ OutFileName,
    case file:consult(File) of
        {ok, [Terms]} ->
            case write_to_file(Terms, OutFile) of
                {error, exsit} ->
                    io:format("OutFile ~p is exist !!! not create again~n", [OutFile]);
                {error, Reason} ->
                    io:format("open file ~p error: ~p ~n", [OutFile, Reason]);
                ok ->
                    io:format("create file succese!!! OutFile: ~p ~n", [OutFile])
            end;
        {error, Reason} -> io:format("open file ~p error: ~p ~n", [File, Reason])
    end.

write_to_file(ByteRecord, OutFile) ->
    case filelib:is_file(OutFile) of
        true ->
            file:delete(OutFile);
        false ->
            nothing
    end,
    case file:open(OutFile, [write, raw, binary, append]) of
        {ok, F} ->
            write_record_to_file(F, ByteRecord),
            file:close(F),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


write_record_to_file(F, ByteRecord) ->
    file:write(F, get_file_str(ByteRecord)).

get_file_str(Record) ->
    "Classfile " ++ Record#bytecode.classfile ++ "\n"
        ++ "  Last modified " ++ javap_util:datetime_to_string(Record#bytecode.last_modify) ++ "; size " ++ integer_to_list(Record#bytecode.size) ++ " bytes\n"
        ++ "  MD5 checksum " ++ string:to_lower(javap_util:binary_to_hexstring(Record#bytecode.md5)) ++ "\n"
        ++ "  Compiled from \"" ++ javap_util:convert_ext(javap_util:get_filename(Record#bytecode.classfile), "class", "java") ++ "\"\n"
        ++ "public class " ++ javap_util:convert_chars(javap_util:tag_get(Record#bytecode.class_name, Record#bytecode.constants), "/", ".") ++ "\n"
        ++ "  minor version: " ++ integer_to_list(Record#bytecode.minor_version) ++ "\n"
        ++ "  major version: " ++ integer_to_list(Record#bytecode.major_version) ++ "\n"
        ++ "  flags: (" ++ io_lib:format("0x~4.16.0B", [Record#bytecode.accessflag]) ++ ") " ++ accessflag_get(Record#bytecode.accessflag) ++ "\n"
        ++ "  this_class: #" ++ integer_to_list(Record#bytecode.class_name) ++ "\t\t\t\t\t\t  // " ++ javap_util:tag_get(Record#bytecode.class_name, Record#bytecode.constants) ++ "\n"
        ++ "  super_class: #" ++ integer_to_list(Record#bytecode.super_class_name) ++ "\t\t\t\t\t\t  // " ++ javap_util:tag_get(Record#bytecode.super_class_name, Record#bytecode.constants) ++ "\n"
        ++ "  interfaces: " ++ integer_to_list(Record#bytecode.interface_num) ++ ", fields: " ++ integer_to_list(Record#bytecode.field_num)
        ++ ", methods: " ++ integer_to_list(Record#bytecode.method_num) ++ ", attributes: " ++ integer_to_list(Record#bytecode.attribute_num) ++ "\n"
        ++ "Constant pool:\n"
        ++ make_constants_pool_str(Record)
        ++ "{\n"
        ++ make_methods_str(Record)
        ++ "}\n"
        ++ "".

make_constants_pool_str(#bytecode{constant_num = ConstantNum, constants = Constants}) ->
    lists:foldl(fun(#tag{num = Num} = Tag, Acc) ->
        Acc ++ constants_head_num_str(ConstantNum, Num) ++ " = " ++ make_tag_value(Tag, Constants) ++ "\n"
                end, [], Constants).

make_methods_str(#bytecode{method_num = MethodNum, methods = Methods, constants = Constants} = Record) ->
    lists:foldl(fun(#method{access_flags = Flag, name_index = Name, descriptor_index = Des} = Method, Acc) ->
        Acc ++ make_single_method_str(Method, Record)
                end, [], Methods).

make_single_method_str(#method{access_flags = Flag, name_index = Name, descriptor_index = Des,
    attributes_count = AttrCount, attributes = Attrs} = Method, #bytecode{constants = Constants} = Record) ->
    {Param, ReType} = method_param_and_return_get(javap_util:tag_get(Des, Constants)),
    ParamStr = string:join(Param, ", "),
    ReTypeStr = string:join(ReType, ", "),
    AttrCode = attr_record_get(Attrs, attr_record_code),
    #attr_record_code{max_stack = Stack, max_locals = Locals} = AttrCode,
    %% @doc 成员方法默认有this参数，所以要+1
    ArgSize = integer_to_list(?IF(is_static_method(Flag), length(Param), length(Param) + 1)),
    "  " ++ method_flag_get(Flag) ++ " " ++ ReTypeStr ++ " " ++ javap_util:tag_get(Name, Constants) ++ "(" ++ ParamStr ++ ");\n"
        ++ "    descriptor: " ++ javap_util:tag_get(Des, Constants) ++ "\n"
        ++ "    flags: (" ++ io_lib:format("0x~4.16.0B", [Flag]) ++ ") " ++ accessflag_get(Flag) ++ "\n"
        ++ "    Code:\n"
        ++ "      stack=" ++ integer_to_list(Stack) ++ ", locals=" ++ integer_to_list(Locals) ++ ", args_size=" ++ ArgSize ++ "\n"
        ++ make_code_str(AttrCode,Record)
        ++ "\n".

make_code_str(#attr_record_code{code_length = CodeLen, code = Code} = AttrCode, #bytecode{constants = Constants} = Record) ->
    Codes = opcode:opcode_parse(Code, Record, 0, []),
    lists:foldl(fun(#code_str{head = Head, command = Command, indexes = Indexes, params = Params}, Acc) ->
        CodeStr = lists:flatten(codes_head_num_str(Head) ++Command++make_middle_symbo(Command,"  ")++Indexes++"\t\t\t\t"++Params++"\n"),
        Acc ++ CodeStr
                end, [], Codes).

codes_head_num_str(Index) ->
    if
        Index > 99 -> "       " ++ integer_to_list(Index)++ ": ";
        Index > 9 -> "        " ++ integer_to_list(Index)++ ": ";
        true -> "         " ++ integer_to_list(Index)++ ": "
    end.

attr_record_get(Attrs, RecordAtom) ->
    lists:foldl(fun(#attribute{attr_record = AttrRecord}, Acc) ->
        [H | L] = tuple_to_list(AttrRecord),
        if
            H =:= RecordAtom ->
                Acc ++ AttrRecord;
            true ->
                Acc
        end
                end, [], Attrs).

method_param_and_return_get(Des) ->
    Pa = javap_util:get_str_by_start_end(Des, "(", ")"),
    Re = javap_util:get_str_by_after_char(Des, $)),
    ParseParam = javap_util:javap_method_param_parse(Pa, [], []),
    ParseReturn = javap_util:javap_method_param_parse(Re, [], []),
    javap:debug("method_return_get:~p~n", [{ParseParam, ParseReturn}]),
    {ParseParam, ParseReturn}.

accessflag_get(Flag) ->
    case Flag of
        ?ACC_PUBLIC -> "ACC_PUBLIC";
        ?ACC_PRIVATE -> "ACC_PRIVATE";
        ?ACC_PROTECTED -> "ACC_PROTECTED";
        ?ACC_STATIC -> "ACC_STATIC";
        ?ACC_PUBLIC bor ?ACC_STATIC -> "ACC_PUBLIC, ACC_STATIC";
        ?ACC_PRIVATE bor ?ACC_STATIC -> "ACC_PRIVATE, ACC_STATIC";
        ?ACC_PROTECTED bor ?ACC_STATIC -> "ACC_PROTECTED, ACC_STATIC";
        ?ACC_PUBLIC bor ?ACC_SUPER -> "ACC_PUBLIC, ACC_SUPER";
        _ -> "ERROR"
    end.

method_flag_get(Flag) ->
    case Flag of
        ?ACC_PUBLIC -> "public";
        ?ACC_PUBLIC bor ?ACC_STATIC -> "public static";
        ?ACC_PRIVATE bor ?ACC_STATIC -> "private static";
        ?ACC_PROTECTED bor ?ACC_STATIC -> "protected static";
        _ -> "ERROR"
    end.

is_static_method(Flag) ->
    (Flag bsr 3) band 1 =:= 1.

make_tag_value(#tag{num = Num, code = Code, bytes = Bytes, index1 = Index1, index2 = Index2} = Tag, Tags) ->
    TagIndex = make_tag_index(Tag),
    Middle = make_middle_symbo(TagIndex,"  "),
    Tag1 = ?IF(Index1=/=undefined,javap_util:tag_get(Index1, Tags),""),
    Tag2 = ?IF(Index2=/=undefined,javap_util:tag_get(Index2, Tags),""),
    case Code of
        16#01 -> "Utf8" ++ "\t\t\t\t" ++ binary_to_list(Bytes);
        16#03 -> "Integer" ++ "\t\t\t" ++ integer_to_list(Bytes);
        16#04 -> "Float" ++ "\t\t\t\t" ++ float_to_list(Bytes);
        16#05 -> "Long" ++ "\t\t\t\t" ++ integer_to_list(Bytes);
        16#06 -> "Double" ++ "\t\t\t\t" ++ integer_to_list(Bytes);
        16#07 -> "Class" ++ "\t\t\t\t" ++ TagIndex ++ Middle ++"// "++ Tag1;
        16#08 -> "String" ++ "\t\t\t\t" ++ TagIndex ++ Middle++"// " ++ Tag1;
        16#09 -> "Fieldref" ++ "\t\t\t" ++ TagIndex ++ Middle++"// " ++ Tag1 ++ "." ++ Tag2;
        16#0A -> "Methodref" ++ "\t\t\t" ++ TagIndex ++ Middle ++"// "++ Tag1 ++ "." ++ Tag2;
        16#0B -> "InterfaceMethodref" ++ "\t" ++ TagIndex ++ Middle ++"// "++ Tag1 ++ "." ++ Tag2;
        16#0C -> "NameAndType" ++ "\t\t" ++ TagIndex ++ Middle ++"// "++ Tag1 ++ ":" ++ Tag2;
        16#0F -> "MethodHandle" ++ "\t\t" ++ TagIndex ++ Middle ++"// "++ Tag1 ++ "." ++ Tag2;
        16#10 -> "MethodType" ++ "\t\t\t" ++ TagIndex ++ Middle ++"// "++ Tag1;
        16#12 -> "InvokeDynamic" ++ "\t\t" ++ TagIndex ++ Middle ++"// "++ Tag1 ++ ":" ++ Tag2;
        _-> "Error"
    end.

make_middle_symbo(Str,Offset) ->
    if
        length(Str) > 0, length(Str) < 4 -> "\t\t\t"++Offset;
        length(Str) >= 4, length(Str) < 8 -> "\t\t"++Offset;
        length(Str) >= 8, length(Str) < 12 -> "\t"++Offset;
        length(Str) =:= 12 -> "  ";
        true -> " "
    end.


make_tag_index(#tag{code = Code, index1 = Index1, index2 = Index2} = Tag) ->
    if
        Code =:= 16#0C;Code =:= 16#12 -> "#" ++ integer_to_list(Index1) ++ ":#" ++ integer_to_list(Index2);
        Code =:= 16#0F -> integer_to_list(Index1) ++ ":#" ++ integer_to_list(Index2);
        Index1 =/= undefined, Index2 =/= undefined -> "#" ++ integer_to_list(Index1) ++ ".#" ++ integer_to_list(Index2);
        Index1 =/= undefined -> "#" ++ integer_to_list(Index1);
        Index2 =/= undefined -> "#" ++ integer_to_list(Index2);
        true -> ""
    end.

constants_head_num_str(ConstantNum, Num) ->
    if
        ConstantNum > 99 ->
            if
                Num > 99 -> "  #" ++ integer_to_list(Num);
                Num > 9 -> "   #" ++ integer_to_list(Num);
                true -> "    #" ++ integer_to_list(Num)
            end;
        ConstantNum > 9 ->
            if
                Num > 9 -> "  #" ++ integer_to_list(Num);
                true -> "   #" ++ integer_to_list(Num)
            end;
        true ->
            "  #" ++ integer_to_list(Num)
    end.

