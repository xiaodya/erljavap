%%%-------------------------------------------------------------------
%%% @author xiaodya
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 8月 2020 11:33 下午
%%%-------------------------------------------------------------------
-module(javap).
-author("xiaodya").
-include("bytecode.hrl").
-include_lib("kernel/include/file.hrl").
%%
%%erlang impl javap
%%

%% API
-export([
    debug/2,
    test/0,
    readfile/1,
    uncompress/1,
    javapfile/2,
    magic/2,
    version/2,
    constants/2,
    access_flag/2
]).

%%
%% simple data types
%%

u1(<<X, B/binary>>) ->
    {X, B}.
u2(<<X:16/unsigned-integer, B/binary>>) ->
    {X, B}.

debug(Fmt, Args) -> io:format("DEBUG: " ++ Fmt, Args). %% display message
%%debug(_, _) -> foo. %% do nothing

%% @doc read a whole file
%% @spec readfile(string()) -> binary()
readfile(Filename) ->
    case file:read_file(Filename) of
        {ok, B} ->
            B;
        Err ->
            debug("~s ~p", [Filename, Err])
    end.

%% @doc read class type, version, file length and uncompress if necessary
%% @spec uncompress(binary()) -> binary() | atom()
%% @throws no_swf
uncompress(<<"FWS", Version, FileLength:32, B/binary>>) ->
    todo;

uncompress(<<"CWS", Version, FileLength:32, B/binary>>) ->
    todo1;

uncompress(_) -> throw(no_swf).

javap(B, State) ->
    <<16#CAFEBABE:32, B1/binary>> = B,
    {MinorVersion, B2} = u2(B1),
    {MajorVersion, B3} = u2(B2),
    {ConstantNum, B4} = u2(B3),
    {Constants, B5} = constant_pool_parse(ConstantNum, B4),
    {AccessFlags, B6} = u2(B5),
    {ClassName, B7} = u2(B6),
    {SuperClassName, B8} = u2(B7),
    {InterfaceNum, B9} = u2(B8),
    {Interfaces, B10} = interfaces_parse(InterfaceNum, B9),
    debug("javap MinorVersion:~p~n MajorVersion:~p, ConstantNum:~p~n, Constants:~p~n
    AccessFlags:0x~4.16.0B~n ClassName:~p~n SuperClassName:~p~n", [MinorVersion, MajorVersion, ConstantNum, Constants, AccessFlags, ClassName, SuperClassName]).

constant_pool_parse(ConstantNum, B) ->
    tag_foreach(1, B, [], ConstantNum - 1).

interfaces_parse(InterfaceNum, B) ->
    {[],B}.

magic(<<16#CAFEBABE:32, B/binary>>, State) ->
    debug("header check ok ~n!", []),
    NewState = State#bytecode{magic = true},
    version(B, NewState);
magic(_, _) -> throw(magic_error).

version(<<Minor_version:16/unsigned-integer, Major_version:16/unsigned-integer, B/binary>>, State) ->
    debug("Major_version:~p,Minor_version:~p~n", [Major_version, Minor_version]),
    NewState = State#bytecode{major_version = Major_version, minor_version = Minor_version},
    constants(B, NewState);
version(_, _) -> throw(version_error).

constants(<<Constant_num:16/unsigned-integer, B/binary>>, State) ->
    debug("Constant_num:~p~n", [Constant_num]),
    {Constants, B2} = constant_pool_parse(Constant_num, B),
    NewState = State#bytecode{constant_num = Constant_num, constants = Constants},
    access_flag(B2, NewState).

access_flag(<<Access_flag:16/unsigned-integer, B/binary>>, State) ->
    debug("Access_flag:~p~n", [Access_flag]),
    NewState = State#bytecode{accessflag = Access_flag},
    class_name(B, NewState).

class_name(<<Class_name:16/unsigned-integer, B/binary>>, State) ->
    debug("Class_name:~p~n", [Class_name]),
    NewState = State#bytecode{class_name = Class_name},
    super_class_name(B, NewState).

super_class_name(<<Super_class_name:16/unsigned-integer, B/binary>>, State) ->
    debug("Super_class_name:~p~n", [Super_class_name]),
    NewState = State#bytecode{super_class_name = Super_class_name},
    interfaces(B, NewState).

interfaces(<<Interface_num:16/unsigned-integer, B/binary>>, State) ->
    debug("Interface_num:~p~n", [Interface_num]),
    {Interfaces, LeftB} =
        lists:foldl(
            fun(_, {Acc, AccBin}) ->
                <<InterfaceIndex:16/unsigned-integer,LeftAccBin/binary>> = AccBin,
                {Acc++[InterfaceIndex],LeftAccBin}
            end, {[], B}, lists:seq(1, Interface_num)),
    NewState = State#bytecode{interface_num = Interface_num, interfaces = Interfaces},
    fields(LeftB, NewState).

fields(<<Field_num:16/unsigned-integer, B/binary>>, State) ->
    debug("Field_num:~p~n", [Field_num]),
    {Fields, LeftB} =
        lists:foldl(
            fun(Num, {AccFields, AccBin}) ->
                <<Access_flags:16/unsigned-integer, Name_index:16/unsigned-integer,
                    Descriptor_index:16/unsigned-integer, Attributes_count:16/unsigned-integer,
                    LeftAccBin/binary>> = AccBin,
                {Attributes, LeftAccBin2} =
                    lists:foldl(fun(Num2, {AccAttributes, AccBin2}) ->
                        {Attribute, LeftAccBin2} = attributes_parse(AccBin2, Num2, State),
                        {AccAttributes ++ [Attribute], LeftAccBin2}
                                end, {[], LeftAccBin}, lists:seq(1, Attributes_count)),
                Field = #field{num = Num, access_flags = Access_flags,
                    name_index = Name_index, descriptor_index = Descriptor_index,
                    attributes_count = Attributes_count, attributes = Attributes},
                {AccFields ++ [Field], LeftAccBin2}
            end, {[], B}, lists:seq(1, Field_num)),
%%    debug("Field_num:~p Fields~p~n", [Field_num, Fields]),
    NewState = State#bytecode{field_num = Field_num, fields = Fields},
    methods(LeftB, NewState).

methods(<<Method_num:16/unsigned-integer, B/binary>>, State) ->
    debug("Method_num:~p~n", [Method_num]),
    {Methods, LeftB} =
        lists:foldl(
            fun(Num, {AccMethods, AccBin}) ->
                <<Access_flags:16/unsigned-integer, Name_index:16/unsigned-integer,
                    Descriptor_index:16/unsigned-integer, Attributes_count:16/unsigned-integer,
                    LeftAccBin/binary>> = AccBin,
                {Attributes, LeftAccBin2} =
                    lists:foldl(fun(Num2, {AccAttributes, AccBin2}) ->
                        {Attribute, LeftAccBin2} = attributes_parse(AccBin2, Num2, State),
                        {AccAttributes ++ [Attribute], LeftAccBin2}
                                end, {[], LeftAccBin}, lists:seq(1, Attributes_count)),
                Method = #method{num = Num, access_flags = Access_flags,
                    name_index = Name_index, descriptor_index = Descriptor_index,
                    attributes_count = Attributes_count, attributes = Attributes},
                {AccMethods ++ [Method], LeftAccBin2}
            end, {[], B}, lists:seq(1, Method_num)),
%%    debug("Method_num:~p Methods~p~n", [Method_num, Methods]),
    NewState = State#bytecode{method_num = Method_num, methods = Methods},
    attributes(LeftB, NewState).

attributes(<<Attribute_num:16/unsigned-integer, B/binary>>, State) ->
    debug("Attribute_num:~p~n", [Attribute_num]),
    {Attributes, LeftB} =
        lists:foldl(fun(Num, {AccAttributes, AccBin}) ->
            {Attribute, LeftAccBin} = attributes_parse(AccBin, Num, State),
            {AccAttributes ++ [Attribute], LeftAccBin}
                    end, {[], B}, lists:seq(1, Attribute_num)),
    NewState = State#bytecode{attribute_num = Attribute_num, attributes = Attributes},
%%    debug("LeftB:~p NewState:~p~n", [LeftB, NewState]),
    NewState.

attributes_parse(<<AttributeIndex:16/unsigned-integer, B/binary>>, Num, State) ->
    UtfAttrName = get_attribute_name_utf(AttributeIndex, State),
    debug("AttributeIndex:~p UtfAttrName:~p~n ", [AttributeIndex, UtfAttrName]),
    case UtfAttrName of
        <<"Signature">> ->
            <<AttributeLength:32, SignatureIndex:16, LeftB/binary>> = B,
            AttrSignature = #attr_record_signature{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, signatrue_index = SignatureIndex},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrSignature};
        <<"Code">> ->
            <<AttributeLength:32, MaxStack:16, MaxLocals:16, CodeLen:32, Code:CodeLen/binary,
                ExceptionLength:16, LeftTemp/binary>> = B,
            {Code16,_}=
            lists:foldl(
                fun(_,{Acc,<<OpCode,LeftAccBin/binary>>=AccBin})->
                    {Acc++[io_lib:format("~2.16.0B",[OpCode])],LeftAccBin}
                end,{[],Code},lists:seq(1,CodeLen)),
%%            debug("code16:~p~n",[Code16]),
            {ExceptionTable, LeftExceptionParseBin} =
                lists:foldl(fun(_, {AccExceptions, AccBin}) ->
                    {Exception, LeftAccBin} = exception_info_parse(AccBin, State),
                    {AccExceptions ++ [Exception], LeftAccBin}
                            end, {[], LeftTemp}, lists:seq(1, ExceptionLength)),
            <<CodeAttributeCount:16, LeftB2/binary>> = LeftExceptionParseBin,
            debug("CodeAttributeCount:~p~n", [CodeAttributeCount]),
            {Attributes, LeftB} =
                lists:foldl(fun(Num, {AccAttributes, AccBin2}) ->
                    {Attribute, LeftAccBin2} = attributes_parse(AccBin2, Num, State),
                    {AccAttributes ++ [Attribute], LeftAccBin2}
                            end, {[], LeftB2}, lists:seq(1, CodeAttributeCount)),
            AttrCode = #attr_record_code{attribute_name_index = AttributeIndex, attribute_length = AttributeLength,
                max_stack = MaxStack, max_locals = MaxLocals, code_length = CodeLen, code = Code, code16 = Code16,
                exception_table_length = ExceptionLength, exception_table = ExceptionTable,
                attributes_count = CodeAttributeCount, attributes = Attributes},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrCode};
        <<"ConstantValue">> ->
            <<AttributeLength:32, ConstantValueIndex:16, LeftB/binary>> = B,
            AttrConstantValue = #attr_record_constant_value{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, constant_value_index = ConstantValueIndex},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrConstantValue};
        <<"Deprecated">> ->
            <<AttributeLength:32, LeftB/binary>> = B,
            AttrDeprecated = #attr_record_deprecated{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrDeprecated};
        <<"Exceptions">> ->
            <<AttributeLength:32, NumberOfExceptions:16, LeftTemp/binary>> = B,
            {ExceptionIndexTable, LeftB} =
                lists:foldl(fun(_, {AccExceptionIndex, AccBin}) ->
                    <<EIndex:16, LeftBin/binary>> = AccBin,
                    {AccExceptionIndex ++ [EIndex], LeftBin}
                            end, {[], LeftTemp}, lists:seq(1, NumberOfExceptions)),
            AttrExceptionTable = #attr_record_exceptions{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, number_of_exceptions = NumberOfExceptions,
                exception_index_table = ExceptionIndexTable},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrExceptionTable};
        <<"InnerClasses">> ->
            <<AttributeLength:32, NumberOfClasses:16, LeftTemp/binary>> = B,
            {InnerClasses, LeftB} =
                lists:foldl(fun(_, {AccInnerInfo, AccBin}) ->
                    {InnerInfo, LeftBin} = inner_classes_info_parse(AccBin, State),
                    {AccInnerInfo ++ [InnerInfo], LeftBin}
                            end, {[], LeftTemp}, lists:seq(1, NumberOfClasses)),
            AttrInnerClass = #attr_record_inner_classes{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, number_of_classes = NumberOfClasses,
                inner_classes = InnerClasses},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrInnerClass};
        <<"LineNumberTable">> ->
            <<AttributeLength:32, LineLen:16, LeftTemp/binary>> = B,
            {LineNumberTable, LeftB} =
                lists:foldl(fun(_, {AccLineInfo, AccBin}) ->
                    {LineNumberInfo, LeftBin} = line_number_info_parse(AccBin, State),
                    {AccLineInfo ++ [LineNumberInfo], LeftBin}
                            end, {[], LeftTemp}, lists:seq(1, LineLen)),
            AttrLineNumber = #attr_record_line_number_table{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, line_number_table_length = LineLen,
                line_number_table = LineNumberTable},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrLineNumber};
        <<"LocalVariableTable">> ->
            <<AttributeLength:32, LocalVariableLen:16, LeftTemp/binary>> = B,
            {LocalVariableTable, LeftB} =
                lists:foldl(fun(_, {AccLocalVariableInfo, AccBin}) ->
                    {LocalVariableInfo, LeftBin} = local_variable_info_parse(AccBin, State),
                    {AccLocalVariableInfo ++ [LocalVariableInfo], LeftBin}
                            end, {[], LeftTemp}, lists:seq(1, LocalVariableLen)),
            AttrLocalVariable = #attr_record_local_variable_table{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, local_variable_table_length = LocalVariableLen,
                local_variable_table = LocalVariableTable},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrLocalVariable};
        <<"LocalVariableTypeTable">> ->
            <<AttributeLength:32, Len:16, LeftTemp/binary>> = B,
            {Table, LeftB} =
                lists:foldl(fun(_, {Acc, AccBin}) ->
                    {Info, LeftBin} = local_variable_type_info_parse(AccBin, State),
                    {Acc ++ [Info], LeftBin}
                            end, {[], LeftTemp}, lists:seq(1, Len)),
            AttrRecord = #attr_record_local_variable_type_table{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, local_variable_type_table_length = Len,
                local_variable_type_table = Table},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrRecord};
        <<"SourceFile">> ->
            <<AttributeLength:32, SourceFileIndex:16, LeftB/binary>> = B,
            AttrSourceFile = #attr_record_source_file{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, source_file_index = SourceFileIndex},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrSourceFile};
        <<"Synthetic">> ->
            <<AttributeLength:32, LeftB/binary>> = B,
            AttrSynthetic = #attr_record_deprecated{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrSynthetic};
        <<"StackMapTable">> ->
            <<AttributeLength:32, NumberOfEntries:16, LeftTemp/binary>> = B,
            debug("StackMapTable AttributeLength:~p NumberOfEntries~p~n", [AttributeLength, NumberOfEntries]),
            {Entries, LeftB} =
                lists:foldl(fun(_, {AccMapFrameInfo, AccBin}) ->
                    {MapFrameInfo, LeftBin} = stack_map_frame_info_parse(AccBin, State),
                    {AccMapFrameInfo ++ [MapFrameInfo], LeftBin}
                            end, {[], LeftTemp}, lists:seq(1, NumberOfEntries)),
            AttrStackMap = #attr_record_stack_map_table{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, number_of_entries = NumberOfEntries,
                entries = Entries},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrStackMap};
        <<"BootstrapMethods">> ->
            <<AttributeLength:32, Len:16, LeftTemp/binary>> = B,
            {Table, LeftB} =
                lists:foldl(fun(_, {Acc, AccBin}) ->
                    {Info, LeftBin} = bootstrap_methods_info_parse(AccBin, State),
                    {Acc ++ [Info], LeftBin}
                            end, {[], LeftTemp}, lists:seq(1, Len)),
            AttrRecord = #attr_record_bootstrap_methods{attribute_name_index = AttributeIndex,
                attribute_length = AttributeLength, num_bootstrap_methods = Len,
                bootstrap_methods = Table},
            Attribute = #attribute{num = Num, code = AttributeIndex, attr_record = AttrRecord}
    end,
    {Attribute, LeftB}.

exception_info_parse(<<StartPc:16, EndPc:16, HandlerPc:16, CatchType:16, B/binary>>, State) ->
    {#exception_info{start_pc = StartPc, end_pc = EndPc, handler_pc = HandlerPc, catch_type = CatchType}, B}.

line_number_info_parse(<<StartPc:16, LineNumber:16, B/binary>>, State) ->
    {#line_number_info{start_pc = StartPc, line_number = LineNumber}, B}.

inner_classes_info_parse(<<InnerIndex:16, OuterIndex:16, InnerNameIndex:16, InnerAccess:16, B/binary>>, State) ->
    {#inner_classes_info{inner_class_info_index = InnerIndex, outer_class_info_index = OuterIndex,
        inner_name_index = InnerNameIndex, inner_name_access_flags = InnerAccess}, B}.

local_variable_info_parse(<<StartPc:16, Length:16, NameIndex:16, DesIndex:16, Index:16, B/binary>>, State) ->
    {#local_variable_info{start_pc = StartPc, length = Length, name_index = NameIndex,
        descriptor_index = DesIndex, index = Index}, B}.

local_variable_type_info_parse(<<StartPc:16, Length:16, NameIndex:16, SigIndex:16, Index:16, B/binary>>, State) ->
    {#local_variable_type_info{start_pc = StartPc, length = Length, name_index = NameIndex,
        signature_index = SigIndex, index = Index}, B}.

bootstrap_methods_info_parse(<<BootstrapMethodRef:16, NumBootstrapArguments:16, B/binary>>, State) ->
    {Args, B2} =
        lists:foldl(fun(_Index, {Acc, <<Arg:16, LeftAccBin/binary>> = AccBin}) ->
            {Acc ++ [Arg], LeftAccBin}
                    end, {[], B}, lists:seq(1, NumBootstrapArguments)),
    {#bootstrap_methods_info{bootstrap_method_ref = BootstrapMethodRef,
        num_bootstrap_arguments = NumBootstrapArguments, bootstrap_arguments = Args}, B2}.

stack_map_frame_info_parse(<<FrameType:8, B/binary>>, State) ->
    if
        FrameType >= 0, FrameType =< 63 ->
            {#stack_map_frame_info{frame_type = FrameType}, B};
        FrameType >= 64, FrameType =< 127 ->
            <<VerificationType:8, LeftB/binary>> = B,
            LeftB2 = verification_type_parse(VerificationType, LeftB, State),
            {#stack_map_frame_info{frame_type = FrameType}, LeftB2};
        FrameType =:= 247 ->
            <<Offset:16, VerificationType:8, LeftB/binary>> = B,
            LeftB2 = verification_type_parse(VerificationType, LeftB, State),
            {#stack_map_frame_info{frame_type = FrameType}, LeftB2};
        FrameType >= 248, FrameType =< 250 ->
            <<Offset:16, LeftB/binary>> = B,
            {#stack_map_frame_info{frame_type = FrameType}, LeftB};
        FrameType =:= 251 ->
            <<Offset:16, LeftB/binary>> = B,
            {#stack_map_frame_info{frame_type = FrameType}, LeftB};
        FrameType >= 252, FrameType =< 254 ->
            <<Offset:16, LeftB/binary>> = B,
            LeftB2 = lists:foldl(fun(_, AccBin) ->
                <<VerificationType:8, LeftAccBin/binary>> = AccBin,
                LeftAccBin2 = verification_type_parse(VerificationType, LeftAccBin, State),
                LeftAccBin2
                                 end, LeftB, lists:seq(1, (FrameType - 251))),
            {#stack_map_frame_info{frame_type = FrameType}, LeftB2};
        FrameType =:= 255 ->
            <<Offset:16, NumberOfLocals:16, LeftB/binary>> = B,
            LeftB2 = lists:foldl(fun(_, AccBin) ->
                <<VerificationType:8, LeftAccBin/binary>> = AccBin,
                LeftAccBin2 = verification_type_parse(VerificationType, LeftAccBin, State),
                LeftAccBin2
                                 end, LeftB, lists:seq(1, NumberOfLocals)),
            <<NumberOfStackItems:16, LeftB3/binary>> = LeftB2,
            LeftB4 = lists:foldl(fun(_, AccBin) ->
                <<VerificationType:8, LeftAccBin/binary>> = AccBin,
                LeftAccBin2 = verification_type_parse(VerificationType, LeftAccBin, State),
                LeftAccBin2
                                 end, LeftB3, lists:seq(1, NumberOfStackItems)),
            {#stack_map_frame_info{frame_type = FrameType}, LeftB4};
        true ->
            {#stack_map_frame_info{}, B}
    end.

verification_type_parse(Type, B, State) ->
    case Type of
        0 -> B;
        1 -> B;
        2 -> B;
        3 -> B;
        4 -> B;
        5 -> B;
        6 -> B;
        7 -> <<Offset:16, LeftB/binary>> = B, LeftB;
        8 -> <<Offset:16, LeftB/binary>> = B, LeftB
    end.

tag_foreach(Index, <<Type, _/binary>> = B, TagAcc, Count) ->
    if
        Index > Count ->
            {TagAcc, B};
        true ->
            {Tag, B2, NextIndex} = tags_parse(B, Index),
%%            debug("tag:~p index:~p tag:~p~n ", [Type, Index, Tag]),
            tag_foreach(NextIndex, B2, TagAcc ++ [Tag], Count)
    end.

tags_parse(<<Type, B/binary>>, Index) ->
    case Type of
        16#01 -> tag_fun_utf(Type, Index, B);
        16#03 -> tag_fun_i(int, Type, Index, B);
        16#04 -> tag_fun_i(float, Type, Index, B);
        16#05 -> tag_fun_i(long, Type, Index, B);
        16#06 -> tag_fun_i(double, Type, Index, B);
        16#07 -> tag_fun_indexU2(Type, Index, B);
        16#08 -> tag_fun_indexU2(Type, Index, B);
        16#09 -> tag_fun_indexU2_indexU2(Type, Index, B);
        16#0A -> tag_fun_indexU2_indexU2(Type, Index, B);
        16#0B -> tag_fun_indexU2_indexU2(Type, Index, B);
        16#0C -> tag_fun_indexU2_indexU2(Type, Index, B);
        16#0F -> tag_fun_indexU1_indexU2(Type, Index, B);
        16#10 -> tag_fun_indexU2(Type, Index, B);
        16#12 -> tag_fun_indexU2_indexU2(Type, Index, B)
    end.


tag_fun_utf(Type, Index, B) ->
    <<Len:16, Utf:Len/binary, B2/binary>> = B,
    Tag = #tag{num = Index, code = Type, bytes = Utf},
    {Tag, B2, Index + 1}.

tag_fun_indexU2(Type, Index, B) ->
    <<Index1:16, B2/binary>> = B,
    Tag = #tag{num = Index, code = Type, index1 = Index1},
    {Tag, B2, Index + 1}.

tag_fun_indexU2_indexU2(Type, Index, B) ->
    <<Index1:16, Index2:16, B2/binary>> = B,
    Tag = #tag{num = Index, code = Type, index1 = Index1, index2 = Index2},
    {Tag, B2, Index + 1}.

tag_fun_indexU1_indexU2(Type, Index, B) ->
    <<Index1:8, Index2:16, B2/binary>> = B,
    Tag = #tag{num = Index, code = Type, index1 = Index1, index2 = Index2},
    {Tag, B2, Index + 1}.

tag_fun_i(int, Type, Index, B) ->
    <<X:32/unsigned-integer-little, B2/binary>> = B,
    Tag = #tag{num = Index, code = Type, bytes = X},
    {Tag, B2, Index + 1};
tag_fun_i(float, Type, Index, B) ->
    <<X:32/float-little, B2/binary>> = B,
    Tag = #tag{num = Index, code = Type, bytes = X},
    {Tag, B2, Index + 1};
tag_fun_i(long, Type, Index, B) ->
    <<HighX:32/unsigned-integer-big, LowX:32/integer-little, B2/binary>> = B,
    X = HighX bsl 32 bor LowX,
    Tag = #tag{num = Index, code = Type, bytes = X},
    {Tag, B2, Index + 2};
tag_fun_i(double, Type, Index, B) ->
    <<HighX:32/unsigned-integer-big, LowX:32/integer-little, B2/binary>> = B,
    X = HighX bsl 32 bor LowX,
    Tag = #tag{num = Index, code = Type, bytes = X},
    {Tag, B2, Index + 2};
tag_fun_i(_Type, _, _, _) ->
    debug("unkown int type:~p~n", [_Type]).

get_attribute_name_utf(Index, #bytecode{constants = Constants} = State) ->
    case lists:keyfind(Index, #tag.num, Constants) of
        false ->
            debug("AttributeName not in constant pool find out error:~p ", [Index]);
        #tag{bytes = Bytes} ->
            Bytes
    end.

javapfile(Filename,State) ->
    B = readfile(Filename),
    Md5 = erlang:md5(B),
    {ok,#file_info{mtime = Mtime, size = Size}} = file:read_file_info(Filename),
    NewState = State#bytecode{classfile = Filename,md5 = Md5,size = Size,last_modify = Mtime},
    {B,NewState}.

test() ->
%%    B = javapfile("../../../../../data/Main.class"),
%%    debug("~s~n~n", [binary_to_list(B)]),
%%    javap(B,#bytecode{}).
%%    NewState = magic(B, #bytecode{}).


    %%test all bytecode to log file.
%%    javap_util:del_files("../../../../../log/*.log"),
%%    filelib:ensure_dir("../../../../../log/"),
%%    AllFile = filelib:wildcard("../../../../../data/*.class"),
%%    lists:foreach(
%%        fun(File) ->
%%            OutFile = string:replace(File, "class", "log"),
%%            OutFile1 = string:replace(OutFile, "data", "log"),
%%            debug("File:~p~n", [{File, lists:flatten(OutFile1)}]),
%%            {B,NewState} = javapfile(File,#bytecode{}),
%%            State = magic(B, NewState),
%%            {ok, Fd} = file:open(OutFile1, [write, raw, binary, append]),
%%            Info = io_lib:format("~p.", [State]),
%%            case filelib:is_file(OutFile1) of
%%                true -> file:write(Fd, Info);
%%                false ->
%%                    file:close(Fd),
%%                    {ok, NewFd} = file:open(OutFile1, [write, raw, binary, append]),
%%                    file:write(NewFd, Info)
%%            end
%%        end, AllFile).

    %%test log file to javap file.
    gen_code:gen_code("Kthlargest.log").


