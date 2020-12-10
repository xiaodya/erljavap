%%%-------------------------------------------------------------------
%%% @author xiaodya
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 9月 2020 11:26 下午
%%%-------------------------------------------------------------------
-module(javap_util).
-author("xiaodya").
-include("bytecode.hrl").
%% API
-export([
    get_str_by_start_end/3,
    convert_ext/3,
    convert_chars/3,
    del_dir/1,
    del_files/1,
    datetime_to_string/1,
    list_dir/1,
    get_filename/1,
    binary_to_hexstring/1,
    hexstring_to_binary/1,
    copy_files2/2,
    get_str_by_after_char/2,
    javap_method_param_parse/3,
    tag_get/2,
    make_tag_name/1,
    make_tagcode_name_by_num/2,
    make_tagcode_name_by_code/1
]).

make_tag_name(Code) ->
    case Code of
        16#01 -> "Utf8";
        16#03 -> "Integer";
        16#04 -> "Float";
        16#05 -> "Long";
        16#06 -> "Double";
        16#07 -> "Class";
        16#08 -> "String";
        16#09 -> "Fieldref";
        16#0A -> "Methodref";
        16#0B -> "InterfaceMethodref";
        16#0C -> "NameAndType";
        16#0F -> "MethodHandle";
        16#10 -> "MethodType";
        16#12 -> "InvokeDynamic";
        _ -> "Error"
    end.

make_tagcode_name_by_code(Code) ->
    case Code of
        16#01 -> "Utf8";
        16#03 -> "int";
        16#04 -> "float";
        16#05 -> "long";
        16#06 -> "double";
        16#07 -> "class";
        16#08 -> "String";
        16#09 -> "Field";
        16#0A -> "Method";
        16#0B -> "InterfaceMethod";
        16#0C -> "NameAndType";
        16#0F -> "MethodHandle";
        16#10 -> "MethodType";
        16#12 -> "InvokeDynamic";
        _ -> "Error"
    end.

make_tagcode_name_by_num(TagNum, Tags)->
    case lists:keyfind(TagNum,#tag.num, Tags) of
        false->
            if
                TagNum =:= 0 -> "";
                true ->
                    javap:debug("make_tagcode_name_by_num Error no tag num ~p~n", [TagNum]),
                    "Error "
            end;
        #tag{code = Code} ->
            make_tagcode_name_by_code(Code)
    end.

tag_get(TagNum, Tags) ->
    case lists:keyfind(TagNum, #tag.num, Tags) of
        false ->
            if
                TagNum =:= 0 -> "#0";
                true ->
                    javap:debug("tag_get Error no tag num ~p~n", [TagNum]),
                    "Error "
            end;
        #tag{code = Code, index1 = Index1, index2 = Index2, bytes = Bytes} ->
            Res =
                case Code of
                    16#01 -> ?IF(Bytes =:= <<"<init>">>, <<"\"<init>\"">>, Bytes);
                    16#03 -> integer_to_list(Bytes);
                    16#04 -> float_to_list(Bytes);
                    16#05 -> integer_to_list(Bytes);
                    16#06 -> integer_to_list(Bytes);
                    16#07 -> tag_get(Index1, Tags);
                    16#08 -> tag_get(Index1, Tags);
                    16#09 -> [tag_get(Index1, Tags)] ++ "." ++ [tag_get(Index2, Tags)];
                    16#0A -> [tag_get(Index1, Tags)] ++ "." ++ [tag_get(Index2, Tags)];
                    16#0B -> [tag_get(Index1, Tags)] ++ "." ++ [tag_get(Index2, Tags)];
                    16#0C -> [tag_get(Index1, Tags)] ++ ":" ++ [tag_get(Index2, Tags)];
                    16#0F -> [tag_get(Index1, Tags)] ++ "." ++ [tag_get(Index2, Tags)];
                    16#10 -> tag_get(Index1, Tags);
                    16#12 -> [tag_get(Index1, Tags)] ++ ":" ++ [tag_get(Index2, Tags)]
                end,
            if
                is_binary(Res) ->
%%                    javap:debug("tag_get ok ~p~n",[{TagNum,binary_to_list(Res)}]),
                    binary_to_list(Res);
                true ->
                    Res
            end
    end.

get_str_by_start_end(String, Start, End) ->
    StartPos = string:str(String, Start) + 1,
    Length = string:str(String, End) - StartPos,
    string:strip(string:substr(String, StartPos, Length), both, ?BLANK).

convert_chars(String, Srcchar, Deschar) ->
    Strs = string:tokens(String, Srcchar),
    string:join(Strs, Deschar).

del_dir(Dir) ->
    Files = filelib:wildcard(Dir ++ "/*"),
    list_dir(Files),
    file:del_dir(Dir).

list_dir([]) ->
    ignor;

list_dir([File | Files]) ->
    IsDir = filelib:is_dir(File),
    if
        IsDir ->
            list_dir(filelib:wildcard(File ++ "/*")),
            file:del_dir(File),
            list_dir(Files);
        true ->
            file:delete(File),
            list_dir(Files)
    end.

del_files(Dir) ->
    AllFile = filelib:wildcard(Dir),
    lists:foreach(fun(File) ->
        case filelib:is_dir(File) of
            true ->
                ignor;
            _ ->
                file:delete(File)
        end
                  end, AllFile).

copy_files2(Source, Destination) ->
    AllFile = filelib:wildcard(Source),
    lists:foreach(fun(File) ->
        case filelib:is_dir(File) of
            true ->
                ignor;
            _ ->
                DestFile = Destination ++ filename:basename(File),
                file:copy(File, DestFile)
        end
                  end, AllFile).

datetime_to_string(DateTime) ->
    {{Y, Mon, D}, {H, Min, S}} = DateTime,
    %%use format ~ts
    binary_to_list(unicode:characters_to_binary(io_lib:format("~p年~2..0w月~2..0w日", [Y, Mon, D]), unicode)).

get_filename(Path) ->
    get_str_by_after_char(Path, $/).

get_str_by_after_char(Str, Char) ->
    case string:rchr(Str, Char) of
        0 -> Str;
        I -> string:sub_string(Str, I + 1)
    end.

convert_ext(File, SrcExt, NewExt) ->
    case string:rstr(File, SrcExt) of
        0 -> File ++ NewExt;
        I -> string:sub_string(File, 1, I - 1) ++ NewExt
    end.

binary_to_hexstring(<<>>) ->
    "";
binary_to_hexstring(Bin) when is_binary(Bin) ->
    <<Byte:8, LefBin/binary>> = Bin,
    integer_to_hexstring(Byte) ++ binary_to_hexstring(LefBin);
binary_to_hexstring(_) ->
    "".

hexstring_to_binary([]) ->
    <<>>;
hexstring_to_binary([H, L | Left] = List) when is_list(List) ->
    CurValue = erlang:list_to_integer([H, L], 16),
    LeftBinary = hexstring_to_binary(Left),
    <<CurValue:8, LeftBinary/binary>>;
hexstring_to_binary(_) ->
    <<>>.


integer_to_hexstring(Byte) ->
    Str = erlang:integer_to_list(Byte, 16),
    case length(Str) of
        1 -> "0" ++ Str;
        _ -> Str
    end.

javap_method_param_parse([], MidParam, ReParam) ->
    ReParam;
javap_method_param_parse("[" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [MidParam ++ "[]"], ReParam);
javap_method_param_parse("[[" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [MidParam ++ "[][]"], ReParam);
javap_method_param_parse("[[[" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [MidParam ++ "[][][]"], ReParam);
javap_method_param_parse("L" ++ Left, MidParam, ReParam) ->
    {Lbody, Left1} = javap_method_param_collect_l_body(Left, []),
    javap_method_param_parse(Left1, [], ReParam ++ [lists:flatten(Lbody ++ MidParam)]);
javap_method_param_parse("I" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("int" ++ MidParam)]);
javap_method_param_parse("B" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("byte" ++ MidParam)]);
javap_method_param_parse("C" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("char" ++ MidParam)]);
javap_method_param_parse("D" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("double" ++ MidParam)]);
javap_method_param_parse("F" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("float" ++ MidParam)]);
javap_method_param_parse("J" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("long" ++ MidParam)]);
javap_method_param_parse("S" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("short" ++ MidParam)]);
javap_method_param_parse("Z" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("boolean" ++ MidParam)]);
javap_method_param_parse("V" ++ Left, MidParam, ReParam) ->
    javap_method_param_parse(Left, [], ReParam ++ [lists:flatten("void" ++ MidParam)]);
javap_method_param_parse([_ | Left] = Param, MidParam, ReParam) ->
    javap_method_param_parse(Left, MidParam, ReParam).

javap_method_param_collect_l_body(";" ++ Left, L) -> {lists:reverse(L), Left};
javap_method_param_collect_l_body([H | Left], L) -> javap_method_param_collect_l_body(Left, [H | L]);
javap_method_param_collect_l_body([], _) -> {[], []}.
