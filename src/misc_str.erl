%% @author box
%% @doc 字符串 列表整理


-module(misc_str).

%% ====================================================================
%% API functions
%% ====================================================================
-export([fix_bit/2,
		 term_to_string/1,
		 string_to_term/1,
		 bitstring_to_term/1,
		 term_to_bitstring/1,
		 split_string/3,
		 split_string/4
		]).

%% 补齐位数, 前面补0
%% Len::共多少位
%% fix_bit("11", 4) -> "0011".
%% fix_bit("11111", 4) -> "00011111".
fix_bit(String, Len) ->
	%% 补位（不足n*Len位的前面补0）
	case length(String) rem Len of
		 0 -> String;
		 N -> 
			 lists:flatten(lists:duplicate(Len-N, "0")) ++ String
	 end.


term_to_string(Term) ->
	lists:flatten(io_lib:format("~p", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> _Err
            end;
        _Error ->
            undefined
    end.



%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(<<>>) -> "";
bitstring_to_term(BitString) when is_list(BitString) -> BitString;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% 用指定的符号分隔字符串（一般同于打印信息，方便看，如数字："1000,000,000"， 二进制："0000 1111"）
%% Symbol::分隔符号
%% Len::长度 > 0
%% Order::顺序 asc有前往后xxx,xxx,x | desc由后往前，如数字1,000,000
%% split_string("00001111", " ", 4, asc) ->  "0000 1111".
%% split_string("1234567", ",", 3, desc) ->  "1,234,567".
split_string(Str, Symbol, Len) ->
	split_string(Str, Symbol, Len, asc).
split_string(Str, Symbol, Len, Order) ->
	StrLen = length(Str),
	case Order of
		asc ->
			List = lists:map(fun(N) -> 
							  Start = N*Len-(Len-1),
							  Stop = N * Len,
							  string:sub_string(Str, Start, Stop)
					  end, lists:seq(1, erlang:ceil(StrLen/Len))),
			string:join(List, Symbol);
		desc ->
			Str1 = lists:reverse(Str),
			List = lists:map(fun(N) -> 
							  Start = N*Len-(Len-1),
							  Stop = N * Len,
							  string:sub_string(Str1, Start, Stop)
					  end, lists:seq(1, erlang:ceil(StrLen/Len))),
			NewStr = string:join(List, Symbol),
			lists:reverse(NewStr)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


