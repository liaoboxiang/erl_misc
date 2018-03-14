%% @author box
%% @doc 编码相关
%% uft8, unicode, gbk
%% string(), binary()
%% 我 ：	gbk				unicode			utf8
%%		OXd2ce[53966]	OX6211[25105]	OXe6-88-91[230,136,145]

%% 所有的binary都是utf8, 

-module(misc_encode).

-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 is_utf8_string/1,		%% 是否utf8字符串
		 is_utf8/1,				%% 是否utf8
		 to_utf8_string/1,		%% 转成utf8字符串
		 analyze_unicode_to_utf8/1,	%% 分析unicode转成utf8的具体过程
		 number_base_conversion/3	%% 进制转换
		]).

%% ===============================================================
%% unicode 和 utf8 erlang中常用
%% ===============================================================
%% 是否utf8字符串
%% return -> true | false
is_utf8_string(String) ->
	lists:all(fun(S) -> S =< 255 end, String).

%% 是否是utf8编码
%% Term::string() | binary()
%% return -> true | false
is_utf8(Term) ->
	case is_binary(Term) of
		true -> true;
		false ->
			is_utf8_string(Term)
	end.
	
%% 转成utf8字符串
%% return -> string()
-spec to_utf8_string(Term::string() | binary()) -> String::string().
to_utf8_string(String) when is_list(String) ->
    case is_utf8_string(String) of
		true -> String;
		false ->
			binary_to_list(unicode:characters_to_binary(String))
	end;
to_utf8_string(String) when is_binary(String) ->
    binary_to_list(String).

%% 转成unicode
%% return -> string()
to_unicode_string(String) when is_list(String) ->
	case is_utf8_string(String) of
		true -> unicode:characters_to_list(list_to_binary(String));
		false -> String
	end;
to_unicode_string(Bin) when is_binary(Bin) ->
	unicode:characters_to_list(Bin).

%% 拆分 unicode to utf8的过程
%% misc_encode:analyze_unicode_to_utf8([25105]).
%% werl上可以输入 misc_encode:analyze_unicode_to_utf8("我").
%% String为单个字符
analyze_unicode_to_utf8(String) ->
	[Int] = String,
	%% 16进制
	Hex = number_base_conversion(integer_to_list(Int), 10, 16),
	%% 2进制
	Bin = number_base_conversion(integer_to_list(Int), 10, 2),
	io:format("unicode字库：~ts~n\\u:~p~n0x:~p~nbin:~p~n~n", [String, integer_to_list(Int), Hex, get_output_bin_str(Bin)]),
	%% 根据编码规则， 转换编码：以下4个区间，根据区间，带入模板
	%% U+ 0000 ~ U+ 007F(0~127): 		XXXXXXXX
	%% U+ 0080 ~ U+ 07FF(128~2047): 	110XXXXX 10XXXXXX
	%% U+ 0800 ~ U+ FFFF(2048~65535): 	1110XXXX 10XXXXXX 10XXXXXX
	%% U+10000 ~ U+1FFFF(65536~131071): 11110XXX 10XXXXXX 10XXXXXX 10XXXXXX
	NewBin = 
	if
		Int =< 127 -> 
			io:format("在第一区间， 套用模板 0XXXXXXX~n"),
			fix_bit(Bin, 8);
		Int =< 2047 ->
			io:format("在第二区间， 套用模板 110XXXXX 10XXXXXX~n"),
			Bin1 = fix_bit(Bin, 11),
			"110" ++ string:left(Bin1, 5) ++ 
				"10" ++ string:sub_string(Bin1, 6, 11);
		Int =< 65535 -> 
			io:format("在第三区间， 套用模板 1110XXXX 10XXXXXX 10XXXXXX~n"),
			Bin1 = fix_bit(Bin, 16),
			"1110" ++ string:left(Bin1, 4) ++ 
				"10" ++ string:sub_string(Bin1, 5, 10) ++
				"10" ++ string:sub_string(Bin1, 11, 16);
		true ->
			io:format("在第四区间， 套用模板 11110XXX 10XXXXXX 10XXXXXX 10XXXXXX~n"),
			Bin1 = fix_bit(Bin, 21),
			"11110" ++ string:left(Bin1, 3) ++ 
				"10" ++ string:sub_string(Bin1, 4, 9) ++
				"10" ++ string:sub_string(Bin1, 10, 15) ++
				"10" ++ string:sub_string(Bin1, 16, 21)
	end,
	NewHex = misc_encode:number_base_conversion(NewBin, 2, 16),
	NewHex1 = fix_bit(NewHex, 2),
	HexLen = length(NewHex1),
	Utf8List = lists:map(fun(N) -> 
								 Start = N*2-1,
								 Stop = N*2,
								 Str = string:sub_string(NewHex1, Start, Stop),
								 number_base_conversion(Str, 16, 10)
						 end, lists:seq(1, erlang:trunc(HexLen/2))),
	io:format("转换后：~n"),
	io:format("bin:~p~n0x:~p~nutf8:~p~n", [get_output_bin_str(NewBin), NewHex, Utf8List]),
	ok.

%% ===============================================================
%% unicode 和 utf8 结束
%% ===============================================================

%% 从From进制转换成To进制
%% return -> string()
%% 如 number_base_conversion("25105", 10, 16) -> "6211". 10进制的25105转换成16进制
number_base_conversion(Str, From, To) ->
	if
		%% 10进制转其他
		From == 10 ->
			erlang:integer_to_list(list_to_integer(Str), To);
		%% 转成10进制
		To == 10 ->
			integer_to_list(erlang:list_to_integer(Str, From));
		true ->
			%% 先转10进制， 再转其他
			Int = erlang:list_to_integer(Str, From),
			integer_to_list(Int, To)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
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
	
%% 打印二进制时用，每4位用空格分割
get_output_bin_str(BinStr) ->
	%% 补齐位数
	BinStr1 = fix_bit(BinStr, 4),
	Len = length(BinStr1),
	List = lists:map(fun(N) -> 
					  Start = N*4-3,
					  Stop = N * 4,
					  string:sub_string(BinStr1, Start, Stop)
			  end, lists:seq(1, erlang:trunc(Len/4))),
	string:join(List, " ").






