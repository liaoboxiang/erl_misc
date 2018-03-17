%% @author box
%% @doc 编码相关

%% uft8, unicode, gbk
%% string(), binary()
%% 		gbk				unicode			utf8
%% 我 ：	0Xd2ce[206,210]	0X6211[25105]	0Xe6-88-91[230,136,145]		https://unicode-table.com/cn/6211/
%% a:	0x61[97]		0x61[97]		0x61[97]					https://unicode-table.com/cn/0061/
%% 注：erlang使用的unicode编码是UTF-16BE（ucs-2Big）

%% unicode 包含了世界各国的文字和符号，是最全的字符集（国际标准编码），unicode的每个数字（10进制）对应一个字符
%% gbk是中文字符集，包含所有的中文（包括繁体）及中文符号，gbk的字符集是unicode字符集的子集（就是说gbk的字符都能在unicode中找到）。但gbk的编码跟unicode并不一致，
%%		gbk的编码是1/2字节的，1字节的编码（范围在0~127）跟ASCII码是一致的，例如"a"的编码是97（0X61），这个范围内的编码跟unicode是一致的，但2字节的编码就不一样了，
%%		如"我"，gbk的编码是0xd2ce, unicode是0x6211,gbk和unicode的对应关系可以根据gbk-字符-unicode这样的链式关系找到
%% utf8是unicode的一种编码规则，utf8是一种变长编码，使用1~4个字节来表示一个字符，如"a"使用1个字节（0x61）, "我"使用3个字节(0Xe68891)，utf8和unicode的对应关系
%%		其实是一种规则（或者说是一个公式），具体可参考analyze_unicode_to_utf8/1。

%% unicode与utf8的关系/区别，unicode是字符集，它为每一个字符分配一个唯一的数字（id/码位/码点/codepoint），理论上可以无限拓展这个字符集
%% 						utf8是编码规则，计算机编码解码所遵循的逻辑，如一串数据"01100001 111001101000100010010001"，按照utf8的编码规则解码，就能读出"a我"，
%%							过程：数据的的第一位是0，说明第一个字符占用8位（01100001）；第9~11位是1110，说明第二个字符占用24位(111001101000100010010001)，
%%							从字符集中找出对应的2个字符"a我"
%%						同样的可以使用其他规则对unicode编码，如双字节的编码ucs-2（这种编码是包含了unicode字符库中的一部分，有一些字符没法找到），计算机按每16位去解码数据（注意大小端）， 
%%							4字节的编码ucs-4，按32位去解码数据（注意大小端）等

%% unicode字符集： https://unicode-table.com/cn/#control-character

%% 所有的binary数据<<>>在shell中看到的都是utf8

-module(misc_encode).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 is_utf8_string/1,		%% 是否utf8字符串
		 is_utf8/1,				%% 是否utf8
		 to_utf8_string/1,		%% 转成utf8字符串
		 to_unicode_string/1,	%% 转成unicode
		 analyze_unicode_to_utf8/1	%% 分析unicode转成utf8的具体过程
		]).

-export([
		 gbk_to_utf8/1,
		 utf8_to_gbk/1
		 ]).

-export([
		 number_base_conversion/3,	%% 进制转换
		 has_chinese/1			%% 是否包含中文字符
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
%% ps:可以试着用unicode:characters_to_binary(Str)对比下结果
analyze_unicode_to_utf8(String) ->
	[Int] = String,
	%% 16进制
	Hex = number_base_conversion(integer_to_list(Int), 10, 16),
	%% 2进制
	Bin = number_base_conversion(integer_to_list(Int), 10, 2),
	io:format("unicode字库：~ts~n\\u:~p~n0x:~p~nbin:~p~n~n", [String, integer_to_list(Int), Hex, get_output_bin_str(Bin)]),
	%% 根据编码规则， 转换编码：以下4个区间，根据区间，带入模板
	%% U+ 0000 ~ U+ 007F(0~127): 		0XXXXXXX
	%% U+ 0080 ~ U+ 07FF(128~2047): 	110XXXXX 10XXXXXX
	%% U+ 0800 ~ U+ FFFF(2048~65535): 	1110XXXX 10XXXXXX 10XXXXXX
	%% U+10000 ~ U+1FFFF(65536~131071): 11110XXX 10XXXXXX 10XXXXXX 10XXXXXX
	NewBin = 
	if
		Int =< 127 -> 
			io:format("在第一区间， 套用模板 0XXXXXXX~n"),
			misc_str:fix_bit(Bin, 8);
		Int =< 2047 ->
			io:format("在第二区间， 套用模板 110XXXXX 10XXXXXX~n"),
			Bin1 = misc_str:fix_bit(Bin, 11),
			"110" ++ string:left(Bin1, 5) ++ 
				"10" ++ string:sub_string(Bin1, 6, 11);
		Int =< 65535 -> 
			io:format("在第三区间， 套用模板 1110XXXX 10XXXXXX 10XXXXXX~n"),
			Bin1 = misc_str:fix_bit(Bin, 16),
			"1110" ++ string:left(Bin1, 4) ++ 
				"10" ++ string:sub_string(Bin1, 5, 10) ++
				"10" ++ string:sub_string(Bin1, 11, 16);
		true ->
			io:format("在第四区间， 套用模板 11110XXX 10XXXXXX 10XXXXXX 10XXXXXX~n"),
			Bin1 = misc_str:fix_bit(Bin, 21),
			"11110" ++ string:left(Bin1, 3) ++ 
				"10" ++ string:sub_string(Bin1, 4, 9) ++
				"10" ++ string:sub_string(Bin1, 10, 15) ++
				"10" ++ string:sub_string(Bin1, 16, 21)
	end,
	NewHex = misc_encode:number_base_conversion(NewBin, 2, 16),
	NewHex1 = misc_str:fix_bit(NewHex, 2),
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

%% ===============================================================
%% gbk 和 utf8 转换
%% 建立字库的对应关系， 通过字库查询，找出对应的值
%% ===============================================================

gbk_to_utf8(Gbk) ->
	gbk_to_utf8_2(Gbk, []).

gbk_to_utf8_2([], ResList) ->
	lists:reverse(ResList);
gbk_to_utf8_2([Char| List], ResList) when Char < 128 ->
	gbk_to_utf8_2(List, [Char | ResList]);
gbk_to_utf8_2([A, B| List], ResList) ->
	Utf8 = misc_encode_data_gbk2utf8:get([A, B]),
	NewResList = lists:reverse(Utf8) ++ ResList,
	gbk_to_utf8_2(List, NewResList).

utf8_to_gbk(Utf8) ->
	%% 先转unicode, 再转utf8
	U = to_unicode_string(Utf8),
	List = lists:map(fun(Unicode) -> 
							 misc_encode_data_unicode2gbk:get([Unicode])
					 end, U),
	lists:flatten(List).

%% ===============================================================
%% gbk 和 utf8 转换结束
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

%% 是否包含中文字符
%% return -> true | false
%% StrList::[UnicodeCodePoint|...]
%% 中、日、韩的三种文字占用了Unicode中2E80到0x9FFF的部分，判断是否带有中文的时候就可以判断字符的unicode codepoint是否在区间[11904, 40959]
%% 2E80—2EFF 中日韩部首补充
%% 2F00—2FDF 康熙部首
%% 2FF0—2FFF 表意文字描述符
%% 3000—303F 中日韩符号和标点
%% 3040—309F 日文平假名
%% 30A0—30FF 日文片假名
%% 3100—312F 注音字母
%% 3130—318F 谚文兼容字母
%% 3190—319F 象形字注释标志
%% 31A0—31BF 注音字母扩展
%% 31C0—31EF 中日韩笔画
%% 31F0—31FF 日文片假名语音扩展
%% 3200—32FF 带圈中日韩字母和月份
%% 3300—33FF 中日韩字符集兼容
%% 3400—4DBF 中日韩统一表意文字扩展A
%% 4DC0—4DFF 易经六十四卦符号
%% 4E00—9FFF 中日韩统一表意文字 
has_chinese(StrList) ->
	lists:any(fun(CodePoint) -> 
					  11904 =< CodePoint andalso CodePoint =< 40959 
			  end, StrList).
	
%% ====================================================================
%% Internal functions
%% ====================================================================
	
%% 打印二进制时用，每4位用空格分割
%% get_output_bin_str("00001111") -> "0000 1111".
get_output_bin_str(BinStr) ->
	%% 补齐位数
	BinStr1 = misc_str:fix_bit(BinStr, 4),
	Len = length(BinStr1),
	List = lists:map(fun(N) -> 
					  Start = N*4-3,
					  Stop = N * 4,
					  string:sub_string(BinStr1, Start, Stop)
			  end, lists:seq(1, erlang:trunc(Len/4))),
	string:join(List, " ").






