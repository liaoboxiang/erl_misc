%% @author box
%% @doc 字符串 列表整理


-module(misc_str).

-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
-export([fix_bit/2]).

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

%% ====================================================================
%% Internal functions
%% ====================================================================


