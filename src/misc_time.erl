%% @author box
%% @doc 时间日期相关

-module(misc_time).

-type year()	:: integer().
-type month()	:: integer().
-type day()		:: integer().
-type hour()	:: integer().
-type minute()	:: integer().
-type second()	:: integer().

-type unixtime():: integer().	%% unix timestamp， 时间戳
-type date()	:: {year(),month(),day()}.		
-type time()	:: {hour(), minute(), second()}.
-type datetime():: {date(), time()}.
-type datenum() :: integer().	%% 日期YYYYMMDD 20180316


-define(DIFF_SECONDS_0000_1900,	62167219200).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 now/0,					%% Unixtime
		 now_ms/0,				%% Unixtime ms
		 date_num/0,			%% 20180316
		 date_tuple/0,			%% {Y,M,D}
		 time/0,				%% {H, Min, S}
		 localtime/0,			%% {{Y,M,D}, {H, Min, S}}
		 unixtime_to_localtime/1,
		 unixtime_to_date_num/1,
		 week/0,				%% 星期几
		 week/1,				%% 星期几
		 week/3,				%% 星期几
		 get_diff_days/2,		%% 相差的天数
		 date_num_to_unixtime/1,	
		 date_time_to_unixtime/1,
		 midnight/0,			%% 获取当天的零点时间
		 next_daytime/0,		%% 距离明天还有多少秒 
		 next_daytime/1,
		 next_weektime/0,		%% 距离下周1零点还有多少秒
		 next_weektime/2,
		 next_monthtime/0,
		 next_monthtime/1
		]).


%% 获取现在的时间戳(秒)
-spec now() -> Unixtime::unixtime().
now() ->
    {A, B, _} = os:timestamp(),
    A * 1000000 + B.

%% 获取现在的时间戳(毫秒)
now_ms() ->
	{M, S, Ms} = os:timestamp(),
	M * 1000000000 + S * 1000 + Ms div 1000.

%% 得到现在日期--20180316
-spec date_num() -> DateNum::datenum().
date_num() ->
	{Y, M, D} 	= date_tuple(),
	Y * 10000 + M * 100 + D.

%% 得到现在日期{年,月,日}
date_tuple() ->
	{Date, _Time}	= unixtime_to_localtime(?MODULE:now()),
	Date.
 
%% 得到现在时间{时,分,秒}
-spec time() -> time().
time() ->
	{_Date, Time}	= unixtime_to_localtime(?MODULE:now()),	
	Time.

%% return -> datetime()
localtime() ->
	erlang:localtime().

%% ===================================================================
%% 根据1970年以来的秒数获得日期
%% ===================================================================
%% return -> {{2018,3,16},{15,16,17}}
-spec unixtime_to_localtime(UnixTime::unixtime()) -> Localtime::datetime().
unixtime_to_localtime(UnixTime) ->
	DateTime = calendar:gregorian_seconds_to_datetime(UnixTime + ?DIFF_SECONDS_0000_1900),
	calendar:universal_time_to_local_time(DateTime).
%% 根据秒数获得日期 --20120630
unixtime_to_date_num(UnixTime) ->
	{{Y, M, D}, {_, _, _}} = unixtime_to_date_num(UnixTime),
	Y * 10000 + M * 100 + D.

%% 得到今天是星期几
%% return -> 1~7
week() ->
	{Y,M,D} = date_tuple(),
	week(Y,M,D).

%% 得到Y年M月D日是星期几
week({Y,M,D}) ->
	week(Y,M,D).

%% 得到Y年M月D日是星期几
%% week(2018,3,16) -> 5. 
week(Y, M, D) -> 
	calendar:day_of_the_week(Y,M,D).

%% 计算相差的天数
%% return -> Days::integer().
get_diff_days(Seconds1, Seconds2) ->
	{{Year1, Month1, Day1}, _} = unixtime_to_localtime(Seconds1),
	{{Year2, Month2, Day2}, _} = unixtime_to_localtime(Seconds2),
	Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
	Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
	abs(Days2-Days1).

%% 日期转化为当天0点秒数
%% date_to_unixtime(20180316) ->1521129600. 
date_num_to_unixtime(Date) ->
	Y = Date div 10000,
	M = (Date - 10000 * Y) div 100,
	D = (Date - 10000 * Y - 100 * M),
	date_time_to_unixtime({{Y, M, D}, {0, 0, 0}}).

%% {{Y, M, D}, {H, Min, S}} -> Unixtime.
date_time_to_unixtime({{Y, M, D}, {H, Min, S}}) ->
	[UniversalTime]	= calendar:local_time_to_universal_time_dst({{Y, M, D}, {H, Min, S}}),
	Seconds			= calendar:datetime_to_gregorian_seconds(UniversalTime),
	TimeGMT			= ?DIFF_SECONDS_0000_1900,
	Seconds - TimeGMT.


%% @doc 获取当天的零点时间
%% now:{{2018,3,16}, {16,0,0}}(unixtime:1521187200), 零点为{{2018,3,16}, {0,0,0}}(1521129600)
%% return -> UnixTime
midnight() ->
	{Hour, Min, Seconds} = erlang:time(),
	?MODULE:now() - calendar:time_to_seconds({Hour, Min, Seconds}).

%% @doc hour时,Min,Second分钟距离明天还有多少秒
next_daytime() -> 
	Time = erlang:time(),
	next_daytime(Time).
-spec next_daytime(Time::time()) -> Seconds::integer().
next_daytime({Hour, Min, Seconds}) ->
    86400 - (calendar:time_to_seconds({Hour, Min, Seconds})).

%% @doc 判断WeekDay,hour,min,second 距离下周1零点还有多少秒
next_weektime() -> 
	Week = week(),
	Time = erlang:time(),
	next_weektime(Week, Time).
next_weektime(WeekDay, {Hour, Min, Seconds}) ->
	(7 - WeekDay) * 86400 + next_daytime({Hour, Min, Seconds}).

%% @doc 判断Day,hour,min,second 距离下个月1号还有多少秒
next_monthtime() ->
	Date = erlang:date(),
	Time = erlang:time(),
	next_monthtime({Date, Time}).
next_monthtime({{Year, Month, Day}, {Hour, Min, Seconds}}) ->
	TotalDay = calendar:last_day_of_the_month(Year, Month),
	(TotalDay - Day) * 86400 + next_daytime({Hour, Min, Seconds}). 

%% ====================================================================
%% Internal functions
%% ====================================================================


