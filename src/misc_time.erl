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

-type division_time() :: integer().	%% 分割的时间点(整点 小时),  0=< DivisionTime =< 23

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
		 week/1,				
		 week/3,				
		 get_diff_days_by_unixtime/2,	%% 相差的天数
		 get_diff_days_by_datenum/2,	
		 get_diff_days_by_date/2,		
		 date_num_to_date/1,		%% {Y,M,D} -> YYYYMMDD
		 date_num_to_unixtime/1,	%% {Y,M,D} -> Unixtime
		 date_time_to_unixtime/1,	%% {{Y,M,D}, {H,Min,S}} -> Unixtime
		 midnight/0,			%% 获取当天的零点时间
		 next_daytime/0,		%% 距离明天还有多少秒 
		 next_daytime/1,
		 next_weektime/0,		%% 距离下周1零点还有多少秒
		 next_weektime/2,
		 next_monthtime/0,		%% 距离下个月1号还有多少秒 
		 next_monthtime/1
		]).

-export([
		 get_date_num_by_time_division/1,
		 get_date_num_by_time_division/2,
		 get_diff_days_by_time_division/3,
		 get_today_left_time_by_time_division/1
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
	{{Y, M, D}, {_, _, _}} = unixtime_to_localtime(UnixTime),
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
get_diff_days_by_unixtime(UnixTime1, UnixTime2) ->
	{Date1, _} = unixtime_to_localtime(UnixTime1),
	{Date2, _} = unixtime_to_localtime(UnixTime2),
	get_diff_days_by_date(Date1, Date2).

get_diff_days_by_datenum(Datenum1, Datenum2) ->
	Date1 = date_num_to_date(Datenum1),
	Date2 = date_num_to_date(Datenum2),
	get_diff_days_by_date(Date1, Date2).

get_diff_days_by_date({Y1,M1,D1}, {Y2,M2,D2}) ->
	Days1 = calendar:date_to_gregorian_days(Y1,M1,D1),
	Days2 = calendar:date_to_gregorian_days(Y2,M2,D2),
	abs(Days2-Days1).

%% 20180316 -> {2018, 3, 16}.
date_num_to_date(Datenum) ->
	Y = Datenum div 10000,
	M = (Datenum - 10000 * Y) div 100,
	D = (Datenum - 10000 * Y - 100 * M),
	{Y, M, D}.

%% 日期转化为当天0点秒数
%% date_num_to_unixtime(20180316) ->1521129600. 
date_num_to_unixtime(Datenum) ->
	Date = date_num_to_date(Datenum),
	date_time_to_unixtime({Date, {0, 0, 0}}).

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
%% 以某个时间点来划分天 
%% 如 4点前是前一天， 4点后是另一天
%% _by_time_division
%% ====================================================================
%% 以DTime为分割线区分日期
%% DTime点前为前一天， DTime点后是第二天
-spec get_date_num_by_time_division(DTime::division_time()) -> Datenum::datenum().
get_date_num_by_time_division(DTime) ->
	{H,_,_}	= ?MODULE:time(),
	case H < DTime of
		true ->
			UuixTime		= ?MODULE:now(),
			UuixTime1		= UuixTime - 86400,
			unixtime_to_date_num(UuixTime1);
		_ -> 
			?MODULE:date_num()
	end.

get_date_num_by_time_division(DTime, UnixTime) ->
	{{Y, M, D}, {H, _, _}} = unixtime_to_localtime(UnixTime),
	case H >= DTime of
		true -> 
			Y * 10000 + M * 100 + D;
		false ->
			Time = UnixTime - 86400,
			unixtime_to_date_num(Time)
	end.

%% 以DTime点未分割，相隔多少天
get_diff_days_by_time_division(DTime, UnixTime1, UnixTime2) ->
	Datenum1 = get_date_num_by_time_division(DTime, UnixTime1),
	Datenum2 = get_date_num_by_time_division(DTime, UnixTime2),
	get_diff_days_by_datenum(Datenum1, Datenum2).

%% 今天的剩余时间
get_today_left_time_by_time_division(DTime) ->
	{H, M, S} = ?MODULE:time(),
	case H < DTime of
		true -> DTime * 3600 - (H*3600 + M*60 + S);
		false -> DTime * 3600 + 86400 - (H*3600 + M*60 + S)
	end.



%% ====================================================================
%% Internal functions
%% ====================================================================


