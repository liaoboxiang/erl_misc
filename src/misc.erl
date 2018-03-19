%% @author box

-module(misc).

%% ====================================================================
%% API functions
%% ====================================================================
%% 随机数
-export([
		 rand/2,					%% 随机区间[MinInt, MaxInt]中的一个整数
		 rand_seed/0,				%% 设定随机种子
		 rand_from_list/1,			%% 从List中随机一个元素
		 rand_from_list/2,			%% 从List中随机N个元素
		 rand_from_list_no_repeat/2,%% 从List中随机N个不重复的元素
		 rand_weight/1,				%% List中按权重随机一个
		 list_shuffle/1,			%% 随机打乱List元素顺序(较快)
		 list_shuffle2/1			%% 随机打乱List元素顺序(换牌算法)
		]).

%% 计数器
-export([
		 counter_spwan/0,		%% 创建
		 counter_put/2,			%% 插入记录
		 counter_get/2,			%% 获取结果
		 counter_get_all/1,		%% 获取所有结果
		 counter_close/1		%%  关闭
		]).

%% 其他
-export([
		 get_ip/1,
		 md5/1
		]).

%% 测试
-export([
		 test/2
		]).

%% 在区间[MinInt, MaxInt]内（包含MinInt和MaxInt）随机一个整数
%% R18后 rand已经默认使用了随机种子
%% 如果想自己设定随机种子， 可参考?MODULE:rand_seed().
rand(MinInt, MaxInt) ->
	Diff = MaxInt - MinInt + 1,
    Random = rand:uniform(Diff),
    MinInt + Random - 1.

%% 随机数种子
rand_seed() ->
	<<A:32,B:32,C:32>> = crypto:strong_rand_bytes(12),
	%% builtin_alg()::exs64 | exsplus | exsp | exs1024 | exs1024s | exrop
	rand:seed(exs1024, {A,B,C}).

%% List中随机其中的一个元素
%% List中的元素等概率
rand_from_list(List) ->
    lists:nth(rand:uniform(length(List)), List).

%% List中随机N个元素(可重复) / List中随机1个元素，共随机N次
%% List中的元素等概率， 如 [1,1,2], 1的概率33%+33%，2的概率33%，
%% N::integer>=1
%% return -> [term()|...]
rand_from_list(List, N) ->
	lists:map(fun(_) -> 
					  rand_from_list(List) 
			  end, lists:seq(1, N)).

%% List中随机N个元素（不重复）
%% List中的不同元素等概率， 如[1,1,2] 1的概率50%， 2的概率50%
%% 当N小于可选元素的个数时，返回所有不同的元素，如rand_from_list_no_repeat([1,1,2], 3) -> [1,2]. 
%% 元素里有1.0和1这样的情况(A==B andalso A=/=B),这里认为是2个不同的元素（使用=:=判断）
rand_from_list_no_repeat(List, N) ->
	%% sets是不同元素的集合，任意元素满足A=/=B
	%% lists:usort的结果仅满足A/=B (lists:usort([1,1.0]). =>1)
	List1 = sets:to_list(sets:from_list(List)),
	Len = length(List1),
	case Len =< N of
		true -> List1;
		false -> rand_from_list_no_repeat2(List1,Len, N, [])
	end.

rand_from_list_no_repeat2(_List,_Len, 0, ResL) -> ResL;
rand_from_list_no_repeat2(List, Len, N, ResL) -> 
	Rand = rand(1, Len),
	E = lists:nth(Rand, List),
	NewList = lists:delete(E, List),
	rand_from_list_no_repeat2(NewList, Len-1, N-1, [E|ResL]).

%% 按权重随机1个
%% List :: [{Item, Weight}|...].
rand_weight(List) ->
	rand_weight1(List, 0, []).
%% 计算权重的和，再随机
rand_weight1([], Sum, L) ->
	Rand = ?MODULE:rand(1, Sum),
	rand_weight2(lists:reverse(L), Rand);
rand_weight1([{K,V}|T], Sum, L) ->
	rand_weight1(T, Sum+V, [{K,V+Sum}|L]).
rand_weight2([{K,N}|T], Rand) ->
	case Rand =< N of
		true -> K;
		false -> rand_weight2(T, Rand)
	end.

%% 随机打乱list元素顺序
%% 为每个元素添加一个随机次序，排序后获取最终结果
%% 较快
list_shuffle(L) ->
    List1 = [{rand:uniform(), X} || X <- L],
    List2 = lists:sort(List1), 
    [E || {_, E} <- List2].


%% 随机打乱list元素顺序
%% 随机交换洗牌算法（RandomExchangeShuffle）：遍历List，对第n个元素，以1/n的概率与前n个元素中的某个元素互换位置，最后生成的序列即满足要求
%% 比list_shuffle/1慢,大概是1~2倍的时间
%% 其余算法：全随机洗牌，随机抽取洗牌
list_shuffle2([]) ->
    [];
list_shuffle2(L = [_]) ->
    L;
list_shuffle2(L) ->
    shuffle1(1, L, length(L)).

shuffle1(Len, L, Len) ->
    L;
shuffle1(Cur, L, Len) ->
    S = rand:uniform(Len - Cur + 1) + Cur - 1, 
    L2 = swap_element(L, Cur, S),
    shuffle1(Cur + 1, L2, Len).

swap_element(L, N, N) ->
    L;
swap_element(L, N1, N2) ->
    Z = lists:zip(lists:seq(1, length(L)), L),
    {value, E1} = lists:keysearch(N1, 1, Z),
    {value, E2} = lists:keysearch(N2, 1, Z),
    Z2 = lists:keyreplace(N1, 1,
                    lists:keyreplace(N2, 1, Z, E1),
                    E2),
    {_, Result} = lists:unzip(Z2),
    Result. 

%% ====================================================================
%% 计数器进程
%% ====================================================================
%% 创建一个计数器进程
%% return -> Pid
counter_spwan() ->
	spawn(fun() -> counter_loop() end).

counter_loop() ->
	receive 
		{put, Key} -> 
			case get({data, Key}) of
				undefined -> put({data,  Key}, 1);
				Count ->  put({data, Key}, Count+1)
			end,
			counter_loop();
		{get, Key, FromPid} ->
			Num = 
			case get({data, Key}) of
				undefined -> 0;
				Count -> Count
			end,
			FromPid ! {counter, Key, Num},
			counter_loop();
		{get_all, FromPid} ->
			KeyList = erlang:get_keys(),
			List = lists:foldl(fun({data, Key}, L) -> 
									   Count = get({data, Key}),
									   [{Key, Count}|L];
								  (_, L) -> L
							   end, [], KeyList),
			FromPid ! {counter, List},
			counter_loop();
		{close} -> 
			ok
	end.

%% 放入数据
-spec counter_put(Pid::pid(), Key::term()) -> ok.
counter_put(Pid, Key) ->
	Pid ! {put, Key}.

%% 获取结果
%% return -> Num.
counter_get(Pid, Key) ->
	Pid ! {get, Key, self()},
	receive 
		{counter, Key, Num} -> 
			Num
	after 1000 ->
		io:format("count error, timeout"),
		0
	end.
	
%% 获取所有的结果
%% return -> [{key, Value}|...]
counter_get_all(Pid) -> 
	Pid ! {get_all, self()},
	receive 
		{counter, List} -> 
			List
	after 1000 ->
		io:format("count error, timeout"),
		[]
	end.

%% 关闭进程
counter_close(Pid) ->
	Pid ! {close}.

%% ====================================================================
%% 其他
%% ====================================================================
%% 获取Socket连接另一方的IP
%% return -> "127.0.0.1"
get_ip(Socket) when is_port(Socket) ->
	case inet:peername(Socket) of
		{ok, {Ip, _Port}} ->
			inet:ntoa(Ip);
		_ ->
			integer_to_list(0) ++ "." ++ integer_to_list(0) ++ "." ++ integer_to_list(0) ++ "." ++ integer_to_list(0)
	end;
%% Ip::ip_address()
%% ip_address()::{_,_,_,_}(ipv4) | {_,_,_,_,_,_}(ipv6)
get_ip(Ip) ->
	inet:ntoa(Ip).

%% 转换成32为16进制格式(小写)的md5
-spec md5(Term::string() | binary()) -> string().
%% md5("af") -> "f0357a3f154bc2ffe2bff55055457068"
md5(Str) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(Str))]).


%% ====================================================================
%% 测试
%% ====================================================================
%% N::运行次数， 
%% M::atom()模块名. F::atom()函数名, A::list()参数
%% test(100, {rand_from_list_no_repeat, [1,2,3,4,5,6]}) -> ResLits::list()
test(N, {F, A}) ->
	test(N, {?MODULE, F, A});
test(N, {M,F,A}) ->
	%% 创建计数器进程
	Pid = counter_spwan(),
	%% 开始统计时间
	erlang:statistics(runtime),
	%% 执行{M,F,A} N次
	lists:foreach(fun(_) -> 
						  Res = erlang:apply(M, F, A),
						  ?MODULE:counter_put(Pid, Res)
				  end, lists:seq(1, N)),
	{_, Time} = erlang:statistics(runtime),
	ResList = ?MODULE:counter_get_all(Pid),
	?MODULE:counter_close(Pid),
	%% 打印输出
	NStr = misc_str:split_string(erlang:integer_to_list(N), ",", 3, desc),
	io:format("run:~p times, use:~pms~n", [NStr,Time]),
	ResList.







