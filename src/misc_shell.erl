%% @author box
%% @doc @todo Add description to misc_shell.


-module(misc_shell).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 print/2
		]).

%% 在shell 上打印record
%% 前提：需要先把record写入到ets表shell_records中
%% 写入方法：rd(R,D) | rr(File) | user_default.erl
%% e.g
%% > rd(r_test, {a, b}).
%% > misc_shell:print("~p:~w", [record, {t_test,1,1}]).
print(Format, Args) -> 
	Format1 = re:replace(Format, "~p|~w|~s","~ts", [{return,list},global]),
	Args1 = [format_print(Term) || Term<-Args],
	Msg = lists:flatten(io_lib:format(Format1, Args1)),
	io:format("~ts~n", [Msg]).

	

%% ====================================================================
%% Internal functions
%% ====================================================================
format_print(Term) ->
	Enc = [{encoding,unicode}],
	TabId = get_ets_shell_records_id(),
	Cs = io_lib_pretty:print(Term, ([{column, 1}, {line_length, 80},
							 {depth, -1}, {max_chars, 60},
							 {strings, true},
							 {record_print_fun, record_print_fun(TabId)}]
								++ Enc)),
	Cs.

%% 获得ets表shell_records的ID(表不是named_table,只能使用id)
get_ets_shell_records_id() ->
	List = ets:all(),
	[TabID] = lists:filter(fun(Tab) -> 
						 ets:info(Tab, name) == shell_records 
				 end, List),
	TabID.
record_print_fun(RT) ->
    fun(Tag, NoFields) ->
            case ets:lookup(RT, Tag) of
                [{_,{attribute,_,record,{Tag,Fields}}}] 
                                  when length(Fields) =:= NoFields ->
                    record_fields(Fields);
                _ ->
                    no
            end
    end.

record_fields([{record_field,_,{atom,_,Field}} | Fs]) ->
    [Field | record_fields(Fs)];
record_fields([{record_field,_,{atom,_,Field},_} | Fs]) ->
    [Field | record_fields(Fs)];
record_fields([]) ->
    [].