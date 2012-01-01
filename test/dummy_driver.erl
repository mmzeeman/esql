%%
%%

-module(dummy_driver).

-include_lib("esql/include/esql.hrl").

% esql callbacks
-export([open/1,  close/1, 
	 start_transaction/1, commit/1, rollback/1, 
	 describe_table/2, tables/1, 
	 run/3, execute/3, 
	 info/1]).

-behaviour(esql).

-record(state, {calls=[]}).

open(_Args) ->
    Pid = erlang:spawn(fun() -> connection_loop(#state{}) end),
    {ok, Pid}.

connection_loop(#state{calls=Calls}=State) ->
    receive
	stop ->
	    ok;
	{info, R} ->
	    R ! lists:reverse(Calls);
	Msg -> 
	    Calls1 = [Msg | Calls],
	    connection_loop(State#state{calls=Calls1})
    end.

close(Pid) ->
    Pid ! stop,
    ok.

start_transaction(Pid) ->
    Pid ! start_transaction,
    ok.

commit(Pid) ->
    Pid ! commit,
    ok.

rollback(Pid) ->
    Pid ! rollback,
    ok.

tables(_) ->
    [].

describe_table(_Table, _Pid) ->
    undefined.

execute(Query, Args, Pid) ->
    Pid ! {execute, Query, Args},
    ok.

run(Query, Args, Pid) ->
    Pid ! {run, Query, Args},
    ok.
    
% Return the list with calls.
info(Pid) ->
    Pid ! {info, self()},
    receive 
	Msg -> Msg
    end.
    
