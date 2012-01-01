%%
%%

-module(esql_test).

-include_lib("eunit/include/eunit.hrl").

open_test() ->
    {ok, _C} = esql:open(dummy_driver, []),
    ok.

open_close_test() ->
    {ok, C} = esql:open(dummy_driver, []),
    ok = esql:close(C),
    ok.
    
transaction_test() ->
    {ok, C} = esql:open(dummy_driver, []),

    F = fun(Conn) ->
		esql:run("test", [1], Conn),
		esql:run("test", [2], Conn),
		esql:run("test", [3], Conn),
		test123
	end,

    test123 = esql:transaction(F, C),

    Calls = esql:call(info, C),
    [{run, "test", [1]}, {run, "test", [2]}, {run, "test", [3]}, commit] = Calls,
    
    ok.

transaction_error_test() ->
    {ok, C} = esql:open(dummy_driver, []),

    F = fun(Conn) ->
		esql:run("test", [1], Conn),
		esql:run("test", [2], Conn),
		esql:run("test", [3], Conn),
		throw(test_error)
	end,

    catch esql:transaction(F, C),

    Calls = esql:call(info, C),
    [{run, "test", [1]}, {run, "test", [2]}, {run, "test", [3]}, rollback] = Calls,
    
    ok.

    
		
    
    
	
