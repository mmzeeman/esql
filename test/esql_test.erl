%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman

%% @doc Tests

%% Copyright 2012 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(esql_test).

-include_lib("eunit/include/eunit.hrl").

open_test() ->
    {ok, C} = esql:open(dummy_driver, []),
    [] = esql:call(info, C),
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

    {ok, test123} = esql:transaction(F, C),

    Calls = esql:call(info, C),
    [start_transaction, {run, "test", [1]}, {run, "test", [2]}, {run, "test", [3]}, commit] = Calls,
    
    ok.

transaction_error_test() ->
    {ok, C} = esql:open(dummy_driver, []),

    F = fun(Conn) ->
		esql:run("test", [1], Conn),
		esql:run("test", [2], Conn),
		esql:run("test", [3], Conn),
		throw(test_error)
	end,

    %% Errors are transformed into error tuples
    {rollback, _Error} = esql:transaction(F, C),

    Calls = esql:call(info, C),
    [start_transaction, {run, "test", [1]}, {run, "test", [2]}, {run, "test", [3]}, rollback] = Calls,
    
    ok.

    
		
    
    
	
