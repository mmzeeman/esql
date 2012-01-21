%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman

%% @doc Erlang Database Connection Framework

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

-module(esql).

-export([behaviour_info/1]).

-export([open/2, close/1, 
	 transaction/2, 
	 run/2, run/3, 
	 execute/2, execute/3, 
	 start_transaction/1, commit/1, rollback/1, 
	 tables/1, describe_table/2, column_names/2, 
	 call/2, call/3]).

-include("esql.hrl").

behaviour_info(callbacks) ->
    [{open, 1}, {close, 1},
     {start_transaction, 1}, {commit, 1}, {rollback, 1},
     {run, 3}, {execute, 3},
     {tables, 1}, {describe_table, 2}];
behaviour_info(_Other) ->
    undefined.

-record(connection, {driver, connection_data}).

%% @doc Open a connection to a new database
%%
open(Driver, Args) ->
    {ok, ConnectionData} = Driver:open(Args),
    {ok, #connection{driver=Driver, connection_data=ConnectionData}}.

%% @doc Close a database connection.
%%
close(#connection{driver=Driver, connection_data=Data}) ->
    Driver:close(Data).

%% @doc Start a transaction
start_transaction(#connection{driver=Driver, connection_data=Data}) ->
    Driver:start_transaction(Data).

%% @doc Commit all changes
commit(#connection{driver=Driver, connection_data=Data}) ->
    Driver:commit(Data).

%% @doc Rollback all changes
rollback(#connection{driver=Driver, connection_data=Data}) ->
    Driver:rollback(Data).

%% @doc Return a list with tablenames...
tables(#connection{driver=Driver, connection_data=Data}) ->
    Driver:tables(Data).

%% @doc 
describe_table(TableName, #connection{driver=Driver, connection_data=Data}) ->
    Driver:describe_table(TableName, Data).

%% @doc
column_names(TableName, Connection) ->
    [Info#esql_column_info.name || Info <- describe_table(TableName, Connection) ].
    
%% @doc Execute the statement, without returning results.
run(Sql, Connection) -> 
    run(Sql, [], Connection).

run(Sql, Params, #connection{driver=Driver, connection_data=Data}) ->
    Driver:run(Sql, Params, Data).

%% @doc Execute the statement, return the result
execute(Sql, Connection) ->
    execute(Sql, [], Connection).

execute(Sql, Params, #connection{driver=Driver, connection_data=Data}) ->
    Driver:execute(Sql, Params, Data).

%% @doc Support for driver specific extensions.
call(Function, Connection) ->
    call(Function, [], Connection).

call(Function, Args, #connection{driver=Driver, connection_data=Data}) ->
    apply(Driver, Function, Args ++ [Data]).

%% @doc 
%% 
transaction(F, Connection) ->
    try 
	throw_error(fun start_transaction/1, Connection),
	R = F(Connection),
	throw_error(fun commit/1, Connection),
	{ok, R}
    catch 
	_:Error ->
	    throw_error(fun rollback/1, Connection),
	    {rollback, Error}
    end.

throw_error(F, Connection) ->
    case apply(F, [Connection]) of
	ok ->
	    ok;
	{error, _} = Error ->
	    throw(Error)
    end.

    
