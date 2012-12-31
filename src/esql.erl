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
         execute/4,
         step/1, 
         start_transaction/1, commit/1, rollback/1, 
         table_exists/2, tables/1, describe_table/2, column_names/2, 
         call/2, call/3]).

% higher level api...
-export([map/3, map/4]).

-include("esql.hrl").

behaviour_info(callbacks) ->
    [{open, 1}, 
     {close, 1},
     {start_transaction, 1}, 
     {commit, 1}, 
     {rollback, 1},
     {run, 3}, 
     {execute, 3},
     {execute, 4},
     {table_exists, 2},
     {tables, 1}, 
     {describe_table, 2}];
behaviour_info(_Other) ->
    undefined.


-type connection() :: tuple().
-type driver() :: module().
-type name() :: atom() | string() | binary().
-type sql() :: iolist().
-type description() :: term().

%% @doc Open a connection to the database 
%%
-spec open(driver(), list()) -> {ok, connection()} | {error, _}.
open(Driver, Args) ->
    {ok, Data} = Driver:open(Args),
    {ok, #esql_connection{driver=Driver, data=Data}}.

%% @doc Close the connection.
%%
-spec close(connection()) -> ok | {error, _}.
close(#esql_connection{driver=Driver, data=Data}) ->
    Driver:close(Data).

%% @doc Start a transaction
-spec start_transaction(connection()) -> ok | {error, _}.
start_transaction(#esql_connection{driver=Driver, data=Data}) ->
    Driver:start_transaction(Data).

%% @doc Close the transaction and store all changes.
-spec commit(connection()) -> ok | {error, _}.
commit(#esql_connection{driver=Driver, data=Data}) ->
    Driver:commit(Data).

%% @doc Close the transaction and rollback all changes.
-spec rollback(connection()) -> ok | {error, _}.
rollback(#esql_connection{driver=Driver, data=Data}) ->
    Driver:rollback(Data).

%% @doc Returns true iff the table with Name exists.
-spec table_exists(name(), connection()) -> true | false.
table_exists(Name, #esql_connection{driver=Driver, data=Data}) ->
    Driver:table_exists(Name, Data).

%% @doc Return a list with tablenames.
-spec tables(connection()) -> list(atom()).
tables(#esql_connection{driver=Driver, data=Data}) ->
    Driver:tables(Data).

%% @doc Return a description of table Name.
-spec describe_table(name(), connection()) -> description().
describe_table(TableName, #esql_connection{driver=Driver, data=Data}) ->
    Driver:describe_table(TableName, Data).

%% @doc Get the column names of table Name.
-spec column_names(name(), connection()) -> list(atom()).
column_names(Name, Connection) ->
    [Info#esql_column_info.name || Info <- describe_table(Name, Connection)].
    
%% @doc Execute the statement, without returning results.
-spec run(sql(), connection()) -> ok | {error, _}.
run(Sql, Connection) -> 
    run(Sql, [], Connection).

-spec run(sql(), list(), connection()) -> ok | {error, _}.
run(Sql, Params, #esql_connection{driver=Driver, data=Data}) ->
    Driver:run(Sql, Params, Data).

%% @doc Execute the statement, return the result
-spec execute(sql(), connection()) -> {ok, list(atom()), list(tuple)} | {error, _}.
execute(Sql, Connection) ->
    execute(Sql, [], Connection).

-spec execute(sql(), list(), connection()) -> {ok, list(atom()), list(tuple)} | {error, _}.
execute(Sql, Params, #esql_connection{driver=Driver, data=Data}) ->
    Driver:execute(Sql, Params, Data).

%% @doc Execute the statement, and return the rows, one by one to the receiver.
%% Testing if this is a good api for asynchronous returning query results.
%% Returns {ok, Ref}
%% Receiver, pid
%% start... -> more , row -> more, row -> more, row, end...
%% Experimental interface.
execute(Sql, Params, Receiver, #esql_connection{driver=Driver, data=Data}) ->
    Driver:execute(Sql, Params, Receiver, Data).

%% @doc Retrieve the next answer.
%%
step(Ref) when is_pid(Ref) ->
    receive 
        {Ref, What, Item} when What =:= row; What =:= column_names; What =:= error->
            Ref ! continue,
            {What, Item};
        {Ref, done} ->
            done;
        Something ->
            {error, unexpected_answer, Something}
    end.
        

%% @doc Support for driver specific extensions.
call(Function, Connection) ->
    call(Function, [], Connection).

call(Function, Args, #esql_connection{driver=Driver, data=Data}) ->
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

%% @doc 
map(F, Sql, Connection) ->
    map(F, Sql, [], Connection).

map(F, Sql, Parameters, Connection) ->
    %% Push this to the driver for a streaming api.
    case execute(Sql, Parameters, Connection) of 
        {ok, Names, Rows} ->
            {ok, map_result(F, Names, Rows)};
        Other -> 
            Other
    end.

map_result(F, _Names, Rows) when is_function(F, 1) ->
    [F(Row) || Row <- Rows];
map_result(F, Names, Rows) when is_function(F, 2) ->
    [F(Names, Row) || Row <- Rows].

throw_error(F, Connection) ->
    case apply(F, [Connection]) of
        ok -> ok;
        {error, _}=Error -> throw(Error)
    end.
