%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman
%%
%% @doc Erlang Database Connection Framework
%%
%% Copyright 2012 Maas-Maarten Zeeman
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(esql_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {esql_connection}).

% @doc Start the poolboy worker connection
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

% @doc Initiate the connection.
init(Opts) ->
    process_flag(trap_exit, true),
   
    Connection = case proplists:get_value(connection, Opts) of
        undefined -> esql_pool:open_esql_connection(Opts);
        C -> C
    end,

    {ok, #state{esql_connection=Connection}}.

% @doc ...
handle_call({run, Sql, Params}, From, #state{esql_connection=Conn}=State) ->
    spawn_link(fun() ->
        gen_server:reply(From, esql:run(Sql, Params, Conn))
    end),
    {noreply, State};
handle_call({execute, Sql, Params}, From, #state{esql_connection=Conn}=State) ->
    spawn_link(fun() -> 
        gen_server:reply(From, esql:execute(Sql, Params, Conn))
    end),
    {noreply, State};
handle_call({transaction, F}, From, #state{esql_connection=Conn}=State) ->
    spawn_link(fun() ->
        gen_server:reply(From, esql:transaction(F, Conn))
    end),
    {noreply, State};
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

% @doc ...
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

% @doc ...
handle_info(stop, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    {noreply, State}.

% @doc Close the connection
terminate(_Reason, #state{esql_connection=Conn}) ->
    esql:close(Conn),
    ok.

% @doc 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
