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
%%

-module(esql_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([child_spec/3, create_pool/3, delete_pool/1]).

-export([init/1]).

% Start the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% @doc Create a childspec for a new connection pool.
child_spec(PoolName, Size, Options) ->
    {PoolName, {poolboy, start_link, [[{name, {local, PoolName}},
                                                  {worker_module, esql_worker},
                                                  {size, Size},
                                                  {max_overflow, 10}]
                                                 ++ Options
                                                ]},
                permanent, 5000, worker, [poolboy, esql_worker]}.


% @doc Create a new connection pool
create_pool(PoolName, Size, Options) ->
    supervisor:start_child(?MODULE, child_spec(PoolName, Size, Options)). 

% @doc Delete the specified connection pool.
delete_pool(Name) ->
    case supervisor:terminate_child(?MODULE, Name) of
        ok -> supervisor:delete_child(?MODULE, Name);
        {error, _}=Error -> Error
    end.  

% @doc Initialize an empty supervisor waiting for new pools.
init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

