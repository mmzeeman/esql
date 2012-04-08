


-module(esql_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:fwrite(standard_error, "Starting esql.~n", []),
    esql_pool_sup:start_link().

stop(_State) ->
    ok.
