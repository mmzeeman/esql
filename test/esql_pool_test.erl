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

-module(esql_pool_test).

-include_lib("eunit/include/eunit.hrl").

pool_test_() ->
    {inparallel,
     {setup, fun() -> application:start(esql) end, fun(_) -> application:stop(esql) end,
      [{"start and stop",
         fun() -> ok end
       },

       {"create a new pool",
        fun() -> ?assertMatch({ok, _Pid}, 
                              esql_pool:create_pool(test_pool, 10, [{driver, dummy_driver}, {args, []}]))
        end
       }
      ]
     }
    }.
	
