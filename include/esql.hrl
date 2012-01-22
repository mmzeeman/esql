%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2012 Maas-Maarten Zeeman

%% @doc Erlang esql database connectivity framework

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

% @doc The connection record, 'driver' is the module responsible the
% real database connection, 'data' holds the information needed by the
% driver.
-record(esql_connection, {driver, data}).

% @doc The column info contains: 'name' an atom containing the name of
% the column, 'type' the sql type of the column, 'default' the default
% value, 'notnull' is true when the column does not allow nulls, and
% 'pk' is set to true when the column is a primary key.
-record(esql_column_info, {name, type, default, notnull, pk}).
