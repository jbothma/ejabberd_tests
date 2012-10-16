%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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
%%==============================================================================

-module(tls_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [run_cases].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(_Config) ->
    Users = ct:get_config(escalus_tls_users),
    escalus:create_users([{escalus_users, Users}]).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

run_cases(_Config) ->
    [{_,UserConf}] = ct:get_config(escalus_tls_users),
    lists:map(fun({PortOpt,SSLOpt, TestFun}=Case) ->
                      case catch ?MODULE:TestFun([PortOpt,SSLOpt|UserConf]) of
                          ok -> ok;
                          Error ->
                              ct:pal("Error in case ~p~n", [Case]),
                              throw(Error)
                      end
              end,
              cases()).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

start_ok(UserConf) ->
    {ok, _, _} = escalus_connection:start(UserConf),
    ok.

start_error(UserConf) ->
    case catch escalus_connection:start(UserConf) of
        {ok, _, _} -> exit({error, "connection shouldn't succeed"});
        _ -> ok
    end.

cases() ->
    [{tls_required_port(), {ssl, false}, start_error},
     {tls_required_port(), {ssl, required}, start_ok},
     {tls_optional_port(), {ssl, required}, start_ok},
     {tls_optional_port(), {ssl, false},  start_ok},
     {no_tls_port(), {ssl, required}, start_error},
     {no_tls_port(), {ssl, false}, start_ok}].

tls_required_port() ->
    {port, 5226}.

tls_optional_port() ->
    {port, 5225}.

no_tls_port() ->
    {port, 5224}.
