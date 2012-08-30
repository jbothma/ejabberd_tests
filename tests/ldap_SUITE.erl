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

-module(ldap_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, login}].

groups() ->
    [{login, [sequence], [login]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Users = {escalus_users, [{john, [
                                     {username, <<"john">>},
                                     {server, <<"example.com">>},
                                     {password, <<"johnldap">>},
                                     {host, <<"localhost">>}]}
                            ]},
    Config2 = proplists:delete(escalus_users, Config),
    escalus:init_per_suite([Users|Config2]).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

login(Config) ->
    escalus:story(Config, [1], fun(John) ->
        escalus_client:send(John, escalus_stanza:chat_to(John, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>], escalus_client:wait_for_stanza(John))
    end).
