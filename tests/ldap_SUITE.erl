%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
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
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, auth}
     ,{group, eunit}].

groups() ->
    [{auth, [], [
                 login,
                 login_negative,
                 login_fail_filter,
                 login_fail_dn_filter,
                 login_fail_local_filter
                ]},
     {eunit, [], [
                  ejabberd_ldap_utils
                  ]}
    ].


suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Authentication Test cases
%%--------------------------------------------------------------------

login(Config) ->
    John = get_ldap_user(valid, Config),
    {ok, _, _} = escalus_connection:start(John).

login_negative(Config) ->
	JohnBad = get_ldap_user(fail_passwd, Config),
    {error, _} = escalus_connection:start(JohnBad).

login_fail_filter(Config) ->
    %% Claire fails on uid: claire
    ClaireBad = get_ldap_user(fail_filter, Config),
    {error, _} = escalus_connection:start(ClaireBad).

login_fail_dn_filter(Config) ->
    %% Ann fails on cn =/= displayName
    AnnBad = get_ldap_user(fail_dn_filter, Config),
    {error, _} = escalus_connection:start(AnnBad).

login_fail_local_filter(Config) ->
    %% Tom fails on description: inactive
    TomBad = get_ldap_user(fail_local_filter1, Config),
    {error, _} = escalus_connection:start(TomBad),
    %% Mark fails on shadowFlag: 1
    MarkBad = get_ldap_user(fail_local_filter2, Config),
    {error, _} = escalus_connection:start(MarkBad).


%%--------------------------------------------------------------------
%% drive module unit tests from CT
%%
%% This is a bit hacky but saves making ejd releases including eunit.
%% It works as long as the test only depends on the module in question.
%%--------------------------------------------------------------------

ejabberd_ldap_utils(_Config) ->
    Mod = ejabberd_ldap_utils,
    {Mod, Bin, FName} = escalus_ejabberd:rpc(code, get_object_code, [Mod]),
    {module, Mod} = code:load_binary(Mod, FName, Bin),
    ok = Mod:test().


%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

get_ldap_user(Key, Config) ->
	element(2, lists:keyfind(
                     Key, 1, escalus_config:get_config(
                               escalus_ldap_users, Config, []))).
