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

-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, auth}
     ,{group, inband_password}
     ,{group, eunit}].

groups() ->
    [{auth, [], [login,
                 login_negative,
                 login_fail_filter,
                 login_fail_dn_filter,
                 login_fail_local_filter
                ]},
     {inband_password, [], [change_password]},
     {eunit, [], [ejabberd_ldap_utils
                  ]}
    ].


suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Users = escalus_config:get_config(escalus_ldap_users, Config, []),
    NewConfig = lists:keystore(escalus_users, 1, Config, {escalus_users, Users}),
    escalus:init_per_suite(NewConfig).

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
    John = get_ldap_user(user1, Config),
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
%% XEP-0077: In-Band Registration Test cases
%% mod_vcard_ldap only supports password change
%%--------------------------------------------------------------------

change_password(Config) ->
    ServJID = <<"example.com">>,
    escalus:story(
      Config, [{user1, 1}],
      fun(User) ->
              test_change_password(ServJID, User, <<"john_changed_password">>)
      end),
    escalus:story(
      Config, [{valid_other_pwd, 1}],
      fun(User) ->
              test_change_password(ServJID, User, <<"johnldap">>)
      end).

test_change_password(ServerJID, Client, NewPassword) ->
    Username = escalus_client:username(Client),
    Children = [#xmlelement{ name = <<"username">>,
                             children = {xmlcdata, Username}},
                #xmlelement{ name = <<"password">>,
                             children = {xmlcdata, NewPassword}}],
    Query = escalus_stanza:query_el(?NS_INBAND_REGISTER, Children),
    IQSet = escalus_stanza:iq(ServerJID, <<"set">>, [Query]),
    escalus:send(Client, IQSet),
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(is_iq_result, Stanza).

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
