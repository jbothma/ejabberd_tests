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
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [%{group, auth},
     {group, vcard}].

groups() ->
    [{auth, [], [
                 login,
                 login_negative,
                 login_fail_filter,
                 login_fail_dn_filter,
                 login_fail_local_filter
                ]},
     {vcard, [], [
                  bob
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

login(_Config) ->
    John = [
            {username, <<"john">>},
            {server, <<"example.com">>},
            {password, <<"johnldap">>},
            {host, <<"localhost">>}],
    {ok, _, _} = escalus_connection:start(John).

login_negative(_Config) ->
    JohnBad = [
               {username, <<"john">>},
               {server, <<"example.com">>},
               {password, <<"wrong_password">>},
               {host, <<"localhost">>}],
    {error, _} = escalus_connection:start(JohnBad).

login_fail_filter(_Config) ->
    %% Claire fails on uid: claire
    ClaireBad = [
                    {username, <<"claire">>},
                    {server, <<"example.com">>},
                    {password, <<"claireldap">>},
                    {host, <<"localhost">>}],
    {error, _} = escalus_connection:start(ClaireBad).

login_fail_dn_filter(_Config) ->
    %% Ann fails on cn =/= displayName
    AnnBad = [
                    {username, <<"ann">>},
                    {server, <<"example.com">>},
                    {password, <<"annldap">>},
                    {host, <<"localhost">>}],
    {error, _} = escalus_connection:start(AnnBad).

login_fail_local_filter(_Config) ->
    %% Tom fails on description: inactive
    TomBad = [
                    {username, <<"tom">>},
                    {server, <<"example.com">>},
                    {password, <<"tomldap">>},
                    {host, <<"localhost">>}],
    {error, _} = escalus_connection:start(TomBad),
    %% Mark fails on shadowFlag: 1
    MarkBad = [
                    {username, <<"mark">>},
                    {server, <<"example.com">>},
                    {password, <<"markldap">>},
                    {host, <<"localhost">>}],
    {error, _} = escalus_connection:start(MarkBad).

%%--------------------------------------------------------------------
%% VCard Test cases
%%--------------------------------------------------------------------

bob(Config) ->
    _Config = [{escalus_users,
               [{john,
                 [{username, <<"john">>},
                  {server, <<"example.com">>},
                  {password, <<"johnldap">>}
                 ]}
               ]}
             ],
    escalus:story(
      Config, [{john, 1}],
      fun(John) ->
              IQGet = escalus_stanza:iq(
                        <<"get">>, [#xmlelement{
                                       name = <<"vCard">>,
                                       attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
                                       children = []
                                      }]),
              ct:pal("~p~n",[IQGet]),
              escalus:send(John, IQGet),
              Stanza = escalus:wait_for_stanza(John),
              %%escalus_new_assert:assert(is_sic_response(), Stanza)
              ct:pal("~p~n",[Stanza])
      end).
