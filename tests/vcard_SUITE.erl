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

-module(vcard_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, vcard}].

groups() ->
    [{vcard, [], [
                  bob
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
%% VCard Test cases
%%--------------------------------------------------------------------

bob(Config) ->
    escalus:story(
      Config, [{valid, 1}],
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

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

