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
    [{vcard, [], [bob,
                  retrieve_own_card
                  ,card_doesnt_exist
                  ,update_card
                  ,retrieve_others_card
                  ,service_discovery
                 ]}
    ].

suite() ->
    escalus:suite().

%% Element CData
-define(EL_CD(Element, Name), exml_query:path(Element, [{element, Name}, cdata])).

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
              ct:pal("~p~n",[Stanza]),

              escalus_pred:is_iq(<<"result">>, Stanza),

              VCard = exml_query:path(Stanza, [{element, <<"vCard">>}]),
              <<"john">> = ?EL_CD(VCard, <<"NICKNAME">>),
              <<"Doe, John">> = ?EL_CD(VCard, <<"FN">>),
              <<"1966-08-06">> = ?EL_CD(VCard, <<"BDAY">>),
              <<"john@example.com">> = ?EL_CD(VCard, <<"JABBERID">>),
              <<"Executive Director">> = ?EL_CD(VCard, <<"TITLE">>),
              <<"Patron Saint">> = ?EL_CD(VCard, <<"ROLE">>),
              <<"I am sam.">> = ?EL_CD(VCard, <<"DESC">>),
              <<"http://john.doe/">> = ?EL_CD(VCard, <<"URL">>),


              EMAIL = exml_query:path(VCard, [{element, <<"EMAIL">>}]),
              <<"john@mail.example.com">> = ?EL_CD(EMAIL, <<"USERID">>),

              N = exml_query:path(VCard, [{element, <<"N">>}]),
              <<"Doe">> = ?EL_CD(N, <<"FAMILY">>),
              <<"John">> = ?EL_CD(N, <<"GIVEN">>),
              <<"E.">> = ?EL_CD(N, <<"MIDDLE">>),

              ADR = exml_query:path(VCard, [{element, <<"ADR">>}]),
              <<"1899 Wynkoop Street">> = ?EL_CD(ADR, <<"STREET">>),
              <<"Denver">> = ?EL_CD(ADR, <<"LOCALITY">>),
              <<"CO">> = ?EL_CD(ADR, <<"REGION">>),
              <<"91210">> = ?EL_CD(ADR, <<"PCODE">>),
              <<"US of A">> = ?EL_CD(ADR, <<"CTRY">>),

              TEL = exml_query:path(VCard, [{element, <<"TEL">>}]),
              <<"303-308-3282">> = ?EL_CD(TEL, <<"NUMBER">>)
      end).



%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------
