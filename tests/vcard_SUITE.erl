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

-include_lib("escalus/include/escalus_xmlns.hrl").
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
                  retrieve_own_card
                  ,user_doesnt_exist
                  ,update_card
                  ,filtered_user_is_nonexistent
                  ,retrieve_others_card
                  ,service_discovery
                  ,server_vcard
                  ,directory_service_vcard
                 ]}
    ].

suite() ->
    escalus:suite().

%% Element CData
-define(EL(Element, Name), exml_query:path(Element, [{element, Name}])).
-define(EL_CD(Element, Name), exml_query:path(Element, [{element, Name}, cdata])).
-define(PHOTO_B64_MD5, <<41,39,104,189,75,25,191,200,129,237,251,129,91,76,195,
  162>>).

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

retrieve_own_card(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              IQGet = escalus_stanza:iq(
                        <<"get">>, [#xmlelement{
                                       name = <<"vCard">>,
                                       attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
                                       children = []
                                      }]),
              escalus:send(John, IQGet),
              Stanza = escalus:wait_for_stanza(John),

              escalus_pred:is_iq(<<"result">>, Stanza),

              VCard = ?EL(Stanza, <<"vCard">>),
              <<"john">> = ?EL_CD(VCard, <<"NICKNAME">>),
              <<"Doe, John">> = ?EL_CD(VCard, <<"FN">>),
              <<"john@example.com">> = ?EL_CD(VCard, <<"JABBERID">>),
              <<"Executive Director">> = ?EL_CD(VCard, <<"TITLE">>),
              <<"Patron Saint">> = ?EL_CD(VCard, <<"ROLE">>),
              <<"active">> = ?EL_CD(VCard, <<"DESC">>),
              <<"http://john.doe/">> = ?EL_CD(VCard, <<"URL">>),

              EMAIL = ?EL(VCard, <<"EMAIL">>),
              <<"john@mail.example.com">> = ?EL_CD(EMAIL, <<"USERID">>),

              N = ?EL(VCard, <<"N">>),
              <<"Doe">> = ?EL_CD(N, <<"FAMILY">>),
              <<"John">> = ?EL_CD(N, <<"GIVEN">>),
              <<"F.">> = ?EL_CD(N, <<"MIDDLE">>),

              ADR = ?EL(VCard, <<"ADR">>),
              <<"1899 Wynkoop Street">> = ?EL_CD(ADR, <<"STREET">>),
              <<"Denver">> = ?EL_CD(ADR, <<"LOCALITY">>),
              <<"CO">> = ?EL_CD(ADR, <<"REGION">>),
              <<"91210">> = ?EL_CD(ADR, <<"PCODE">>),
              %% TODO: Fix country: "additional info: attribute 'c' not allowed"
              %%<<"US of A">> = ?EL_CD(ADR, <<"CTRY">>),

              TEL = ?EL(VCard, <<"TEL">>),
              <<"+1 512 305 0280">> = ?EL_CD(TEL, <<"NUMBER">>),

              ORG = ?EL(VCard, <<"ORG">>),
              <<"The world">> = ?EL_CD(ORG, <<"ORGNAME">>),
              <<"People">> = ?EL_CD(ORG, <<"ORGUNIT">>),

              PHOTO = ?EL(VCard, <<"PHOTO">>),
              ?PHOTO_B64_MD5 = crypto:md5(?EL_CD(PHOTO, <<"BINVAL">>))
      end).

user_doesnt_exist(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              IQGet = #xmlelement{
                         name = <<"iq">>,
                         attrs = [{<<"id">>, base16:encode(crypto:rand_bytes(16))},
                                  {<<"to">>, <<"nobody@example.com">>},
                                  {<<"type">>, <<"get">>}],
                         children = [#xmlelement{
                                        name = <<"vCard">>,
                                        attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
                                        children = []}]},
              escalus:send(John, IQGet),

              Stanza = escalus:wait_for_stanza(John),

              %% If no vCard exists or the user does not exist, the server MUST
              %% return a stanza error, which SHOULD be either
              %% <service-unavailable/> or <item-not-found/>
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Stanza)
      end).

filtered_user_is_nonexistent(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              IQGet = #xmlelement{
                         name = <<"iq">>,
                         attrs = [{<<"id">>, base16:encode(crypto:rand_bytes(16))},
                                  {<<"to">>, <<"tom@example.com">>},
                                  {<<"type">>, <<"get">>}],
                         children = [#xmlelement{
                                        name = <<"vCard">>,
                                        attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
                                        children = []}]},
              escalus:send(John, IQGet),

              Stanza = escalus:wait_for_stanza(John),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Stanza)
      end).

update_card(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              NewVCard = #xmlelement{ name = <<"FN">>,
                                      attrs = [],
                                      children = [{xmlcdata, <<"New name">>}]},
              IQSet = #xmlelement{
                         name = <<"iq">>,
                         attrs = [{<<"id">>, base16:encode(crypto:rand_bytes(16))},
                                  {<<"type">>, <<"set">>}],
                         children = [#xmlelement{
                                        name = <<"vCard">>,
                                        attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
                                        children = [NewVCard]}]},
              escalus:send(John, IQSet),

              Stanza = escalus:wait_for_stanza(John),

              %% auth forbidden is also allowed
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"not-allowed">>], Stanza)

              %% Could check that the VCard didn't change, but since updates
              %% aren't implemented for anyone for vcard_ldap, there's little point
      end).

retrieve_others_card(Config) ->
    escalus:story(
      Config, [{valid, 1}, {valid2, 1}],
      fun(John, Dave) ->
              IQGet = #xmlelement{
                         name = <<"iq">>,
                         attrs = [{<<"id">>, base16:encode(crypto:rand_bytes(16))},
                                  {<<"to">>, <<"dave@example.com">>},
                                  {<<"type">>, <<"get">>}],
                         children = [#xmlelement{
                                        name = <<"vCard">>,
                                        attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
                                        children = []
                                       }]},
              escalus:send(John, IQGet),

              Stanza = escalus:wait_for_stanza(John),
              escalus_pred:is_iq(<<"result">>, Stanza),

              VCard = ?EL(Stanza, <<"vCard">>),
              <<"dave">> = ?EL_CD(VCard, <<"NICKNAME">>),
              <<"Davidson, Dave">> = ?EL_CD(VCard, <<"FN">>),
              <<"dave@example.com">> = ?EL_CD(VCard, <<"JABBERID">>),

              %% In accordance with XMPP Core [5], a compliant server MUST
              %% respond on behalf of the requestor and not forward the IQ to
              %% the requestee's connected resource.

              Error = (catch escalus:wait_for_stanza(Dave)),
              assert_timeout_when_waiting_for_stanza(Error)
      end).

server_vcard(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              IQGet = #xmlelement{
                         name = <<"iq">>,
                         attrs = [{<<"id">>, base16:encode(crypto:rand_bytes(16))},
                                  {<<"to">>, <<"example.com">>},
                                  {<<"type">>, <<"get">>}],
                         children = [#xmlelement{
                                        name = <<"vCard">>,
                                        attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
                                        children = []
                                       }]},
              escalus:send(John, IQGet),

              Stanza = escalus:wait_for_stanza(John),
              escalus_pred:is_iq(<<"result">>, Stanza),

              VCard = ?EL(Stanza, <<"vCard">>),
              <<"ejabberd">> = ?EL_CD(VCard, <<"FN">>)
      end).

service_discovery(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              IQGet = #xmlelement{
                         name = <<"iq">>,
                         attrs = [{<<"id">>, base16:encode(crypto:rand_bytes(16))},
                                  {<<"to">>, <<"example.com">>},
                                  {<<"from">>, escalus_client:full_jid(John)},
                                  {<<"type">>, <<"get">>}],
                         children = [#xmlelement{
                                        name = <<"query">>,
                                        attrs = [{<<"xmlns">>,?NS_DISCO_INFO}],
                                        children = []
                                       }]},
              escalus:send(John, IQGet),
              Stanza = escalus:wait_for_stanza(John),
              escalus:assert(is_iq_result, Stanza),
              has_feature(Stanza, <<"vcard-temp">>)
    end).


%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

%% TODO: copied from ldap_SUTE - move to escalus_predicates
assert_timeout_when_waiting_for_stanza(Error) ->
    {'EXIT', {timeout_when_waiting_for_stanza,_}} = Error.

%% TODO: copied from muc_SUITE - move to escalus_predicates
has_feature(Stanza, Feature) ->
    Features = exml_query:paths(Stanza, [{element, <<"query">>},
                                         {element, <<"feature">>}]),
    true = lists:any(fun(Item) ->
                        exml_query:attr(Item, <<"var">>) == Feature
                     end,
                     Features).
