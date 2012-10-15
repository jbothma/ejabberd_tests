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

%% Element CData
-define(EL(Element, Name), exml_query:path(Element, [{element, Name}])).
-define(EL_CD(Element, Name), exml_query:path(Element, [{element, Name}, cdata])).
-define(PHOTO_B64_MD5, <<41,39,104,189,75,25,191,200,129,237,251,129,91,76,195,
  162>>).

%% This is the UTF-8 of Москва
-define(MOSCOW_RU_BIN, <<208,156,208,190,209,129,208,186,208,178,208,176>>).

%% This is the md5 of the UTF-8 of В Советской России, дорога разветвляется вы
-define(STREET_RU_MD5, <<45,220,43,98,22,144,242,20,45,41,160,214,142,89,215,30>>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, vcard}
     ,{group, search}
     ,{group, no_search}
     ,{group, limited_search}
    ].

groups() ->
    [{vcard, [], [
                  retrieve_own_card
                  ,user_doesnt_exist
                  ,update_card
                  ,filtered_user_is_nonexistent
                  ,retrieve_others_card
                  ,vcard_service_discovery
                  ,server_vcard
                  ,directory_service_vcard
                 ]},
     {search, [], [
                   req_search_fields
                   ,search_open
                   ,search_empty
                   ,search_some
                  ]},
     {no_search, [], [
                      search_not_allowed,
                      search_not_in_service_discovery
                      ]},
     {limited_search, [], [
                           search_in_service_discovery,
                           search_open_limited,
                           search_some
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
%% XEP-0054: vcard-temp Test cases
%%--------------------------------------------------------------------

retrieve_own_card(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              IQGet = escalus_stanza:iq(<<"get">>, [vcard([])]),
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
              %%<<"US">> = ?EL_CD(ADR, <<"CTRY">>),

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
              IQGet = escalus_stanza:iq(
                        <<"nobody@example.com">>, <<"get">>, vcard([])),
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
              IQGet = escalus_stanza:iq(
                        <<"tom@example.com">>,<<"get">>,[vcard([])]),
              escalus:send(John, IQGet),

              Stanza = escalus:wait_for_stanza(John),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Stanza)
      end).

update_card(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              NewVCardEls = [#xmlelement{
                               name = <<"FN">>,
                               attrs = [],
                               children = [{xmlcdata, <<"New name">>}]}],
              IQSet = escalus_stanza:iq(<<"set">>, [vcard(NewVCardEls)]),
              escalus:send(John, IQSet),

              Stanza = escalus:wait_for_stanza(John),

              %% auth forbidden is also allowed
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"not-allowed">>], Stanza)

              %% Could check that the vCard didn't change, but since updates
              %% aren't implemented for anyone for vcard_ldap, there's little point
      end).

retrieve_others_card(Config) ->
    escalus:story(
      Config, [{valid, 1}, {valid2, 1}],
      fun(John, Dave) ->
              IQGet = escalus_stanza:iq(<<"dave@example.com">>, <<"get">>, [vcard([])]),
              escalus:send(John, IQGet),

              Stanza = escalus:wait_for_stanza(John),
              escalus_pred:is_iq(<<"result">>, Stanza),

              VCard = ?EL(Stanza, <<"vCard">>),
              <<"dave">> = ?EL_CD(VCard, <<"NICKNAME">>),
              <<"Davidson, Dave">> = ?EL_CD(VCard, <<"FN">>),
              <<"dave@example.com">> = ?EL_CD(VCard, <<"JABBERID">>),

              ADR = ?EL(VCard, <<"ADR">>),
              ?STREET_RU_MD5 = crypto:md5(?EL_CD(ADR, <<"STREET">>)),

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
              IQGet = escalus_stanza:iq(<<"example.com">>, <<"get">>, [vcard([])]),
              escalus:send(John, IQGet),

              Stanza = escalus:wait_for_stanza(John),
              escalus_pred:is_iq(<<"result">>, Stanza),

              VCard = ?EL(Stanza, <<"vCard">>),
              <<"ejabberd">> = ?EL_CD(VCard, <<"FN">>)
      end).

directory_service_vcard(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              DirJID = <<"vjud.example.com">>,
              IQGet = escalus_stanza:iq(DirJID, <<"get">>, [vcard([])]),
              escalus:send(John, IQGet),

              Stanza = escalus:wait_for_stanza(John),
              escalus_pred:is_iq(<<"result">>, Stanza),

              VCard = ?EL(Stanza, <<"vCard">>),
              <<"ejabberd/mod_vcard">> = ?EL_CD(VCard, <<"FN">>)
      end).

vcard_service_discovery(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              Query = escalus_stanza:query_el(?NS_DISCO_INFO, []),
              IQGet = escalus_stanza:iq(<<"example.com">>, <<"get">>, [Query]),
              escalus:send(John, IQGet),
              Stanza = escalus:wait_for_stanza(John),
              escalus:assert(is_iq_result, Stanza),
              escalus:assert(has_feature, [<<"vcard-temp">>], Stanza)
    end).

%%--------------------------------------------------------------------
%% XEP-0055 jabber:iq:search User Directory service Test cases
%%
%%--------------------------------------------------------------------

%% example.com

req_search_fields(Config) ->
escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              DirJID = <<"vjud.example.com">>,
              Query = escalus_stanza:query_el(?NS_SEARCH, []),
              IQGet = escalus_stanza:iq(DirJID, <<"get">>, [Query]),
              escalus:send(John, IQGet),
              Stanza = escalus:wait_for_stanza(John),
              escalus:assert(is_iq_result, Stanza),
              Result = ?EL(Stanza, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlelement{ children = XChildren } = XData,
              FieldTups = field_tuples(XChildren),
              true = lists:member({<<"text-single">>, <<"%u">>, <<"User">>},
                                  FieldTups),
              true = lists:member({<<"text-single">>,
                                   <<"displayName">>,
                                   <<"Full Name">>},
                                  FieldTups)
      end).

search_open(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              DirJID = <<"vjud.example.com">>,
              Fields = [#xmlelement{ name = <<"field">>}],
              Form = escalus_stanza:x_data_form(<<"submit">>, Fields),
              Query = escalus_stanza:query_el(?NS_SEARCH, [Form]),
              IQGet = escalus_stanza:iq(DirJID, <<"set">>, [Query]),
              escalus:send(John, IQGet),
              Stanza = escalus:wait_for_stanza(John),
              escalus:assert(is_iq_result, Stanza),
              Result = ?EL(Stanza, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlelement{ attrs = _XAttrs,
                           children = XChildren } = XData,
              Reported = ?EL(XData, <<"reported">>),
              ReportedFieldTups = field_tuples(Reported#xmlelement.children),

              %% Basically test that the right values exist
              %% and map to the right column headings
              ItemTups = item_tuples(ReportedFieldTups, XChildren),
              {_,JohnsFields} =
                  lists:keyfind(<<"john@example.com">>, 1, ItemTups),
              %% TODO: we can probably check many fields with a lists:map
              true = lists:member({<<"text-single">>,
                                      <<"EMAIL">>, <<"Email">>,
                                      <<"john@mail.example.com">>},
                                     JohnsFields),
              {_, DavesFields} =
                  lists:keyfind(<<"dave@example.com">>, 1, ItemTups),
              true = lists:member({<<"text-single">>,
                                      <<"FN">>, <<"Full Name">>,
                                      <<"Davidson, Dave">>},
                                     DavesFields)
      end).

search_empty(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              DirJID = <<"vjud.example.com">>,
              Fields = [#xmlelement{
                           name = <<"field">>,
                           attrs = [{<<"var">>,<<"sn">>}],
                           children = [#xmlelement{
                                          name= <<"value">>,
                                          children =
                                              [{xmlcdata,<<"nobody">>}]}]}],
              Form = escalus_stanza:x_data_form(<<"submit">>, Fields),
              Query = escalus_stanza:query_el(?NS_SEARCH, [Form]),
              IQSearch = escalus_stanza:iq(DirJID, <<"set">>, [Query]),
              escalus:send(John, IQSearch),
              Stanza = escalus:wait_for_stanza(John),
              escalus:assert(is_iq_result, Stanza),
              Result = ?EL(Stanza, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlelement{ children = XChildren } = XData,
              Reported = ?EL(XData, <<"reported">>),
              ReportedFieldTups = field_tuples(Reported#xmlelement.children),

              [] = item_tuples(ReportedFieldTups, XChildren)
      end).

search_some(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(John) ->
              DirJID = <<"vjud.example.com">>,
              Fields = [#xmlelement{
                           name = <<"field">>,
                           attrs = [{<<"var">>,<<"l">>}],
                           children = [#xmlelement{
                                          name = <<"value">>,
                                          children =
                                              [{xmlcdata, ?MOSCOW_RU_BIN}]}]}],
              Form = escalus_stanza:x_data_form(<<"submit">>, Fields),
              Query = escalus_stanza:query_el(?NS_SEARCH, [Form]),
              IQGet = escalus_stanza:iq(DirJID, <<"set">>, [Query]),
              escalus:send(John, IQGet),
              Stanza = escalus:wait_for_stanza(John),
              escalus:assert(is_iq_result, Stanza),
              Result = ?EL(Stanza, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlelement{ children = XChildren } = XData,
              Reported = ?EL(XData, <<"reported">>),
              ReportedFieldTups = field_tuples(Reported#xmlelement.children),

              %% Basically test that the right values exist
              %% and map to the right column headings
              ItemTups = item_tuples(ReportedFieldTups, XChildren),
              false = lists:keyfind(<<"john@example.com">>, 1, ItemTups),
              %% TODO: we can probably check many fields with a lists:map
              %% and data from test.config.
              {_, DavesFields} = lists:keyfind(
                                   <<"dave@example.com">>, 1, ItemTups),
              true = lists:member({<<"text-single">>,
                                      <<"FN">>, <<"Full Name">>,
                                      <<"Davidson, Dave">>},
                                     DavesFields)
      end).

%%------------------------------------
%% @limited.search.ldap

search_open_limited(Config) ->
    escalus:story(
      Config, [{ltd_search, 1}],
      fun(LtdUsr) ->
              DirJID = <<"directory.limited.search.ldap">>,
              Fields = [#xmlelement{ name = <<"field">>}],
              Form = escalus_stanza:x_data_form(<<"submit">>, Fields),
              Query = escalus_stanza:query_el(?NS_SEARCH, [Form]),
              IQGet = escalus_stanza:iq(DirJID, <<"set">>, [Query]),
              escalus:send(LtdUsr, IQGet),
              Stanza = escalus:wait_for_stanza(LtdUsr),
              escalus:assert(is_iq_result, Stanza),
              Result = ?EL(Stanza, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlelement{ children = XChildren } = XData,
              Reported = ?EL(XData, <<"reported">>),
              ReportedFieldTups = field_tuples(Reported#xmlelement.children),
              ItemTups = item_tuples(ReportedFieldTups, XChildren),

              %% exactly one result returned and its JID domain is correct
              [{SomeJID, _JIDsFields}] = ItemTups,
              {_Start, _Length} = binary:match(SomeJID, <<"@limited.search.ldap">>)
      end).

%% disco#items to limited.search.ldap says directory.limited.search.ldap exists
%% disco#info to directory.limited.search.ldap says it has feature jabber:iq:search
%% and an <identity category='directory' type='user'/>
%%   http://xmpp.org/extensions/xep-0030.html#registrar-reg-identity
search_in_service_discovery(Config) ->
    escalus:story(
      Config, [{ltd_search, 1}],
      fun(LtdUsr) ->
              ServJID = <<"limited.search.ldap">>,
              DirJID = <<"directory.limited.search.ldap">>,

              %% Item
              ItemsQuery = escalus_stanza:query_el(?NS_DISCO_ITEMS, []),
              ItemsIQGet = escalus_stanza:iq(ServJID, <<"get">>, [ItemsQuery]),
              escalus:send(LtdUsr, ItemsIQGet),
              ItemsStanza = escalus:wait_for_stanza(LtdUsr),
              escalus:assert(is_iq_result, ItemsStanza),
              escalus:assert(has_item, [DirJID], ItemsStanza),

              %% Feature
              InfoQuery = escalus_stanza:query_el(?NS_DISCO_INFO, []),
              InfoIQGet = escalus_stanza:iq(DirJID, <<"get">>, [InfoQuery]),
              escalus:send(LtdUsr, InfoIQGet),
              InfoStanza = escalus:wait_for_stanza(LtdUsr),
              escalus:assert(is_iq_result, InfoStanza),
              escalus:assert(has_feature, [?NS_SEARCH], InfoStanza),

              %% Identity
              escalus:assert(has_identity, [<<"directory">>,
                                            <<"user">>], InfoStanza)
      end).

%%------------------------------------
%% @no.search.ldap

search_not_allowed(Config) ->
    escalus:story(
      Config, [{no_search, 1}],
      fun(NoSearchUsr) ->
              DirJID = <<"vjud.no.search.ldap">>,
              Fields = [#xmlelement{ name = <<"field">>}],
              Form = escalus_stanza:x_data_form(<<"submit">>, Fields),
              Query = escalus_stanza:query_el(?NS_SEARCH, [Form]),
              IQGet = escalus_stanza:iq(DirJID, <<"set">>, [Query]),
              escalus:send(NoSearchUsr, IQGet),
              Stanza = escalus:wait_for_stanza(NoSearchUsr),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Stanza)
      end).

%% disco#items to no.search.ldap says no vjud.limited.search.ldap exists
search_not_in_service_discovery(Config) ->
    escalus:story(
      Config, [{ltd_search, 1}],
      fun(LtdUsr) ->
              ServJID = <<"no.search.ldap">>,
              DirJID = <<"vjud.no.search.ldap">>,
              %% Item
              ItemsQuery = escalus_stanza:query_el(?NS_DISCO_ITEMS, []),
              ItemsIQGet = escalus_stanza:iq(ServJID, <<"get">>, [ItemsQuery]),
              escalus:send(LtdUsr, ItemsIQGet),
              ItemsStanza = escalus:wait_for_stanza(LtdUsr),
              escalus:assert(is_iq_result, ItemsStanza),
              escalus:assert(has_no_such_item, [DirJID], ItemsStanza)
      end).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

%% TODO: copied from ldap_SUTE - move to escalus_predicates
assert_timeout_when_waiting_for_stanza(Error) ->
    {'EXIT', {timeout_when_waiting_for_stanza,_}} = Error.



vcard(Body) ->
    #xmlelement{
       name = <<"vCard">>,
       attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
       children = Body
      }.

%%
%% -> [{Type, Var, Label}]
%%
field_tuples([]) ->
    [];
field_tuples([#xmlelement{name = <<"field">>,
                          attrs=Attrs,
                          children=_Children} = El| Rest]) ->
    {<<"type">>,Type} = lists:keyfind(<<"type">>, 1, Attrs),
    {<<"var">>,Var} = lists:keyfind(<<"var">>, 1, Attrs),
    {<<"label">>,Label} = lists:keyfind(<<"label">>, 1, Attrs),
    case ?EL_CD(El, <<"value">>) of
        undefined ->
            [{Type, Var, Label}|field_tuples(Rest)];
        ValCData ->
            [{Type, Var, Label, ValCData}|field_tuples(Rest)]
    end;
field_tuples([_SomeOtherEl|Rest]) ->
    field_tuples(Rest).


%%
%%  -> [{Type, Var, Label, ValueCData}]
%%
%% This is naiive and expensive LOL!
item_field_tuples(_, []) ->
    [];
item_field_tuples(ReportedFieldTups,
                  [#xmlelement{name = <<"field">>,
                               attrs=Attrs,
                               children=_Children} = El| Rest]) ->
    {<<"var">>,Var} = lists:keyfind(<<"var">>, 1, Attrs),
    {Type, Var, Label} = lists:keyfind(Var, 2, ReportedFieldTups),
    [{Type, Var, Label, ?EL_CD(El, <<"value">>)}
     | item_field_tuples(ReportedFieldTups, Rest)];

item_field_tuples(ReportedFieldTups, [_SomeOtherEl|Rest]) ->
    item_field_tuples(ReportedFieldTups, Rest).


%%
%% -> [{JID, [ItemFieldTups]}]
%%
%% Finds the JID and maps fields to their labels and types
%%
item_tuples(_, []) ->
    [];
item_tuples(ReportedFieldTups, [#xmlelement{name = <<"item">>,
                                            children = Children} | Rest]) ->
    ItemFieldTups = item_field_tuples(ReportedFieldTups, Children),
    {_,_,_,JID} = lists:keyfind(<<"jid">>, 2, ItemFieldTups),
    [{JID, ItemFieldTups}|item_tuples(ReportedFieldTups, Rest)];
item_tuples(ReportedFieldTypes, [_SomeOtherChild | Rest]) ->
    item_tuples(ReportedFieldTypes, Rest).
