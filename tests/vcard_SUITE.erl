%%==============================================================================
%% Copyright 2012 Erlang Solutions Ltd.
%%
%% Test the mod_vcard* modules.
%% mod_vcard uses mnesia
%% mod_vcard_ldap uses ldap
%% mod_vcard_odbc uses odbc
%%
%% They share many comonalities but sometimes behave differently or have
%% some extra or missing features. They also need different config depending
%% on which vhost they're running on.
%%
%%           -----
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


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mnesia}
    ].

groups() ->
    %% setting test data before tests is proving awkward so might as well use the
    %% data set in the update tests to test the rest.
    [{mnesia, [sequence],
      [update_own_card_mnesia
       ,retrieve_own_card_mnesia
       ,user_doesnt_exist_mnesia
       ,update_other_card_mnesia
       ,retrieve_others_card_mnesia
       ,vcard_service_discovery_mnesia
       ,server_vcard_mnesia
       ,directory_service_vcard_mnesia
       ,req_search_fields_mnesia
       ,search_open_mnesia
       ,search_empty_mnesia
       ,search_some_mnesia
       ,search_not_allowed_mnesia
       ,search_not_in_service_discovery_mnesia
       ,search_in_service_discovery_mnesia
       ,search_open_limited_mnesia
      ]}
    ].

suite() ->
    [{require, vcard} | escalus:suite()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
%    dbg:tracer(),
%    dbg:p(all, c),
%    dbg:tpl(escalus_config, []),
%    dbg:tpl(escalus_, []),
%    dbg:tpl(escalus_session, []),

    ok = ct:require({vcard, mnesia}),

    %% use the relevant users
    Users = escalus_config:get_config(escalus_vcard_mnesia_users, Config, []),
    NewConfig = lists:keystore(escalus_users, 1, Config, {escalus_users, Users}),

    escalus_users:create_users(NewConfig, Users),
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

update_own_card_mnesia(Config) ->
    escalus:story(
      Config, [{user1, 1}, {user2, 1}],
      fun(Client1, Client2) ->
              %% set some initial value different from the actual test data
              %% so we know it really got updated and wasn't just old data
              Client1Fields = [vcard_cdata_field(<<"FN">>, <<"Old name">>)],
              Client1SetResultStanza = update_vcard(Client1, Client1Fields),
              escalus:assert(is_iq_result, Client1SetResultStanza),

              Client1GetResultStanza = request_vcard(Client1),
              <<"Old name">> =
                  stanza_get_vcard_field_cdata(Client1GetResultStanza, <<"FN">>),

              %% Setup test data for remaining tests
              JID1 = escalus_client:short_jid(Client1),
              Client1VCardTups =
                  escalus_config:get_ct({vcard, mnesia, expected_vcards, JID1}),
              Client1Fields2 = tuples_to_vcard_fields(Client1VCardTups),
              _SetResltStanza2 = update_vcard(Client1, Client1Fields2),

              %% might as well check this more serious update too
              Client1GetResultStanza2 = request_vcard(Client1),
              check_vcard(Client1VCardTups, Client1GetResultStanza2),

              JID2 = escalus_client:short_jid(Client2),
              Client2VCardTups =
                  escalus_config:get_ct({vcard, mnesia, expected_vcards, JID2}),
              Client2Fields = tuples_to_vcard_fields(Client2VCardTups),
              _Stanza = update_vcard(Client2, Client2Fields)
      end).

retrieve_own_card_mnesia(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(Client) ->
              Stanza = request_vcard(Client),
              JID1 = escalus_client:short_jid(Client),
              ClientVCardTups =
                  escalus_config:get_ct({vcard, mnesia, expected_vcards, JID1}),
              check_vcard(ClientVCardTups, Stanza)
      end).



%% If no vCard exists or the user does not exist, the server MUST
%% return a stanza error, which SHOULD be either
%% <service-unavailable/> or <item-not-found/>
user_doesnt_exist_mnesia(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(Client) ->
              BadJID = <<"nobody@example.com">>,
              Stanza = request_vcard(BadJID, Client),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Stanza)
      end).

update_other_card_mnesia(Config) ->
    escalus:story(
      Config, [{user1, 1}, {user2, 1}],
      fun(Client, OtherClient) ->
              OtherJID = escalus_client:short_jid(OtherClient),
              Fields = [vcard_cdata_field(<<"FN">>, <<"New name">>)],
              Stanza = update_vcard(OtherJID, Client, Fields),

              %% auth forbidden is also allowed
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"not-allowed">>], Stanza),

              %% check that nothing was changed
              ct:fail(todo)
      end).

retrieve_others_card_mnesia(Config) ->
    escalus:story(
      Config, [{valid, 1}, {valid2, 1}],
      fun(Client, OtherClient) ->
              OtherJID = escalus_client:short_jid(OtherClient),
              Stanza = request_vcard(OtherJID, Client),
              check_vcard(OtherJID, Stanza),

              StreetMD5 = ct:config_get({vcard, common, utf8_street_md5}),
              ADR = stanza_get_vcard_field(Stanza, <<"ADR">>),
              StreetMD5 = crypto:md5(?EL_CD(ADR, <<"STREET">>)),

              %% In accordance with XMPP Core [5], a compliant server MUST
              %% respond on behalf of the requestor and not forward the IQ to
              %% the requestee's connected resource.

              Error = (catch escalus:wait_for_stanza(OtherClient)),
              assert_timeout_when_waiting_for_stanza(Error)
      end).

server_vcard_mnesia(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(Client) ->
              ServJID = ct:get_config({vcard, mnesia, server_jid}),
              Stanza = request_vcard(ServJID, Client),
              check_vcard(ServJID, Stanza)
      end).

directory_service_vcard_mnesia(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(Client) ->
              DirJID = ct:get_config({vcard, mnesia, directory_jid}),
              Stanza = request_vcard(DirJID, Client),
              check_vcard(DirJID, Stanza)
      end).

vcard_service_discovery_mnesia(Config) ->
    escalus:story(
      Config, [{valid, 1}],
      fun(Client) ->
              ServJID = ct:get_config({vcard, mnesia, server_jid}),
              Stanza = request_disco_info(ServJID, Client),
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
              {_, ItemTups} = expected_search_results(example.com_open, Config)
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
              MoscowRUBin = escalus_config:get_ct({vcard, common, moscow_ru_utf8}),
              Fields = [#xmlelement{
                           name = <<"field">>,
                           attrs = [{<<"var">>,<<"l">>}],
                           children = [#xmlelement{
                                          name = <<"value">>,
                                          children =
                                              [{xmlcdata, MoscowRUBin}]}]}],
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
              {_, ItemTups} = expected_search_results(example.com_some, Config)
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

expected_search_results(Key, Config) ->
    {_, ExpectedResults} =
        lists:keyfind(expected_results, 1,
                      escalus_config:get_config(search_data, Config)),
    lists:keyfind(Key, 1, ExpectedResults).

%% TODO: copied from ldap_SUTE - move to escalus_predicates
assert_timeout_when_waiting_for_stanza(Error) ->
    {'EXIT', {timeout_when_waiting_for_stanza,_}} = Error.


%%----------------------
%% xmlelement shortcuts

vcard(Body) ->
    #xmlelement{
       name = <<"vCard">>,
       attrs = [{<<"xmlns">>,<<"vcard-temp">>}],
       children = Body
      }.

vcard_cdata_field(Name, Value) ->
    #xmlelement{name = Name,
                attrs = [],
                children = [{xmlcdata, Value}]}.

vcard_field(Name, Children) ->
    #xmlelement{name = Name,
                attrs = [],
                children = Children}.

tuples_to_vcard_fields([]) ->
    [];
tuples_to_vcard_fields([{Name, Value}|Rest]) when is_binary(Value) ->
    [vcard_cdata_field(Name, Value) | tuples_to_vcard_fields(Rest)];
tuples_to_vcard_fields([{Name, Children}|Rest]) when is_list(Children) ->
    [vcard_field(Name, tuples_to_vcard_fields(Children))
     | tuples_to_vcard_fields(Rest)].

stanza_get_vcard_field(Stanza, FieldName) ->
    VCard = ?EL(Stanza, <<"vCard">>),
    ?EL(VCard, FieldName).

stanza_get_vcard_field_cdata(Stanza, FieldName) ->
    VCard = ?EL(Stanza, <<"vCard">>),
    ?EL_CD(VCard, FieldName).

%%---------------------
%% test helpers

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


%% This tests that at least the values in the ExpectedVCardTups are in the
%% VCardUnderTest.
%% Any extra values in the vcard are ignored by this function and should be checked or
%% rejected elsewhere.
%% crash means fail, return means success.
check_vcard(ExpectedVCardTups, Stanza) ->
    escalus_pred:is_iq(<<"result">>, Stanza),
    VCardUnderTest = ?EL(Stanza, <<"vCard">>),
    check_xml_element(ExpectedVCardTups, VCardUnderTest).


check_xml_element([], _ElUnderTest) ->
    ok;  %% just return true to be consistent with other clauses.
check_xml_element([{ExpdFieldName, ExpdChildren}|Rest], ElUnderTest)
  when is_list(ExpdChildren) ->
    check_xml_element(ExpdChildren, ?EL(ElUnderTest, ExpdFieldName)),
    check_xml_element(Rest, ElUnderTest);
check_xml_element([{ExpdFieldName, ExpdCData}|Rest], ElUnderTest) ->
    case ?EL_CD(ElUnderTest, ExpdFieldName) of
        ExpdCData ->
            check_xml_element(Rest, ElUnderTest);
        Else ->
            ct:fail("Expected ~p got ~p~n", [ExpdCData, Else])
    end.


%%--------------------------
%% common actual XMPP exchanges

request_vcard(Client) ->
    IQGet = escalus_stanza:iq(<<"get">>, [vcard([])]),
    escalus:send(Client, IQGet),
    Stanza = escalus:wait_for_stanza(Client),
    Stanza.

request_vcard(JID, Client) ->
    IQGet = escalus_stanza:iq(JID, <<"get">>, [vcard([])]),
    escalus:send(Client, IQGet),
    Stanza = escalus:wait_for_stanza(Client),
    Stanza.


update_vcard(Client, Fields) ->
    IQSet = escalus_stanza:iq(<<"set">>, [vcard(Fields)]),
    escalus:send(Client, IQSet),
    _Stanza = escalus:wait_for_stanza(Client).

update_vcard(JID, Client, Fields) ->
    IQSet = escalus_stanza:iq(JID, <<"set">>, [vcard(Fields)]),
    escalus:send(Client, IQSet),
    _Stanza = escalus:wait_for_stanza(Client).

request_disco_info(JID, Client) ->
    Query = escalus_stanza:query_el(?NS_DISCO_INFO, []),
    IQGet = escalus_stanza:iq(JID, <<"get">>, [Query]),
    escalus:send(Client, IQGet),
    _Stanza = escalus:wait_for_stanza(Client).
