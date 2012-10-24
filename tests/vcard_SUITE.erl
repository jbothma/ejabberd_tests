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
    [{group, mnesia},
     {group, odbc},
     {group, ldap}
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
       ,request_search_fields_mnesia
       ,search_open_mnesia
       ,search_empty_mnesia
       ,search_some_mnesia
       ,search_not_allowed_mnesia
       ,search_not_in_service_discovery_mnesia
       ,search_in_service_discovery_mnesia
       ,search_open_limited_mnesia
       ,search_some_limited_mnesia
      ]},
     {odbc, [sequence],
      [update_own_card_odbc
       ,retrieve_own_card_odbc
       ,user_doesnt_exist_odbc
       ,update_other_card_odbc
       ,retrieve_others_card_odbc
       ,vcard_service_discovery_odbc
       ,server_vcard_odbc
       ,directory_service_vcard_odbc
       ,request_search_fields_odbc
       ,search_open_odbc
       ,search_empty_odbc
       ,search_some_odbc
       ,search_not_allowed_odbc
       ,search_not_in_service_discovery_odbc
       ,search_in_service_discovery_odbc
       ,search_open_limited_odbc
       ,search_some_limited_odbc
      ]},
     %% LDAP data is set according to the README instructions.
     {ldap, [],
      [retrieve_own_card_ldap
       ,user_doesnt_exist_ldap
       ,update_own_card_ldap
       ,filtered_user_is_nonexistent_ldap
       ,retrieve_others_card_ldap
       ,vcard_service_discovery_ldap
       ,server_vcard_ldap
       ,directory_service_vcard_ldap
       ,request_search_fields_ldap
       ,search_open_ldap
       ,search_empty_ldap
       ,search_some_ldap
       ,search_not_allowed_ldap
       ,search_not_in_service_discovery_ldap
       ,search_in_service_discovery_ldap
       ,search_open_limited_ldap
      ]}
    ].

suite() ->
    [{require, vcard} | escalus:suite()].


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(mnesia, Config) ->
    %% use the relevant users
    Users = escalus_config:get_config(escalus_vcard_mnesia_users, Config, []),
    NewConfig = lists:keystore(escalus_users, 1, Config, {escalus_users, Users}),
    escalus_users:create_users(NewConfig, Users),
    NewConfig;
init_per_group(odbc, Config) ->
    %% use the relevant users
    Users = escalus_config:get_config(escalus_vcard_odbc_users, Config, []),
    NewConfig = lists:keystore(escalus_users, 1, Config, {escalus_users, Users}),
    escalus_users:create_users(NewConfig, Users),
    NewConfig;
init_per_group(ldap, Config) ->
    Users = escalus_config:get_config(escalus_ldap_users, Config, []),
    NewConfig = lists:keystore(escalus_users, 1, Config, {escalus_users, Users}),
    escalus:init_per_suite(NewConfig).

end_per_group(mnesia,Config) ->
    Users = escalus_config:get_config(escalus_vcard_mnesia_users, Config, []),
    escalus_users:delete_users(Config, Users),
    Config;
end_per_group(odbc, Config) ->
    Users = escalus_config:get_config(escalus_vcard_odbc_users, Config, []),
    escalus_users:delete_users(Config, Users),
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% XEP-0054: vcard-temp Test cases
%%--------------------------------------------------------------------

update_own_card_ldap(Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              Fields = [vcard_cdata_field(<<"FN">>, <<"New name">>)],
              Stanza = update_vcard(Client, Fields),

              %% auth forbidden is also allowed
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"not-allowed">>], Stanza)

              %% Could check that the vCard didn't change, but since updates
              %% aren't implemented for anyone for vcard_ldap, there's little point
      end).

update_own_card_mnesia(Config) ->
    update_own_vcard(mnesia, Config).

update_own_card_odbc(Config) ->
    update_own_vcard(odbc, Config).

update_own_vcard(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}, {user2, 1}, {ltd_search1, 1}, {ltd_search2, 1}],
      fun(Client1, Client2, Client3, Client4) ->
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
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, expected_vcards, JID1}),
              Client1Fields2 = tuples_to_vcard_fields(Client1VCardTups),
              _Client1SetResultStanza2 = update_vcard(Client1, Client1Fields2),

              %% might as well check this more serious update too
              Client1GetResultStanza2 = request_vcard(Client1),
              check_vcard(Client1VCardTups, Client1GetResultStanza2),

              JID2 = escalus_client:short_jid(Client2),
              Client2VCardTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, expected_vcards, JID2}),
              Client2Fields = tuples_to_vcard_fields(Client2VCardTups),
              _Client2SetResultStanza = update_vcard(Client2, Client2Fields),

              %% two users for limited.search.modvcard with some field equal so
              %% that we can check that {matches, 1} is enforced.
              JID3 = escalus_client:short_jid(Client3),
              Client3VCardTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, expected_vcards, JID3}),
              Client3Fields = tuples_to_vcard_fields(Client3VCardTups),
              _Client3SetResultStanza = update_vcard(Client3, Client3Fields),
              JID4 = escalus_client:short_jid(Client4),
              Client4VCardTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, expected_vcards, JID4}),
              Client4Fields = tuples_to_vcard_fields(Client4VCardTups),
              _Client4SetResultStanza = update_vcard(Client4, Client4Fields)
      end).

retrieve_own_card_ldap(Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              Stanza = request_vcard(Client),
              JID = escalus_client:short_jid(Client),
              ClientVCardTups =
                  escalus_config:get_ct(
                    {vcard, ldap, all_search, expected_vcards, JID}),
              check_vcard(ClientVCardTups, Stanza),

              PHOTO = stanza_get_vcard_field(Stanza, <<"PHOTO">>),
              PhotoB64MD5 = escalus_config:get_ct({vcard, common, photo_b64_md5}),
              PhotoB64MD5 = crypto:md5(?EL_CD(PHOTO, <<"BINVAL">>))
      end).

retrieve_own_card_mnesia(Config) ->
    retrieve_own_card(mnesia, Config).

retrieve_own_card_odbc(Config) ->
    retrieve_own_card(odbc, Config).

retrieve_own_card(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              Stanza = request_vcard(Client),
              JID = escalus_client:short_jid(Client),
              ClientVCardTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, expected_vcards, JID}),
              check_vcard(ClientVCardTups, Stanza)
      end).


user_doesnt_exist_ldap(Config) ->
    user_doesnt_exist(ldap, Config).

user_doesnt_exist_mnesia(Config) ->
    user_doesnt_exist(mnesia, Config).

user_doesnt_exist_odbc(Config) ->
    user_doesnt_exist(odbc, Config).

%% If no vCard exists or the user does not exist, the server MUST
%% return a stanza error, which SHOULD be either
%% <service-unavailable/> or <item-not-found/>
user_doesnt_exist(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              BadJID = escalus_config:get_ct(
                         {vcard, GroupName, all_search, nonexistent_jid}),
              Stanza = request_vcard(BadJID, Client),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Stanza)
      end).

filtered_user_is_nonexistent_ldap(Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              FilteredJID = <<"tom@example.com">>,
              Stanza = request_vcard(FilteredJID, Client),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Stanza)
      end).

update_other_card_mnesia(Config) ->
    update_other_card(mnesia, Config).

update_other_card_odbc(Config) ->
    update_other_card(odbc, Config).

update_other_card(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}, {user2, 1}],
      fun(Client, OtherClient) ->
              JID = escalus_client:short_jid(Client),
              Fields = [vcard_cdata_field(<<"FN">>, <<"New name">>)],
              Stanza = update_vcard(JID, OtherClient, Fields),

              %% auth forbidden is also allowed
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"not-allowed">>], Stanza),

              %% check that nothing was changed
              retrieve_own_card(GroupName, Config)
      end).

retrieve_others_card_ldap(Config) ->
    retrieve_others_card(ldap, Config).

retrieve_others_card_mnesia(Config) ->
    retrieve_others_card(mnesia, Config).

retrieve_others_card_odbc(Config) ->
    retrieve_others_card(odbc, Config).

retrieve_others_card(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}, {user2, 1}],
      fun(Client, OtherClient) ->
              OtherJID = escalus_client:short_jid(OtherClient),
              Stanza = request_vcard(OtherJID, Client),

              OtherClientVCardTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, expected_vcards, OtherJID}),
              check_vcard(OtherClientVCardTups, Stanza),

              StreetMD5 = escalus_config:get_ct({vcard, common, utf8_street_md5}),
              ADR = stanza_get_vcard_field(Stanza, <<"ADR">>),
              StreetMD5 = crypto:md5(?EL_CD(ADR, <<"STREET">>)),

              %% In accordance with XMPP Core [5], a compliant server MUST
              %% respond on behalf of the requestor and not forward the IQ to
              %% the requestee's connected resource.

              Error = (catch escalus:wait_for_stanza(OtherClient)),
              assert_timeout_when_waiting_for_stanza(Error)
      end).

server_vcard_ldap(Config) ->
    server_vcard(ldap, Config).

server_vcard_mnesia(Config) ->
    server_vcard(mnesia, Config).

server_vcard_odbc(Config) ->
    server_vcard(odbc, Config).

server_vcard(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              ServJID = escalus_config:get_ct(
                          {vcard, GroupName, all_search, server_jid}),
              Stanza = request_vcard(ServJID, Client),
              ServerVCardTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, expected_vcards, ServJID}),
              check_vcard(ServerVCardTups, Stanza)
      end).

directory_service_vcard_ldap(Config) ->
    directory_service_vcard(ldap, Config).

directory_service_vcard_mnesia(Config) ->
    directory_service_vcard(mnesia, Config).

directory_service_vcard_odbc(Config) ->
    directory_service_vcard(odbc, Config).

directory_service_vcard(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, all_search, directory_jid}),
              Stanza = request_vcard(DirJID, Client),
              DirVCardTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, expected_vcards, DirJID}),
              check_vcard(DirVCardTups, Stanza)
      end).

vcard_service_discovery_ldap(Config) ->
    vcard_service_discovery(ldap, Config).

vcard_service_discovery_mnesia(Config) ->
    vcard_service_discovery(mnesia, Config).

vcard_service_discovery_odbc(Config) ->
    vcard_service_discovery(odbc, Config).

vcard_service_discovery(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              ServJID = escalus_config:get_ct(
                          {vcard, GroupName, all_search, server_jid}),
              Stanza = request_disco_info(ServJID, Client),
              escalus:assert(is_iq_result, Stanza),
              escalus:assert(has_feature, [<<"vcard-temp">>], Stanza)
    end).

%%--------------------------------------------------------------------
%% XEP-0055 jabber:iq:search User Directory service Test cases
%%
%%--------------------------------------------------------------------


%% all.search.domain

request_search_fields_ldap(Config) ->
    request_search_fields(ldap, Config).

request_search_fields_mnesia(Config) ->
    request_search_fields(mnesia, Config).

request_search_fields_odbc(Config) ->
    request_search_fields(odbc, Config).

request_search_fields(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              {Field1Var, Field1Name} =
                  escalus_config:get_ct({vcard, GroupName, locality_field}),
              {Field2Var, Field2Name} =
                  escalus_config:get_ct({vcard, GroupName, fullname_field}),
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, all_search, directory_jid}),
              Query = escalus_stanza:query_el(?NS_SEARCH, []),
              IQGet = escalus_stanza:iq(DirJID, <<"get">>, [Query]),
              escalus:send(Client, IQGet),
              Stanza = escalus:wait_for_stanza(Client),
              escalus:assert(is_iq_result, Stanza),
              Result = ?EL(Stanza, <<"query">>),
              XData = ?EL(Result, <<"x">>),
              #xmlelement{ children = XChildren } = XData,
              FieldTups = field_tuples(XChildren),
              true = lists:member({<<"text-single">>,
                                   Field1Var, Field1Name},
                                  FieldTups),
              true = lists:member({<<"text-single">>,
                                   Field2Var, Field2Name},
                                  FieldTups)
      end).

search_open_ldap(Config) ->
    search_open(ldap, Config).

search_open_mnesia(Config) ->
    search_open(mnesia, Config).

search_open_odbc(Config) ->
    search_open(odbc, Config).

search_open(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, all_search, directory_jid}),
              Fields = [#xmlelement{ name = <<"field">> }],
              Stanza = set_search_iq(Client, DirJID, Fields),
              escalus:assert(is_iq_result, Stanza),

              %% Basically test that the right values exist
              %% and map to the right column headings
              ItemTups = search_result_item_tuples(Stanza),
              ExpectedItemTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, search_results, open}),
              list_unordered_key_match(1, ExpectedItemTups, ItemTups)
      end).

search_empty_ldap(Config) ->
    search_empty(ldap, Config).

search_empty_mnesia(Config) ->
    search_empty(mnesia, Config).

search_empty_odbc(Config) ->
    search_empty(odbc, Config).

search_empty(GroupName, Config) ->
    escalus:story(
      Config, [{user1, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, all_search, directory_jid}),
              Fields = [#xmlelement{
                           name = <<"field">>,
                           attrs = [{<<"var">>, <<"orgname">>}],
                           children = [#xmlelement{
                                          name= <<"value">>,
                                          children =
                                              [{xmlcdata,<<"nobody">>}]}]}],
              Stanza = set_search_iq(Client, DirJID, Fields),
              escalus:assert(is_iq_result, Stanza),
              [] = search_result_item_tuples(Stanza)
      end).

search_some_ldap(Config) ->
    search_some(ldap, Config).

search_some_mnesia(Config) ->
    search_some(mnesia, Config).

search_some_odbc(Config) ->
    search_some(odbc, Config).

search_some(GroupName, Config) ->
    escalus:story(
      Config, [{user2, 1}],
      fun(Client) ->
              {FieldVar, _FieldName} =
                  escalus_config:get_ct({vcard, GroupName, locality_field}),
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, all_search, directory_jid}),
              MoscowRUBin = escalus_config:get_ct({vcard, common, moscow_ru_utf8}),
              Fields = [#xmlelement{
                           name = <<"field">>,
                           attrs = [{<<"var">>, FieldVar}],
                           children = [#xmlelement{
                                          name = <<"value">>,
                                          children =
                                              [{xmlcdata, MoscowRUBin}]}]}],
              Stanza = set_search_iq(Client, DirJID, Fields),
              escalus:assert(is_iq_result, Stanza),

              %% Basically test that the right values exist
              %% and map to the right column headings
              ItemTups = search_result_item_tuples(Stanza),
              ExpectedItemTups =
                  escalus_config:get_ct(
                    {vcard, GroupName, all_search, search_results, some}),
              list_unordered_key_match(1, ExpectedItemTups, ItemTups)
      end).


%%------------------------------------
%% @limited.search.domain

search_open_limited_ldap(Config) ->
    escalus:story(
      Config, [{ltd_search1, 1}],
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

search_open_limited_mnesia(Config) ->
    search_open_limited(mnesia, Config).

search_open_limited_odbc(Config) ->
    search_open_limited(odbc, Config).

search_open_limited(GroupName, Config) ->
    escalus:story(
      Config, [{ltd_search1, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, ltd_search, directory_jid}),
              Fields = [#xmlelement{ name = <<"field">>}],
              Stanza = set_search_iq(Client, DirJID, Fields),
              escalus:assert(is_iq_result, Stanza),
              %% {allow_return_all, false}
              [] = search_result_item_tuples(Stanza)
      end).

search_some_limited_mnesia(Config) ->
    search_some_limited(mnesia, Config).

search_some_limited_odbc(Config) ->
    search_some_limited(odbc, Config).

search_some_limited(GroupName, Config) ->
    escalus:story(
      Config, [{ltd_search1, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, ltd_search, directory_jid}),
              Server = escalus_client:server(Client),
              Fields = [#xmlelement{
                           name = <<"field">>,
                           attrs = [{<<"var">>,<<"last">>}],
                           children = [#xmlelement{
                                          name = <<"value">>,
                                          children =
                                              [{xmlcdata, <<"Doe">>}]}]}],
              Stanza = set_search_iq(Client, DirJID, Fields),
              escalus:assert(is_iq_result, Stanza),
              ItemTups = search_result_item_tuples(Stanza),
              %% exactly one result returned and its JID domain is correct
              [{SomeJID, _JIDsFields}] = ItemTups,
              {_Start, _Length} = binary:match(SomeJID, <<"@", Server/binary>>)
      end).


search_in_service_discovery_ldap(Config) ->
    search_in_service_discovery(ldap, Config).

search_in_service_discovery_mnesia(Config) ->
    search_in_service_discovery(mnesia, Config).

search_in_service_discovery_odbc(Config) ->
    search_in_service_discovery(odbc, Config).

%% disco#items to limited.search.domain says directory.limited.search.domain exists
%% disco#info to directory.limited.search.domain says it has feature jabber:iq:search
%% and an <identity category='directory' type='user'/>
%%   http://xmpp.org/extensions/xep-0030.html#registrar-reg-identity
search_in_service_discovery(GroupName, Config) ->
    escalus:story(
      Config, [{ltd_search1, 1}],
      fun(Client) ->
              ServJID = escalus_config:get_ct(
                         {vcard, GroupName, ltd_search, server_jid}),
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, ltd_search, directory_jid}),

              %% Item
              ItemsStanza = get_disco_items_iq(ServJID, Client),
              escalus:assert(is_iq_result, ItemsStanza),
              escalus:assert(has_item, [DirJID], ItemsStanza),

              %% Feature
              InfoQuery = escalus_stanza:query_el(?NS_DISCO_INFO, []),
              InfoIQGet = escalus_stanza:iq(DirJID, <<"get">>, [InfoQuery]),
              escalus:send(Client, InfoIQGet),
              InfoStanza = escalus:wait_for_stanza(Client),
              escalus:assert(is_iq_result, InfoStanza),
              escalus:assert(has_feature, [?NS_SEARCH], InfoStanza),

              %% Identity
              escalus:assert(has_identity, [<<"directory">>,
                                            <<"user">>], InfoStanza)
      end).

%%------------------------------------
%% @no.search.domain

search_not_allowed_ldap(Config) ->
    search_not_allowed(ldap, Config).

search_not_allowed_mnesia(Config) ->
    search_not_allowed(mnesia, Config).

search_not_allowed_odbc(Config) ->
    search_not_allowed(odbc, Config).

search_not_allowed(GroupName, Config) ->
    escalus:story(
      Config, [{no_search, 1}],
      fun(Client) ->
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, no_search, directory_jid}),
              Fields = [#xmlelement{ name = <<"field">>}],
              Stanza = set_search_iq(Client, DirJID, Fields),
              escalus:assert(is_error, [<<"cancel">>,
                                        <<"service-unavailable">>], Stanza)
      end).


search_not_in_service_discovery_ldap(Config) ->
    search_not_in_service_discovery(ldap, Config).

search_not_in_service_discovery_mnesia(Config) ->
    search_not_in_service_discovery(mnesia, Config).

search_not_in_service_discovery_odbc(Config) ->
    search_not_in_service_discovery(odbc, Config).

%% disco#items to no.search.domain doesn't say vjud.no.search.domain exists
search_not_in_service_discovery(GroupName, Config) ->
    escalus:story(
      Config, [{no_search, 1}],
      fun(Client) ->
              ServJID = escalus_config:get_ct(
                         {vcard, GroupName, no_search, server_jid}),
              DirJID = escalus_config:get_ct(
                         {vcard, GroupName, no_search, directory_jid}),
              %% Item
              ItemsStanza = get_disco_items_iq(ServJID, Client),
              escalus:assert(is_iq_result, ItemsStanza),
              escalus:assert(has_no_such_item, [DirJID], ItemsStanza)
      end).

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

vcard_data(Key, Config) ->
    lists:keyfind(Key, 1, escalus_config:get_config(vcard_data, Config)).

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
%% Any extra values in the vcard are ignored by this function and should be
%% checked or rejected elsewhere.
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

%% Checks that the elements of two lists with matching keys are equal
%% while the order of the elements does not matter.
%% Returning means success. Crashing via ct:fail means failure.
%% Prints the lists in the ct:fail Result term.
list_unordered_key_match(Keypos, Expected, Actual) ->
    case length(Actual) of
        ActualLength when ActualLength == length(Expected) ->
            list_unordered_key_match2(Keypos, Expected, Actual);
        ActualLength ->
            ct:fail("Expected size ~p, actual size ~p~nExpected: ~p~nActual: ~p",
                    [length(Expected), ActualLength, Expected, Actual])
    end.

list_unordered_key_match2(_, [], _) ->
    ok;
list_unordered_key_match2(Keypos, [ExpctdTup|Rest], ActualTuples) ->
    Key = element(Keypos, ExpctdTup),
    ActualTup = lists:keyfind(Key, Keypos, ActualTuples),
    case ActualTup of
        ExpctdTup ->
            list_unordered_key_match2(Keypos, Rest, ActualTuples);
        _ ->
            ct:fail("~nExpected ~p~nGot ~p", [ExpctdTup, ActualTup])
    end.

search_result_item_tuples(Stanza) ->
    Result = ?EL(Stanza, <<"query">>),
    XData = ?EL(Result, <<"x">>),
    #xmlelement{ attrs = _XAttrs,
                 children = XChildren } = XData,
    Reported = ?EL(XData, <<"reported">>),
    ReportedFieldTups = field_tuples(Reported#xmlelement.children),
    _ItemTups = item_tuples(ReportedFieldTups, XChildren).

%%--------------------------
%% common actual XMPP exchanges

request_vcard(Client) ->
    IQGet = escalus_stanza:iq(<<"get">>, [vcard([])]),
    escalus:send(Client, IQGet),
    _Stanza = escalus:wait_for_stanza(Client).

request_vcard(JID, Client) ->
    IQGet = escalus_stanza:iq(JID, <<"get">>, [vcard([])]),
    escalus:send(Client, IQGet),
    _Stanza = escalus:wait_for_stanza(Client).

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

set_search_iq(Client, JID, Fields) ->
    Form = escalus_stanza:x_data_form(<<"submit">>, Fields),
    Query = escalus_stanza:query_el(?NS_SEARCH, [Form]),
    IQGet = escalus_stanza:iq(JID, <<"set">>, [Query]),
    escalus:send(Client, IQGet),
    _Stanza = escalus:wait_for_stanza(Client).

get_disco_items_iq(ServJID, Client) ->
    ItemsQuery = escalus_stanza:query_el(?NS_DISCO_ITEMS, []),
    ItemsIQGet = escalus_stanza:iq(ServJID, <<"get">>, [ItemsQuery]),
    escalus:send(Client, ItemsIQGet),
    _ItemsStanza = escalus:wait_for_stanza(Client).
