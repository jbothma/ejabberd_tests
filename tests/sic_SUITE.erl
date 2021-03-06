%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_offline* modules
%%% @end
%%%===================================================================

-module(sic_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_SIC, <<"urn:xmpp:sic:0">>).

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, mod_sic_tests}].

all_tests() ->
    [user_sic, forbidden_user_sic].

groups() ->
    [{mod_sic_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    Config1 = escalus:init_per_suite(Config0),
    escalus:create_users(Config1).

end_per_suite(Config) ->
    escalus:delete_users(Config),
    escalus:end_per_suite(Config).

init_per_group(mod_offline_tests, Config) ->
    start_module(mod_sic, []),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(mod_offline_tests, _Config) ->
    stop_module(mod_sic);
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% offline tests
%%%===================================================================

%% Retrieve my IP
user_sic(Config) ->
    escalus:story(Config, [1], fun(Alice) ->
        %% Alice sends a SIC IQ stanza
        escalus:send(Alice, sic_iq_get()),
        %% Alice expects IQ result with client IP address
        Stanza = escalus:wait_for_stanza(Alice),
        escalus_new_assert:assert(is_sic_response(), Stanza)
    end).

%% Try to retrieve other user's IP
forbidden_user_sic(Config) ->
    escalus:story(Config, [1, 1], fun(Alice, _Bob) ->
        %% Alice sends a SIC IQ stanza to get Bob's IP
        escalus:send(Alice, sic_iq_get(bob)),
        %% Alice should get <forbidden/> error
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], Stanza)
    end).

%%%===================================================================
%%% Custom predicates
%%%===================================================================

is_sic_response() ->
    fun(Stanza) ->
        escalus_pred:is_iq(<<"result">>, Stanza)
        andalso
        <<"127.0.0.1">> == exml_query:path(Stanza, [{element, <<"ip">>}, cdata])
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

start_module(ModuleName, Options) ->
    Args = [ct:get_config(ejabberd_domain), ModuleName, Options],
    escalus_ejabberd:rpc(gen_mod, start_module, Args).

stop_module(ModuleName) ->
    Args = [ct:get_config(ejabberd_domain), ModuleName],
    escalus_ejabberd:rpc(gen_mod, stop_module, Args).

sic_iq_get() ->
    escalus_stanza:iq(<<"get">>, [#xmlelement{
        name = <<"ip">>,
        attrs = [{<<"xmlns">>, ?NS_SIC}],
        children = []
    }]).

sic_iq_get(To) ->
    escalus_stanza:to(sic_iq_get(), To).
