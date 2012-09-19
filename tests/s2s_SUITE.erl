%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Suite for testing s2s connection
%%% @end
%%%===================================================================

-module(s2s_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [{group, s2s_tests}].

all_tests() ->
    [
     simple_message,
     nonexistent_user,
     unknown_domain,
     nonascii_addr,
     destination_domain_doesnt_match_cert,
     v1_skip_starttls_and_sasl,
     v1_skip_starttls_try_sasl,
     v1_do_starttls_skip_sasl,
     v1_do_starttls_and_sasl,
     v1_do_starttls_and_sasl_domain_doesnt_match_cert,
     v1_do_starttls_and_sasl_route_other_domain
    ].

groups() ->
    [{s2s_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    NewUsers = ct:get_config(escalus_server2_users) ++ ct:get_config(escalus_users),
    io:format("~p~n", [NewUsers]),
    Config1 = escalus:init_per_suite(Config0),
    escalus_users:create_users(Config1, NewUsers).

end_per_suite(Config) ->
    escalus:delete_users(Config),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% Server-to-server communication test
%%%===================================================================

simple_message(Config) ->
    escalus:story(Config, [{alice2, 1}, {alice, 1}], fun(Alice2, Alice1) ->

        %% Alice@localhost1 sends message to Alice@localhost2
        escalus:send(Alice1, escalus_stanza:chat_to(Alice2, <<"Hi, foreign Alice!">>)),

        %% Alice@localhost2 receives message from Alice@localhost1
        Stanza = escalus:wait_for_stanza(Alice2, 10000),
        escalus:assert(is_chat_message, [<<"Hi, foreign Alice!">>], Stanza),

        %% Alice@localhost2 sends message to Alice@localhost1
        escalus:send(Alice2, escalus_stanza:chat_to(Alice1, <<"Nice to meet you!">>)),

        %% Alice@localhost1 receives message from Alice@localhost2
        Stanza2 = escalus:wait_for_stanza(Alice1, 10000),
        escalus:assert(is_chat_message, [<<"Nice to meet you!">>], Stanza2)

    end).

nonexistent_user(Config) ->
    escalus:story(Config, [{alice, 1}, {alice2, 1}], fun(Alice1, Alice2) ->

        %% Alice@localhost1 sends message to Xyz@localhost2
        RemoteServer = escalus_client:server(Alice2),
        Fake = <<"xyz@", RemoteServer/binary>>,
        escalus:send(Alice1, escalus_stanza:chat_to(Fake,
                                                    <<"Hello, nonexistent!">>)),

        %% Alice@localhost1 receives stanza error: service-unavailable
        Stanza = escalus:wait_for_stanza(Alice1),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Stanza)

    end).

unknown_domain(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice1) ->

        %% Alice@localhost1 sends message to Xyz@localhost3
        escalus:send(Alice1, escalus_stanza:chat_to(
            <<"xyz@somebogushost">>,
            <<"Hello, unreachable!">>)),

        %% Alice@localhost1 receives stanza error: remote-server-not-found
        Stanza = escalus:wait_for_stanza(Alice1, 10000),
        escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza)

    end).

nonascii_addr(Config) ->
    escalus:story(Config, [{alice, 1}, {bob2, 1}], fun(Alice, Bob) ->

        %% Bob@localhost2 sends message to Alice@localhost1
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Cześć Alice!">>)),

        %% Alice@localhost1 receives message from Bob@localhost2
        Stanza = escalus:wait_for_stanza(Alice, 10000),
        escalus:assert(is_chat_message, [<<"Cześć Alice!">>], Stanza),

        %% Alice@localhost1 sends message to Bob@localhost2
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Miło Cię poznać">>)),

        %% Bob@localhost2 receives message from Alice@localhost1
        Stanza2 = escalus:wait_for_stanza(Bob, 10000),
        escalus:assert(is_chat_message, [<<"Miło Cię poznać">>], Stanza2)

    end).

%%
%% This depends on node2 being the server for kate's domain, but
%% kate's hostname not being in the certificate used for serving that domain
%%
destination_domain_doesnt_match_cert(Config) ->
    escalus:story(Config, [{alice, 1}, {kate2, 1}], fun(Alice, Kate) ->

        escalus:send(Alice,
                     escalus_stanza:chat_to(Kate, <<"Hello!">>)),

        %% Kate never receives the message
        Error = (catch escalus:wait_for_stanza(Kate)),
        assert_timeout_when_waiting_for_stanza(Error)
    end).


v1_skip_starttls_and_sasl(Config) ->
    Fun =
        fun(Alice2) ->
                {ok, Conn0, Props0} =
                    escalus_connection:connect(server_props()),
                {_Props1, _Features} =
                    escalus_session:start_stream(Conn0, Props0),

                Message = escalus_stanza:chat(
                            <<"alice@localhost">>,
                            <<"alice@localhost2">>,
                            <<"Hello via v1.0 no StartTLS no SASL!">>),

                escalus_connection:send(Conn0, Message),

                Error = (catch escalus:wait_for_stanza(Alice2)),
                assert_timeout_when_waiting_for_stanza(Error)
        end,
    escalus:story(Config, [{alice2, 1}], Fun).

v1_skip_starttls_try_sasl(Config) ->
    Fun =
        fun(Alice2) ->
                {ok, Conn0, Props0} =
                    escalus_connection:connect(server_props()),
                {Props1, _Features} =
                    escalus_session:start_stream(Conn0, Props0),
                {timeout, auth_reply} =
                    (catch escalus_session:authenticate(Conn0, Props1)),

                Message = escalus_stanza:chat(
                            <<"alice@localhost">>,
                            <<"alice@localhost2">>,
                            <<"Hello via v1.0 skip StartTLS try SASL!">>),

                escalus_connection:send(Conn0, Message),

                Error = (catch escalus:wait_for_stanza(Alice2)),
                assert_timeout_when_waiting_for_stanza(Error)
        end,
    escalus:story(Config, [{alice2, 1}], Fun).

v1_do_starttls_skip_sasl(Config) ->
    Fun =
        fun(Alice2) ->
                {ok, Conn0, Props0} =
                    escalus_connection:connect(server_props()),
                {Props1, _Features} =
                    escalus_session:start_stream(Conn0, Props0),
                {Conn1, _Props2} = escalus_session:starttls(Conn0, Props1),

                Message = escalus_stanza:chat(
                            <<"alice@localhost">>,
                            <<"alice@localhost2">>,
                            <<"Hello via v1.0 do StartTLS skip SASL!">>),

                escalus_connection:send(Conn1, Message),

                Error = (catch escalus:wait_for_stanza(Alice2)),
                assert_timeout_when_waiting_for_stanza(Error)
        end,
    escalus:story(Config, [{alice2, 1}], Fun).

v1_do_starttls_and_sasl(Config) ->
    Fun =
        fun(Alice2) ->
                {ok, Conn0, Props0} =
                    escalus_connection:connect(server_props()),
                {Props1, _Features} =
                    escalus_session:start_stream(Conn0, Props0),
                {Conn1, Props2} = escalus_session:starttls(Conn0, Props1),
                _Props3 = escalus_session:authenticate(Conn1, Props2),

                Message = escalus_stanza:chat(
                            <<"alice@localhost">>,
                            <<"alice@localhost2">>,
                            <<"Hello via v1.0 do StartTLS and SASL!">>),

                escalus_connection:send(Conn1, Message),

                MsgStanza = (catch escalus:wait_for_stanza(Alice2)),
                escalus:assert(is_chat_message,
                               [<<"Hello via v1.0 do StartTLS and SASL!">>],
                               MsgStanza)
        end,
    escalus:story(Config, [{alice2, 1}], Fun).

v1_do_starttls_and_sasl_domain_doesnt_match_cert(_Config) ->
    BadDomain = <<"localhostblah">>,
    Props = lists:keyreplace(endpoint, 1,
                             server_props(),
                             {endpoint, {server, BadDomain}}),

    {ok, Conn0, Props0} = escalus_connection:connect(Props),
    {Props1, _Features} = escalus_session:start_stream(Conn0, Props0),
    {Conn1, Props2} = escalus_session:starttls(Conn0, Props1),
    {auth_failed, BadDomain, _} =
        (catch escalus_session:authenticate(Conn1, Props2)),

    %% receiver MUST close connection after optional retries [RFC3920]
    %% ejabberd closes the connection straight after failure,
    %% it's not like the originating server is going to retry with
    %% a new cert anyway

    %% sleeping is horrid but... :S
    receive after 500 -> ok end,
    false = escalus_connection:is_connected(Conn1).


v1_do_starttls_and_sasl_route_other_domain(Config) ->
    Fun =
        fun(Alice2) ->
                {ok, Conn0, Props0} =
                    escalus_connection:connect(server_props()),
                {Props1, _Features} =
                    escalus_session:start_stream(Conn0, Props0),
                {Conn1, Props2} = escalus_session:starttls(Conn0, Props1),
                _Props3 = escalus_session:authenticate(Conn1, Props2),

                Message = escalus_stanza:chat(
                            <<"alice@google.com">>,
                            <<"alice@localhost2">>,
                            <<"Hello from a spoofed domain">>),

                escalus_connection:send(Conn1, Message),

                Error = (catch escalus:wait_for_stanza(Alice2)),
                assert_timeout_when_waiting_for_stanza(Error)
        end,
    escalus:story(Config, [{alice2, 1}], Fun).





assert_timeout_when_waiting_for_stanza(Error) ->
    {'EXIT', {timeout_when_waiting_for_stanza,_}} = Error.


server_props() ->
    [{endpoint, {server, <<"localhost">>}},
     {server, <<"localhost2">>},
     {host, <<"localhost">>},
     {ssl_opts, [{certfile,"/tmp/node1.pem"}]},
     {auth, {escalus_auth, auth_sasl_external}},
     {port, 5279}
    ].
