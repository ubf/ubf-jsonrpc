%%% Description: eunit test for ubf and jsonrpc
%%%-------------------------------------------------------------------

-module(ubf_jsonrpc_inets_simple_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


do_eunit() ->
    case eunit:test(?MODULE) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.




-define(HOST,     "127.0.0.1").
-define(URL,      "/test").
-define(CONTRACT, ubf_jsonrpc_stateless_plugin).


%%%----------------------------------------------------------------------
%%% HTTP Server API
%%%----------------------------------------------------------------------

%% @note To avoid configuration (right now), implement the HTTP server
%% callback using a parameterized module.

do(Info) ->
    Mod = ubf_jsonrpc_inets_httpd_simple:new(?CONTRACT, ?URL),
    (Mod):do(Info).


%%%----------------------------------------------------------------------
%%% HTTP Client API
%%%----------------------------------------------------------------------

do_rpc(Url, Contract, Q) ->
    Id = make_id(),
    ubf_jsonrpc_inets_httpc_simple:do(Url, Contract, Q, Id).

do_rpc(Url, Contract, Q, Timeout) ->
    Id = make_id(),
    HTTPOptions = [{timeout, Timeout}],
    Options = [],
    ubf_jsonrpc_inets_httpc_simple:do(Url, Contract, Q, Id, HTTPOptions, Options).

make_id() ->
    {MSec, Sec, USec} = now(),
    list_to_binary(integer_to_list((MSec * 1000000 * 1000000) + (Sec * 1000000) + USec)).

%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    {setup,
     fun test_setup/0,
     fun test_teardown/1,
     all_actual_tests_inets_simple()
    }.


%% inets simple
all_actual_tests_inets_simple() ->
    fun ({Pid,Port}) ->
            Url = test_url({Pid,Port}, ?HOST, ?URL),
            Contract = ?CONTRACT,
            %% client will exclude and server will populate
            AuthInfo = dummy_placeholder,

            [?_test(test_001(Url, Contract, AuthInfo))
             , ?_test(test_002(Url, Contract, AuthInfo))
             , ?_test(test_003(Url, Contract, AuthInfo))
             , ?_test(test_004(Url, Contract, AuthInfo))
             , ?_test(test_005(Url, Contract, AuthInfo))
             , ?_test(test_006(Url, Contract, AuthInfo))
             , ?_test(test_007(Url, Contract, AuthInfo))
             , ?_test(test_008(Url, Contract, AuthInfo))
             , ?_test(test_009(Url, Contract, AuthInfo))
            ]
    end.

%% setup
test_setup() ->
    true = code:add_patha("../test/Unit-EUnit-Files"),
    application:start(inets),
    ServerRoot = filename:join([filename:dirname(code:priv_dir(inets)), "examples", "server_root"]),
    DocumentRoot = filename:join(ServerRoot, "htdocs"),
    Options = [{port, 0}, {bind_address, ?HOST}, {server_name,"httpd_test"}
               , {server_root, ServerRoot}, {document_root, DocumentRoot}
               , {modules, [?MODULE, mod_get]}],
    {ok, Pid} = inets:start(httpd, Options),
    [{port, Port}] = httpd:info(Pid, [port]),
    {Pid,Port}.

%% teardown
test_teardown({Pid,_Port}) ->
    ok = inets:stop(httpd, Pid),
    true = code:del_path("../test/Unit-EUnit-Files"),
    ok.

%% url
test_url({_Pid,Port}, Host, Url) ->
    "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ Url.


%%%-------------------------------------------------------------------
%% simple smoke tests to demostrate ubf jsonrpc inets

%% keepalive - exact url match
test_001(Url,Contract,_AuthInfo) ->
    Request = keepalive,
    Response = do_rpc(Url,Contract,Request),
    ?assertMatch({ok,ok,undefined,_Id},Response).

%% keepalive - prefix url match
test_002(Url,Contract,_AuthInfo) ->
    Request = keepalive,
    Response = do_rpc(Url ++ "/suffix",Contract,Request),
    ?assertMatch({ok,ok,undefined,_Id},Response).

%% keepalive - no url match
test_003(Url,Contract,_AuthInfo) ->
    Request = keepalive,
    Response = do_rpc(string:substr(Url,1,length(Url)-1),Contract,Request),
    ?assertMatch({error,500},Response).

%% now
test_004(Url,Contract,AuthInfo) ->
    Request = {now,AuthInfo,infinity},
    Response = do_rpc(Url,Contract,Request),
    ?assertMatch({ok,{ok,{_X,_Y,_Z}},undefined,_Id},Response).

%% client breaks
test_005(Url,Contract, AuthInfo) ->
    Request = {client_breaks_req01_with_this_request,AuthInfo,infinity},
    Response = do_rpc(Url,Contract,Request),
    Error = [{name,<<"JSONRPCError">>}, {code,102}
             , {message,<<"Bad call (clientBrokeContract)">>}, {error,undefined}],
    ?assertMatch({ok,undefined,Error,_Id},Response).

%% client timeout
test_006(Url,Contract,AuthInfo) ->
    Request = {client_timeout_req03,AuthInfo,1000},
    Response = do_rpc(Url,Contract,Request,500),
    ?assertMatch({error,timeout},Response).

%% server breaks
test_007(Url,Contract,AuthInfo) ->
    Request = {server_breaks_req01,AuthInfo,infinity},
    Response = do_rpc(Url,Contract,Request),
    Error = [{name,<<"JSONRPCError">>}, {code,103}
             , {message,<<"Service error (serverBrokeContract)">>}, {error,undefined}],
    ?assertMatch({ok,undefined,Error,_Id},Response).

%% server timeout
test_008(Url,Contract,AuthInfo) ->
    Request = {server_timeout_req03,AuthInfo,500},
    Response = do_rpc(Url,Contract,Request),
    ?assertMatch({ok,server_timeout_res03,undefined,_Id},Response).

%% server crash
test_009(Url,Contract,AuthInfo) ->
    Request = {server_crash_req05,AuthInfo,infinity},
    Response = do_rpc(Url,Contract,Request,infinity),
    ?assertMatch({error,socket_closed_remotely},Response).
