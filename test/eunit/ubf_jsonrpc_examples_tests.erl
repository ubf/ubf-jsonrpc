%%% Description: Executable examples for developer enlightenment.
%%%-------------------------------------------------------------------

-module(ubf_jsonrpc_examples_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

do_eunit() ->
    case eunit:test(?MODULE) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.

%% Result:
%% {undefined,{obj,[{"version",<<"1.1">>},
%%                  {"id",<<"x_id">>},
%%                  {"method",<<"some_rpc_func">>},
%%                  {"params",[777]}]}}

encode_v11_without_authinfo_test() ->
    UbfMod = mod_not_needed_for_encoding_simple_types_like_integers,
    {undefined, _} =
	ubf_jsonrpc:rpc_v11_req_encode(
	  {some_rpc_func, 777}, <<"x_id">>, UbfMod, false).

%% Result:
%% {{whatever,{auth,info},{{{{stuff}}}}},
%%  {obj,[{"version",<<"1.1">>},
%%        {"id",<<"x_id">>},
%%        {"method",<<"some_rpc_func">>},
%%        {"params",[777]}]}}

encode_v11_with_authinfo_test() ->
    UbfMod = mod_not_needed_for_encoding_simple_types_like_integers,
    ArbitraryAuthInfoStuff = {whatever, {auth, info}, {{{{stuff}}}}},
    {ArbitraryAuthInfoStuff, _} =
	ubf_jsonrpc:rpc_v11_req_encode(
	  {some_rpc_func, ArbitraryAuthInfoStuff, 777},
	  <<"x_id">>, UbfMod, true).

