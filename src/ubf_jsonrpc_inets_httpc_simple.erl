-module(ubf_jsonrpc_inets_httpc_simple).

-ifdef(old_inets).
-include_lib("inets/src/httpd.hrl").
-else.
-include_lib("inets/src/http_server/httpd.hrl").
-endif.

-export([do/4, do/6, do/7]).

%% @equiv do(Url, Contract, Request, Id, [], [])

%% do/4
do(Url, Contract, Request, Id) ->
    do(Url, Contract, Request, Id, [], []).

%% @equiv do(Url, Contract, Request, Id, HTTPOptions, Options, true)

%% do/6
do(Url, Contract, Request, Id, HTTPOptions, Options) ->
    do(Url, Contract, Request, Id, HTTPOptions, Options, true).

%% @spec (string(), atom(), atom() | tuple(), string(), proplist(), proplist(),
%%        bool()) ->
%%       {ok, term() | undefined, term() | undefined, string()} | {error,term()}
%% @doc Send an atom/tuple RPC call request to a JSON-RPC service at Url.
%%
%% Per JSON-RPC definition, if we return an 'ok' 4-tuple, either the
%% 2nd element will be 'undefined' (i.e. call failed, see 3rd element) or the
%% 3rd element will be 'undefined' (i.e. call succeeded, see 2nd element).
%%
%% See the Inets app docs for http:request/4 for definition of HTTPOptions
%% and Options arguments.
%%
%% If the SubstAuthInfoP flag is true, then the 2nd element of the
%% Request tuple is assumed to be authentication info of some kind.
%% It must be agreed upon that the contract is expecting such auth
%% info games, because the server side will extract auth info from the
%% HTTP session and insert that data into the 2nd element of Request,
%% which will effective replace whatever the client had placed in the
%% 2nd element here on the client side.

%% do/7
do(Url, Contract, Request, Id, HTTPOptions, Options, SubstAuthInfoP) ->
    {_AuthInfo, EncodedReq} =
        ubf_jsonrpc:rpc_v11_req_encode(Request, Id, Contract, SubstAuthInfoP),
    Encoder = mochijson2:encoder([{utf8, true}]),
    RequestBody = iolist_to_binary(Encoder(EncodedReq)),
    case do_post(Url, RequestBody, HTTPOptions, [{body_format, binary}|Options]) of
        {ok, 200, ResponseBody} ->
            ubf_jsonrpc:rpc_v11_res_decode(ResponseBody, Contract);
        {ok, Code, _} ->
            {error, Code};
        Err ->
            Err
    end.


%%
%%---------------------------------------------------------------------
%%

do_post(Url, RequestBody, HTTPOptions, Options) ->
    RequestHead = [{"connection", "close"}
                   , {"content-type", "application/json; charset=utf-8"}
                   , {"content-length", integer_to_list(size(RequestBody))}],
    Request = {Url, RequestHead, [], RequestBody},
    Response = httpc:request(post, Request, HTTPOptions, Options),
    case Response of
        {ok, {{"HTTP/1.1", Code, _},
              [{"connection", "close"}
               , {"date", _Date}
               , {"server", _Server}
               , {"content-length", _Length}
               , {"content-type", _Type}]
              , ResponseBody}} ->
            {ok, Code, ResponseBody};
        {error, _} ->
            Response
    end.
