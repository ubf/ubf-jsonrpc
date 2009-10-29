-module(ubf_jsonrpc_inets_httpc_simple).

-include_lib("inets/src/httpd.hrl").

-export([do/4, do/6]).

%% do/4
do(Url, Contract, Request, Id) ->
    do(Url, Contract, Request, Id, [], []).

%% do/6
do(Url, Contract, Request, Id, HTTPOptions, Options) ->
    {_AuthInfo, EncodedReq} = ubf_jsonrpc:rpc_v11_req_encode(Request, Id, Contract),
    RequestBody = rfc4627:encode(EncodedReq),
    case do_post(Url, RequestBody, HTTPOptions, Options) of
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
                   , {"content-length", integer_to_list(length(RequestBody))}],
    Request = {Url, RequestHead, [], RequestBody},
    Response = http:request(post, Request, HTTPOptions, Options),
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
