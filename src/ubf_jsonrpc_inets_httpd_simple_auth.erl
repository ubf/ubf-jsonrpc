-module(ubf_jsonrpc_inets_httpd_simple_auth, [CONTRACT, URL, SUBST_AUTHINFO]).

-include_lib("inets/src/httpd.hrl").

-export([do/1]).


%% do/1
do(#mod{method=Method, data=OldData}=Info) ->
    case {Method, proplists:get_value(status, OldData), proplists:get_value(response, OldData)} of
        {"POST", undefined, undefined} ->
            do_rpc(Info);
        _ ->
            {proceed,OldData}
    end.


%%
%%---------------------------------------------------------------------
%%

do_rpc(#mod{init_data=InitData
            , request_uri=RequestUri
            , entity_body=RequestBody
            , data=OldData}) ->
    if CONTRACT /= undefined andalso URL /= undefined ->
            case lists:prefix(URL, RequestUri) of
                true ->
                    AuthInfo = InitData,
                    {JsonResult, JsonError, _JsonStatus, JsonId} = do_jsonrpc(AuthInfo, RequestBody, CONTRACT),
                    ResponseBody = ubf_jsonrpc:rpc_v11_res_encode(JsonResult, JsonError, JsonId, CONTRACT),
                    ResponseHead = [{code,200}
                                    , {content_type, "application/json; charset=utf-8"}
                                    , {content_length, integer_to_list(length(ResponseBody))}],
                    NewData = [{response,{response,ResponseHead,ResponseBody}}],
                    {proceed, NewData};
                false ->
                    {proceed, OldData}
            end;
       true ->
            {proceed, OldData}
    end.


do_jsonrpc(AuthInfo,JSonRpc,Contract) ->
    case catch (ubf_jsonrpc:rpc_v11_req_decode(AuthInfo, JSonRpc, Contract, SUBST_AUTHINFO)) of
        {ok,EbfTerm,Id} ->
            case ubf_rpc(Contract, EbfTerm) of
                {clientBrokeContract, _, _} ->
                    {undefined, error_102(), undefined, Id};
                {serverBrokeContract, _, _} ->
                    {undefined, error_103(), undefined, Id};
                Reply ->
                    {Reply, undefined, undefined, Id}
            end;
        {error,Reason,Id} ->
            {undefined, error_100(Reason), undefined, Id};
        {error,Reason} ->
            Id = undefined,
            {undefined, error_100(Reason), undefined, Id};
        {'EXIT',Reason} ->
            Id = undefined,
            {undefined, error_100(Reason), undefined, Id}
    end.


error_100(Reason) ->
    [{name, <<"JSONRPCError">>},
     {code, 100},
     {message, <<"Parse error (clientBrokeRPC)">>},
     {error, Reason}
    ].

error_102() ->
    error_102(undefined).

error_102(Reason) ->
    [{name, <<"JSONRPCError">>},
     {code, 102},
     {message, <<"Bad call (clientBrokeContract)">>},
     {error, Reason}
    ].

error_103() ->
    error_103(undefined).

error_103(Reason) ->
    [{name, <<"JSONRPCError">>},
     {code, 103},
     {message, <<"Service error (serverBrokeContract)">>},
     {error, Reason}
    ].

ubf_rpc(Plugin, Args) ->
    case ubf_client:lpc(Plugin, Args) of
        {reply,Reply,none} ->
            Reply;
        Err ->
            erlang:error(Err)
    end.
