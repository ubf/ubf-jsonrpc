-module(ubf_jsonrpc).

-include("ubf.hrl").

-export([rpc_v11_req_encode_print/3]).
-export([rpc_v11_req_encode/3]).

-export([rpc_v11_req_decode_print/3]).
-export([rpc_v11_req_decode/3]).

-export([rpc_v11_res_encode_print/4]).
-export([rpc_v11_res_encode/4]).

-export([rpc_v11_res_decode_print/2]).
-export([rpc_v11_res_decode/2]).


%%
%% Links:
%%   http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html
%%   http://json-rpc.org/wiki/specification
%%   http://www.erlang-projects.org/Public/news/ejson/view
%%   http://www.erlang.org/eeps/eep-0018.html
%%   http://www.erlang.org/ml-archive/erlang-questions/200511/msg00193.html
%%   http://www.ietf.org/rfc/rfc4627.txt
%%   http://www.json.org/
%%   http://www.lshift.net/blog/2007/02/17/json-and-json-rpc-for-erlang
%%
%% Other Links:
%%   http://www.json.com/json-schema-proposal/
%%

%%
%% json-rpc
%%

%%
%% @todo - pending reviewing against JSON-RPC 1.1 Specification
%% Working Draft 7 August 2006

%% The following list (non-comprehensive) summarizes the general changes that have been applied to version 1.0:

%%     * JSON-RPC 1.1 is bound to HTTP only. Use over other protocols are not considered normative according to verson 1.1.
%%     * Renamed request to Procedure Call (capitalization significant).
%%     * Renamed response to Procedure Return (capitalization significant).
%%     * Removed notifications.
%%     * Removed the requirement to specify id for regular calls.
%%     * Formalize Content-Type for JSON-RPC.
%%     * Add version member to Procedure Call and Procedure Return objects.
%%     * Formalization of the JSON type system, i.e. Boolean, String, Number, Array, Object and the Null value (capitalization significant).
%%     * Added call encoding for HTTP GET.
%%     * Added named and positional arguments.
%%     * Added Error object and formalization of error codes.
%%     * Added introspection (service description) and required system procedures.


%% 1.1 Request (method invocation)
%%
%% A remote method is invoked by sending a request to a remote
%% service. The request is a single object serialized using JSON.
%%
%% It has three properties:
%%
%% method - A String containing the name of the method to be invoked.
%%
%% params - An Array of objects to pass as arguments to the method.
%%
%% id - The request id. This can be of any type. It is used to match the
%% response with the request that it is replying to.
%%
%% 1.2 Response
%%
%% When the method invocation completes, the service must reply with a
%% response. The response is a single object serialized using JSON.
%%
%% It has three properties:
%%
%% result - The Object that was returned by the invoked method. This must
%% be null in case there was an error invoking the method.
%%
%% error - An Error object if there was an error invoking the method. It
%% must be null if there was no error.
%%
%% id - This must be the same id as the request it is responding to.
%%
%% 1.3 Notification
%%
%% A notification is a special request which does not have a
%% response. The notification is a single object serialized using JSON.
%%
%% It has the same properties as the request object with one exception.
%% id - Must be null.


%%
%%---------------------------------------------------------------------
%%

rpc_v11_req_encode_print(X, Id, UBFMod) ->
    io:format("~s~n", [rpc_v11_req_encode(X, Id, UBFMod)]).

rpc_v11_req_encode(Method, Id, _UBFMod) when is_atom(Method) ->
    {undefined, {obj, [{"version", <<"1.1">>}, {"id", Id}, {"method", jsf:atom_to_binary(Method)}, {"params", []}]}};

rpc_v11_req_encode(X, Id, UBFMod) when is_tuple(X), size(X) > 1, is_atom(element(1, X)) ->
    [Method|[AuthInfo|Params]] = tuple_to_list(X),
    EncodedParams = jsf:do_encode(Params,UBFMod),
    {AuthInfo, {obj, [{"version", <<"1.1">>}, {"id", Id}, {"method", jsf:atom_to_binary(Method)}, {"params", EncodedParams}]}}.


%%
%%---------------------------------------------------------------------
%%

rpc_v11_req_decode_print(AuthInfo, X, UBFMod) ->
    io:format("~s~n", [rpc_v11_req_decode(AuthInfo, X, UBFMod)]).

rpc_v11_req_decode(AuthInfo, X, UBFMod) ->
    try
        case rfc4627:decode(X) of
            {ok, {obj, Props}, []} ->
                {value, {"version", <<"1.1">>}, Props1} = lists:keytake("version", 1, Props),
                {value, {"id", Id}, Props2} = lists:keytake("id", 1, Props1),
                {value, {"method", MethodBin}, Props3} = lists:keytake("method", 1, Props2),
                {value, {"params", JsonParams}, []} = lists:keytake("params", 1, Props3),
                Method = jsf:binary_to_existing_atom(MethodBin),
                case catch (jsf:do_decode(JsonParams, UBFMod)) of
                    {'EXIT', Reason} ->
                        {error, Reason, Id};
                    [] ->
                        {ok, Method, Id};
                    Params ->
                        if AuthInfo =:= undefined ->
                                {ok, list_to_tuple([Method|Params]), Id};
                           true ->
                                {ok, list_to_tuple([Method|[AuthInfo|Params]]), Id}
                        end
                end;
            Other ->
                {error, Other}
        end
    catch
        {'EXIT', Reason1} ->
            {error, Reason1};
          Else ->
            Else
    end.


%%
%%---------------------------------------------------------------------
%%

rpc_v11_res_encode_print(X, Y, Id, UBFMod) ->
    io:format("~s~n", [rpc_v11_res_encode(X, Y, Id, UBFMod)]).

rpc_v11_res_encode(X, Y, Id, UBFMod)  ->
    Result = jsf:do_encode(X,UBFMod),
    Error = jsf:do_encode(Y,UBFMod),
    Z = {obj, [{<<"version">>, <<"1.1">>}, {<<"id">>, Id}, {<<"result">>, Result}, {<<"error">>, Error}]},
    rfc4627:encode(Z).


%%
%%---------------------------------------------------------------------
%%

rpc_v11_res_decode_print(X, UBFMod) ->
    io:format("~s~n", [rpc_v11_res_decode(X, UBFMod)]).

rpc_v11_res_decode(X, UBFMod) ->
    case rfc4627:decode(X) of
        {ok, {obj, Props}, []} ->
            {value, {"version", <<"1.1">>}, Props1} = lists:keytake("version", 1, Props),
            {value, {"id", Id}, Props2} = lists:keytake("id", 1, Props1),
            {value, {"result", Result}, Props3} = lists:keytake("result", 1, Props2),
            {value, {"error", Error}, []} = lists:keytake("error", 1, Props3),
            {ok, jsf:do_decode(Result,UBFMod), jsf:do_decode(Error,UBFMod), Id};
        Other ->
            {error, Other}
    end.
