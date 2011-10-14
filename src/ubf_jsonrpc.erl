-module(ubf_jsonrpc).

-include("ubf.hrl").

-export([rpc_v11_req_encode_print/3]).
-export([rpc_v11_req_encode/3, rpc_v11_req_encode/4]).

-export([rpc_v11_req_decode_print/3]).
-export([rpc_v11_req_decode/3, rpc_v11_req_decode/4]).

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

rpc_v11_req_encode(Request, Id, UBFMod) ->
    rpc_v11_req_encode(Request, Id, UBFMod, true).

%% @spec (atom() | tuple(), binary(), atom(), boolean()) ->
%%       {undefined | term(), encoded_json_term()}
%% @doc Take an Erlang RPC term (atom or tuple, where 1st element of the
%% tuple is the RPC function to call) and extract the AuthInfo (if
%% SubstAuthInfoP is true) and encode the call atom/tuple as an intermediate
%% representation of a JSON object.
%%
%% The intermediate _JSON object needs to be string-ified before it\'s
%% really a JSON thing, because JSON things are strings.
%%
%% See EUnit test module ubf_jsonrpc_examples_test.erl for example
%% usage.

rpc_v11_req_encode(Method, Id, _UBFMod, _SubstAuthInfoP) when is_atom(Method) ->
    {undefined, {struct, [{<<"version">>, <<"1.1">>}, {<<"id">>, Id}, {<<"method">>, jsf:atom_to_binary(Method)}, {<<"params">>, []}]}};

rpc_v11_req_encode(X, Id, UBFMod, SubstAuthInfoP)
  when tuple_size(X) > 1, is_atom(element(1, X)),
       is_boolean(SubstAuthInfoP) ->
    [Method, Param0|Params_rest] = tuple_to_list(X),
    {Method, AuthInfo, Params} =
	if SubstAuthInfoP == true ->
		%% NOTE: assumes the second element of X is AuthInfo and remove
		%% it from the request sent to the server
		{Method, Param0, Params_rest};
	   SubstAuthInfoP == false ->
		{Method, undefined, [Param0|Params_rest]}
	end,
    EncodedParams = jsf:do_encode(Params,UBFMod),
    {AuthInfo, {struct, [{<<"version">>, <<"1.1">>}, {<<"id">>, Id}, {<<"method">>, jsf:atom_to_binary(Method)}, {<<"params">>, EncodedParams}]}}.


%%
%%---------------------------------------------------------------------
%%

rpc_v11_req_decode_print(AuthInfo, X, UBFMod) ->
    io:format("~s~n", [rpc_v11_req_decode(AuthInfo, X, UBFMod)]).

rpc_v11_req_decode(AuthInfo, X, UBFMod) ->
    rpc_v11_req_decode(AuthInfo, X, UBFMod, true).

rpc_v11_req_decode(AuthInfo, X, UBFMod, SubstAuthInfoP) ->
    try
	case classify(X) of
	    '8bit' ->
		%% same crash as rfc4627.erl and xmerl_ucs.erl
		exit({ucs,{bad_utf8_character_code}});
	    _ ->
		case mochijson2:decode(X) of
		    {struct, Props} ->
			{value, {<<"version">>, <<"1.1">>}, Props1} = lists:keytake(<<"version">>, 1, Props),
			{value, {<<"id">>, Id}, Props2} = lists:keytake(<<"id">>, 1, Props1),
			{value, {<<"method">>, MethodBin}, Props3} = lists:keytake(<<"method">>, 1, Props2),
			{value, {<<"params">>, JsonParams}, []} = lists:keytake(<<"params">>, 1, Props3),
			Method = jsf:binary_to_existing_atom(MethodBin),
			case catch (jsf:do_decode(JsonParams, UBFMod, true)) of
			    {'EXIT', Reason} ->
				{error, Reason, Id};
			    [] ->
				{ok, Method, Id};
			    Params ->
				if AuthInfo =:= undefined;
				   SubstAuthInfoP == false ->
					{ok, list_to_tuple([Method|Params]), Id};
				   true ->
					{ok, list_to_tuple([Method|[AuthInfo|Params]]), Id}
				end
			end;
		    Other ->
			{error, Other}
		end
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
    Z = {struct, [{<<"version">>, <<"1.1">>}, {<<"id">>, Id}, {<<"result">>, Result}, {<<"error">>, Error}]},
    Encoder = mochijson2:encoder([{utf8, true}]),
    iolist_to_binary(Encoder(Z)).


%%
%%---------------------------------------------------------------------
%%

rpc_v11_res_decode_print(X, UBFMod) ->
    io:format("~s~n", [rpc_v11_res_decode(X, UBFMod)]).

rpc_v11_res_decode(X, UBFMod) ->
    case classify(X) of
	'8bit' ->
	    %% same crash as rfc4627.erl and xmerl_ucs.erl
	    exit({ucs,{bad_utf8_character_code}});
	_ ->
	    case mochijson2:decode(X) of
		{struct, Props} ->
		    {value, {<<"version">>, <<"1.1">>}, Props1} = lists:keytake(<<"version">>, 1, Props),
		    {value, {<<"id">>, Id}, Props2} = lists:keytake(<<"id">>, 1, Props1),
		    {value, {<<"result">>, Result}, Props3} = lists:keytake(<<"result">>, 1, Props2),
		    {value, {<<"error">>, Error}, []} = lists:keytake(<<"error">>, 1, Props3),
		    {ok, jsf:do_decode(Result,UBFMod,true), jsf:do_decode(Error,UBFMod,true), Id};
		Other ->
		    {error, Other}
	    end
    end.


%%
%%---------------------------------------------------------------------
%%

classify(X) ->
    jsf_charset:classify(bin_ify(X)).

bin_ify(X) when is_binary(X) ->
    X;
bin_ify(X) when is_list(X) ->
    list_to_binary(X);
bin_ify(X) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
bin_ify(X) when is_atom(X) ->
    case get(X) of
        undefined -> B = list_to_binary(atom_to_list(X)),
                     put(X, B),
                     B;
        B when is_binary(B) -> B
    end.
