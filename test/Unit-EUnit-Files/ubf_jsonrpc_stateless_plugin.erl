-module(ubf_jsonrpc_stateless_plugin).
-behavior(ubf_plugin_stateless).

-include("ubf.hrl").

-export([info/0, description/0, keepalive/0]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1]).

-export([now/2]).
-export([client_breaks_req01/2, client_timeout_req03/2]).
-export([server_breaks_req01/2, server_timeout_req03/2, server_crash_req05/2]).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./test/Unit-EUnit-Files/ubf_jsonrpc_stateless_plugin").

info() ->
    "I am a stateless server".

description() ->
    "An stateless server programmed by UBF".

keepalive() ->
    ok.


%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> void()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.


%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc({Event,AuthInfo,Timeout})
  when Event==now
       ; Event==client_breaks_req01
       ; Event==client_timeout_req03
       ; Event==server_breaks_req01
       ; Event==server_timeout_req03
       ; Event==server_crash_req05
       ->
    ?MODULE:Event(AuthInfo,Timeout);
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    {Event, not_implemented}.


%%%----------------------------------------------------------------------
%%% Implementation functions
%%%----------------------------------------------------------------------

now(_AuthInfo, _Timeout) ->
    {ok, erlang:now()}.

client_breaks_req01(_AuthInfo, _Timeout) ->
    exit(client_breaks_req01_should_not_be_called).

client_timeout_req03(_AuthInfo, Timeout) ->
    timer:sleep(Timeout),
    client_timeout_res03.

server_breaks_req01(_AuthInfo, _Timeout) ->
    server_breaks_res01_with_this_response.

server_timeout_req03(_AuthInfo, Timeout) ->
    timer:sleep(Timeout),
    server_timeout_res03.

server_crash_req05(_AuthInfo, _Timeout) ->
    exit(server_crash_res05_with_this_response).
