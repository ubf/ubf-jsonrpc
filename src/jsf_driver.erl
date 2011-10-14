%% @doc Protocol driver process for JSF (JavaScript Format) protocol
%% sessions.

-module(jsf_driver).
-behaviour(contract_driver).

-export([start/1, start/2, init/1, init/2, encode/3, decode/5]).

start(Contract) ->
    start(Contract, []).

start(Contract, Options) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract, Options) end, jsf_client_driver).

init(Contract) ->
    init(Contract, []).

init(_Contract, Options) ->
    Safe = safe(Options),
    {Safe, jsf:decode_init(Safe)}.

encode(Contract, _Safe, Term) ->
    [jsf:encode(Term, Contract), "\n"].

decode(Contract, Safe, Cont, Binary, CallBack) ->
    Cont1 = jsf:decode(Binary, Contract, Cont),
    decode(Contract, Safe, Cont1, CallBack).

decode(Contract, Safe, {ok, Term, Binary}=_Cont, CallBack) ->
    CallBack(Term),
    Cont1 = jsf:decode_init(Safe, Binary),
    decode(Contract, Safe, Cont1, CallBack);
decode(_Contract, _Safe, Cont, _CallBack) ->
    Cont.

safe(Options) ->
    proplists:get_bool(safe, Options).
