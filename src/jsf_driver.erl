%% @doc Protocol driver process for JSF (JavaScript Format) protocol
%% sessions.

-module(jsf_driver).
-behaviour(contract_driver).

-export([start/1, start/2, init/1, init/2, encode/3, decode/4]).

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

decode(Contract, Safe, {init, Rest, undefined}, Binary) ->
    Cont = jsf:decode_init(Safe, Rest),
    jsf:decode(Binary, Contract, Cont);
decode(Contract, _Safe, Cont, Binary) ->
    jsf:decode(Binary, Contract, Cont).

safe(Options) ->
    proplists:get_bool(safe, Options).
