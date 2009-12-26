-module(ubf_jsonrpc_inets_httpd_simple).

%% @doc Fake being an Erlang parameterized module, by exporting a new()
%%      function.
%%
%% Erlang parameterized modes don't support variable # of args for
%% its auto-generated new() function.  So we fake it here, calling
%% the new func for the real parameterized module.

-export([new/2, new/3]).

new(Contract, Url) ->
    ubf_jsonrpc_inets_httpd_simple_auth:new(Contract, Url, true).

new(Contract, Url, Subst_AuthInfo) when is_boolean(Subst_AuthInfo) ->
    ubf_jsonrpc_inets_httpd_simple_auth:new(Contract, Url, Subst_AuthInfo).
