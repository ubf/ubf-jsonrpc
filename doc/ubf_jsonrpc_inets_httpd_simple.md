

#Module ubf_jsonrpc_inets_httpd_simple#
* [Function Index](#index)
* [Function Details](#functions)




<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Fake being an Erlang parameterized module, by exporting a new()       
function.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="new-2"></a>

###new/2##




`new(Contract, Url) -> any()`





Fake being an Erlang parameterized module, by exporting a new()       
function.

Erlang parameterized modes don't support variable # of args for
its auto-generated new() function.  So we fake it here, calling
the new func for the real parameterized module.<a name="new-3"></a>

###new/3##




`new(Contract, Url, Subst_AuthInfo) -> any()`

