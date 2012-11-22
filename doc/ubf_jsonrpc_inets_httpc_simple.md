

#Module ubf_jsonrpc_inets_httpc_simple#
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#do-4">do/4</a></td><td>Equivalent to <a href="#do-6"><tt>do(Url, Contract, Request, Id, [], [])</tt></a>.</td></tr><tr><td valign="top"><a href="#do-6">do/6</a></td><td>Equivalent to <a href="#do-7"><tt>do(Url, Contract, Request, Id, HTTPOptions, Options,
true)</tt></a>.</td></tr><tr><td valign="top"><a href="#do-7">do/7</a></td><td><p>Send an atom/tuple RPC call request to a JSON-RPC service at Url.</p>


<pre><code>Per JSON-RPC definition, if we return an +ok+ 4-tuple, either the
2nd element will be +undefined+ (i.e. call failed, see 3rd element) or the
3rd element will be +undefined+ (i.e. call succeeded, see 2nd element).</code></pre>



<pre><code>See the Inets app docs for http:request/4 for definition of HTTPOptions
and Options arguments.</code></pre>



<pre><code>If the SubstAuthInfoP flag is true, then the 2nd element of the
Request tuple is assumed to be authentication info of some kind.
It must be agreed upon that the contract is expecting such auth
info games, because the server side will extract auth info from the
HTTP session and insert that data into the 2nd element of Request,
which will effective replace whatever the client had placed in the
2nd element here on the client side.</code></pre>
.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="do-4"></a>

###do/4##


`do(Url, Contract, Request, Id) -> any()`

Equivalent to [`do(Url, Contract, Request, Id, [], [])`](#do-6).<a name="do-6"></a>

###do/6##


`do(Url, Contract, Request, Id, HTTPOptions, Options) -> any()`

Equivalent to [`do(Url, Contract, Request, Id, HTTPOptions, Options,true)`](#do-7).<a name="do-7"></a>

###do/7##


<pre>do(Url::string(), Contract::atom(), Request::atom() | tuple(), Id::string(), HTTPOptions::<a href="#type-proplist">proplist()</a>, Options::<a href="#type-proplist">proplist()</a>, SubstAuthInfoP::bool()) -> {ok, term() | undefined, term() | undefined, string()} | {error, term()}</pre>
<br></br>


<p>Send an atom/tuple RPC call request to a JSON-RPC service at Url.</p>


<pre><code>Per JSON-RPC definition, if we return an +ok+ 4-tuple, either the
2nd element will be +undefined+ (i.e. call failed, see 3rd element) or the
3rd element will be +undefined+ (i.e. call succeeded, see 2nd element).</code></pre>



<pre><code>See the Inets app docs for http:request/4 for definition of HTTPOptions
and Options arguments.</code></pre>



<pre><code>If the SubstAuthInfoP flag is true, then the 2nd element of the
Request tuple is assumed to be authentication info of some kind.
It must be agreed upon that the contract is expecting such auth
info games, because the server side will extract auth info from the
HTTP session and insert that data into the 2nd element of Request,
which will effective replace whatever the client had placed in the
2nd element here on the client side.</code></pre>

