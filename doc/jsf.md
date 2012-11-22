

#Module jsf#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Functions for JSON<[8594,69,114,108,97,110,103,32,100,97,116,97,32,99,111,110,118,101,114,115,
 105,111,110,46]</p>


<pre><code>For most purposes, these functions are not called by code outside of
this library: Erlang client &amp; Erlang server application code usually
have no need to use these functions.</code></pre>



<pre><code>== Links</code></pre>



<pre><code><ul>
<li> http://www.erlang-projects.org/Public/news/ejson/view </li>
<li> http://www.erlang.org/eeps/eep-0018.html </li>
<li> http://www.erlang.org/ml-archive/erlang-questions/200511/msg00193.html </li>
<li> http://www.ietf.org/rfc/rfc4627.txt </li>
<li> http://www.json.org/ </li>
<li> http://www.lshift.net/blog/2007/02/17/json-and-json-rpc-for-erlang </li>
<li> http://www.json.com/json-schema-proposal/ </li>
</ul></code></pre>



<pre><code>== JSON Basic Data Types
------
object
          {}
          { members }
members
          pair
          pair, members
pair
          string : value
array
         []
         [ elements ]
elements
         value
         value, elements
value
         string
         number
         object
         true (atom)
         false (atom)
         null (atom)
------</code></pre>



<pre><code>== Mapping: JSON -> Erlang Terms, using mochiweb
------
json::object() = {struct, [json::pair()]}</code></pre>



<pre><code>json::pair() = {string(), json::value()}
     string() = [byte()]
     byte() = integer()</code></pre>



<pre><code>json::array() = [json::value()]</code></pre>



<pre><code>json::value() = json::object() | json::array() | json::number() | json::string() | json::true() | json::false() | json::null()</code></pre>



<pre><code>json::number() = integer() | float()</code></pre>



<pre><code>json::string() = binary()</code></pre>



<pre><code>json::true() = true
json::false() = false
json::null() = null
------</code></pre>



<pre><code>== Mapping: UBF -> Erlang Terms
------
ubf::tuple() = tuple()</code></pre>



<pre><code>ubf::list() = list()</code></pre>



<pre><code>ubf::number = integer() | float()</code></pre>



<pre><code>ubf::string() = {'$S', [integer()]}</code></pre>



<pre><code>ubf::proplist() = {'$P', [{term(), term()}]}</code></pre>



<pre><code>ubf::binary() = binary()</code></pre>



<pre><code>ubf::true() = true
ubf::false() = false
ubf::undefined() = undefined</code></pre>



<pre><code>ubf::atom() = atom()</code></pre>



<pre><code>ubf::record() = record()
------</code></pre>



<pre><code>== Mapping: UBF value -> JSON value
------
ubf::tuple() = {struct, [{<<"$T">>, ubf::list()}]}</code></pre>



<pre><code>ubf::list() = [value()]</code></pre>



<pre><code>ubf::number() = integer() | float()</code></pre>



<pre><code>ubf::string() = {struct, [{<<"$S">>, binary()}]}</code></pre>



<pre><code>ubf::proplist() = {struct, [{binary(), value()}]}</code></pre>



<pre><code>ubf::binary() = binary()</code></pre>



<pre><code>ubf::true() = true
ubf::false() = false
ubf::undefined() = null</code></pre>



<pre><code>ubf::atom() = {struct, [{<<"$A">>, atomname()}]}
     atomname() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the actual atom</code></pre>



<pre><code>ubf::record() = {struct, [{<<"$R">>, recordname()}] ++ [recordpair()]}
     recordname() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the record's name
     recordpair() = {recordkey(), value()}
     recordkey() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the record key's name</code></pre>



<pre><code>value() = ubf::tuple() | ubf::list() | ubf::number() | ubf::string() | ubf::binary() | ubf::true() | ubf::false() | ubf::undefined() | ubf::atom() | ubf::record()
------</code></pre>
.

__Behaviours:__ [`contract_proto`](contract_proto.md).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#atom_to_binary-1">atom_to_binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary_to_atom-1">binary_to_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary_to_existing_atom-1">binary_to_existing_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#contract_records-0">contract_records/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-3">decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-0">decode_init/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-1">decode_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-2">decode_init/2</a></td><td></td></tr><tr><td valign="top"><a href="#do_decode-2">do_decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#do_decode-3">do_decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#do_encode-2">do_encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#proto_driver-0">proto_driver/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_packet_type-0">proto_packet_type/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_vsn-0">proto_vsn/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="atom_to_binary-1"></a>

###atom_to_binary/1##


`atom_to_binary(X) -> any()`

<a name="binary_to_atom-1"></a>

###binary_to_atom/1##


`binary_to_atom(X) -> any()`

<a name="binary_to_existing_atom-1"></a>

###binary_to_existing_atom/1##


`binary_to_existing_atom(X) -> any()`

<a name="contract_records-0"></a>

###contract_records/0##


`contract_records() -> any()`

<a name="decode-1"></a>

###decode/1##


`decode(X) -> any()`

<a name="decode-2"></a>

###decode/2##


`decode(X, Mod) -> any()`

<a name="decode-3"></a>

###decode/3##


`decode(X, Mod, State) -> any()`

<a name="decode_init-0"></a>

###decode_init/0##


`decode_init() -> any()`

<a name="decode_init-1"></a>

###decode_init/1##


`decode_init(Safe) -> any()`

<a name="decode_init-2"></a>

###decode_init/2##


`decode_init(Safe, Binary) -> any()`

<a name="do_decode-2"></a>

###do_decode/2##


`do_decode(X, Mod) -> any()`

<a name="do_decode-3"></a>

###do_decode/3##


`do_decode(X, Mod, Safe) -> any()`

<a name="do_encode-2"></a>

###do_encode/2##


`do_encode(X, Mod) -> any()`

<a name="encode-1"></a>

###encode/1##


`encode(X) -> any()`

<a name="encode-2"></a>

###encode/2##


`encode(X, Mod) -> any()`

<a name="proto_driver-0"></a>

###proto_driver/0##


`proto_driver() -> any()`

<a name="proto_packet_type-0"></a>

###proto_packet_type/0##


`proto_packet_type() -> any()`

<a name="proto_vsn-0"></a>

###proto_vsn/0##


`proto_vsn() -> any()`

