

#Module jsf#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Functions for JSON<->Erlang data conversion.



__Behaviours:__ [`contract_proto`](https://github.com/norton/ubf/blob/master/doc/contract_proto.md).<a name="description"></a>

##Description##




For most purposes, these functions are not called by code outside of
this library: Erlang client & Erlang server application code usually  
have no need to use these functions.



###<a name="Links">Links</a>##



* http://www.erlang-projects.org/Public/news/ejson/view

* http://www.erlang.org/eeps/eep-0018.html

* http://www.erlang.org/ml-archive/erlang-questions/200511/msg00193.html

* http://www.ietf.org/rfc/rfc4627.txt

* http://www.json.org/

* http://www.lshift.net/blog/2007/02/17/json-and-json-rpc-for-erlang

* http://www.json.com/json-schema-proposal/





###<a name="JSON_Basic_Data_Types">JSON Basic Data Types</a>##

<pre>  object
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
           null (atom)</pre>



###<a name="Mapping:_JSON_->_Erlang_Terms,_using_mochiweb">Mapping: JSON -> Erlang Terms, using mochiweb</a>##

<pre>  json::object() = {struct, [json::pair()]}
 
  json::pair() = {string(), json::value()}
       string() = [byte()]
       byte() = integer()
 
  json::array() = [json::value()]
 
  json::value() = json::object() | json::array() | json::number() | json::string() | json::true() | json::false() | json::null()
 
  json::number() = integer() | float()
 
  json::string() = binary()
 
  json::true() = true
  json::false() = false
  json::null() = null</pre>



###<a name="Mapping:_UBF_->_Erlang_Terms">Mapping: UBF -> Erlang Terms</a>##

<pre>  ubf::tuple() = tuple()
 
  ubf::list() = list()
 
  ubf::number = integer() | float()
 
  ubf::string() = {'$S', [integer()]}
 
  ubf::proplist() = {'$P', [{term(), term()}]}
 
  ubf::binary() = binary()
 
  ubf::true() = true
  ubf::false() = false
  ubf::undefined() = undefined
 
  ubf::atom() = atom()
 
  ubf::record() = record()</pre>



###<a name="Mapping:_UBF_value_->_JSON_value">Mapping: UBF value -> JSON value</a>##

<pre>  ubf::tuple() = {struct, [{<<"$T">>, ubf::list()}]}
 
  ubf::list() = [value()]
 
  ubf::number() = integer() | float()
 
  ubf::string() = {struct, [{<<"$S">>, binary()}]}
 
  ubf::proplist() = {struct, [{binary(), value()}]}
 
  ubf::binary() = binary()
 
  ubf::true() = true
  ubf::false() = false
  ubf::undefined() = null
 
  ubf::atom() = {struct, [{<<"$A">>, atomname()}]}
       atomname() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the actual atom
 
  ubf::record() = {struct, [{<<"$R">>, recordname()}] ++ [recordpair()]}
       recordname() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the record's name
       recordpair() = {recordkey(), value()}
       recordkey() = binary()  % a.k.a. list_to_binary(atom_to_list()) for the record key's name
 
  value() = ubf::tuple() | ubf::list() | ubf::number() | ubf::string() | ubf::binary() | ubf::true() | ubf::false() | ubf::undefined() | ubf::atom() | ubf::record()</pre><a name="index"></a>

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

