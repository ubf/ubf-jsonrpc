

#Module ubf_jsonrpc#
* [Function Index](#index)
* [Function Details](#functions)




<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#rpc_v11_req_decode-3">rpc_v11_req_decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#rpc_v11_req_decode-4">rpc_v11_req_decode/4</a></td><td></td></tr><tr><td valign="top"><a href="#rpc_v11_req_decode_print-3">rpc_v11_req_decode_print/3</a></td><td></td></tr><tr><td valign="top"><a href="#rpc_v11_req_encode-3">rpc_v11_req_encode/3</a></td><td></td></tr><tr><td valign="top"><a href="#rpc_v11_req_encode-4">rpc_v11_req_encode/4</a></td><td>Take an Erlang RPC term (atom or tuple, where 1st element of the  
tuple is the RPC function to call) and extract the AuthInfo (if  
SubstAuthInfoP is true) and encode the call atom/tuple as an intermediate  
representation of a JSON object.</td></tr><tr><td valign="top"><a href="#rpc_v11_req_encode_print-3">rpc_v11_req_encode_print/3</a></td><td></td></tr><tr><td valign="top"><a href="#rpc_v11_res_decode-2">rpc_v11_res_decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#rpc_v11_res_decode_print-2">rpc_v11_res_decode_print/2</a></td><td></td></tr><tr><td valign="top"><a href="#rpc_v11_res_encode-4">rpc_v11_res_encode/4</a></td><td></td></tr><tr><td valign="top"><a href="#rpc_v11_res_encode_print-4">rpc_v11_res_encode_print/4</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="rpc_v11_req_decode-3"></a>

###rpc_v11_req_decode/3##




`rpc_v11_req_decode(AuthInfo, X, UBFMod) -> any()`

<a name="rpc_v11_req_decode-4"></a>

###rpc_v11_req_decode/4##




`rpc_v11_req_decode(AuthInfo, X, UBFMod, SubstAuthInfoP) -> any()`

<a name="rpc_v11_req_decode_print-3"></a>

###rpc_v11_req_decode_print/3##




`rpc_v11_req_decode_print(AuthInfo, X, UBFMod) -> any()`

<a name="rpc_v11_req_encode-3"></a>

###rpc_v11_req_encode/3##




`rpc_v11_req_encode(Request, Id, UBFMod) -> any()`

<a name="rpc_v11_req_encode-4"></a>

###rpc_v11_req_encode/4##




<pre>rpc_v11_req_encode(Method::atom() | tuple(), Id::binary(), UBFMod::atom(), SubstAuthInfoP::boolean()) -&gt; {undefined | term(), [encoded_json_term()](#type-encoded_json_term)}</pre>
<br></br>






Take an Erlang RPC term (atom or tuple, where 1st element of the  
tuple is the RPC function to call) and extract the AuthInfo (if  
SubstAuthInfoP is true) and encode the call atom/tuple as an intermediate  
representation of a JSON object.



The intermediate _JSON object needs to be string-ified before it's really a  
JSON thing, because JSON things are strings.

See EUnit test module ubf_jsonrpc_examples_test.erl for example usage.<a name="rpc_v11_req_encode_print-3"></a>

###rpc_v11_req_encode_print/3##




`rpc_v11_req_encode_print(X, Id, UBFMod) -> any()`

<a name="rpc_v11_res_decode-2"></a>

###rpc_v11_res_decode/2##




`rpc_v11_res_decode(X, UBFMod) -> any()`

<a name="rpc_v11_res_decode_print-2"></a>

###rpc_v11_res_decode_print/2##




`rpc_v11_res_decode_print(X, UBFMod) -> any()`

<a name="rpc_v11_res_encode-4"></a>

###rpc_v11_res_encode/4##




`rpc_v11_res_encode(X, Y, Id, UBFMod) -> any()`

<a name="rpc_v11_res_encode_print-4"></a>

###rpc_v11_res_encode_print/4##




`rpc_v11_res_encode_print(X, Y, Id, UBFMod) -> any()`

