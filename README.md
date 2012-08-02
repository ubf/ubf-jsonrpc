

#Universal Binary Format and JSON-RPC#


Copyright (c) 2011-2012 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).<p>This is UBF-JSONRPC, a framework for integrating UBF, JSF and
JSON-RPC.  This repository depends on the UBF and MOCHIWEB (actually
only the mochijson2.erl module) open source repositories.</p>
<p><em>This repository is intended for production deployment and is deployed
in carrier-grade systems.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download, build, and test the ubf_jsonrpc application in one shot,
please follow this recipe:</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/ubf/ubf-jsonrpc.git ubf_jsonrpc
$ cd ubf_jsonrpc
$ ./rebar get-deps
$ ./rebar clean
$ ./rebar compile
$ ./rebar eunit</tt></pre>

<p>For an alternative recipe with other "features" albeit more complex,
please read further.</p>



<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is a good first step.</p>
<p>The UBF User's Guide is the best next step.  Check out
<a href="http://ubf.github.com/ubf/ubf-user-guide.en.md">http://ubf.github.com/ubf/ubf-user-guide.en.html</a> for further
detailed information.</p>
<p>Eunit tests can be found in the test/eunit directory.  These tests
illustrate an inets-based httpd module that uses UBF's contract
manager for checking JSON-RPC requests and responses.</p>


<h3 id="_what_is_ubf">What is UBF?</h3>
<p>UBF is the "Universal Binary Format", designed and implemented by Joe
Armstrong.  UBF is a language for transporting and describing complex
data structures across a network.  It has three components:</p>
<ul>
<li>
<p>
UBF(a) is a "language neutral" data transport format, roughly
  equivalent to well-formed XML.
</p>
</li>
<li>
<p>
UBF(b) is a programming language for describing types in UBF(a) and
  protocols between clients and servers.  This layer is typically
  called the "protocol contract".  UBF(b) is roughly equivalent to
  Verified XML, XML-schemas, SOAP and WDSL.
</p>
</li>
<li>
<p>
UBF(c) is a meta-level protocol used between a UBF client and a UBF
  server.
</p>
</li>
</ul>
<p>See <a href="http://norton.github.com/ubf">http://norton.github.com/ubf</a> for further details.</p>


<h3 id="_what_is_jsf">What is JSF?</h3>
<p>JSF is an implementation of UBF(b) but does not use UBF(a) for
client-server communication.  Instead, JSON (RFC 4627) is used.</p>
<p>"JSF" is short for "JavaScript Format".</p>
<p>There is no agreed-upon convention for converting Erlang terms to JSON
objects.  This library uses the convention set forth by MochiWeb's
JSON library (see URL above).  In addition, there are a couple of
other conventions layered on top of MochiWeb's implementation.</p>
<p>The UBF(b) contract checker has been modified to make a distinction
between an Erlang record and an arbitrary Erlang tuple.  An experience
Erlang developer would view such a distinction either with skepticism
or with approval.</p>
<p>For the skeptics, the contract author has the option of having the
UBF(b) contract compiler automatically generate Erlang <tt>-record()</tt>
definitions for appropriate tuples within the contract.  Such record
definitions are very convenient for developers on the Erlang side of
the world, but they introduce more complication to the JavaScript side
of the world.  For example, JavaScript does not have a concept of an
arbitrary atom, as Erlang does.  Also, the JavaScript side must make a
distinction between <tt>{foo, 42}</tt> and <tt>{bar, 42}</tt> when <tt>#foo</tt> is a
record on the Erlang side but <tt>#bar</tt> is not.</p>
<p>This extra convention creates something slightly messy-looking, if you
look at the raw JSON passed back-and-forth.  The examples of the
Erlang record <tt>{foo, 42}</tt> and the general tuple <tt>{bar, 42}</tt> would look
like this:</p>


<pre><tt>record (defined in the contract as "foo() = #foo{attribute1 = term()};")

   {"$R":"foo", "attribute1":42}

general tuple

   {"$T":[{"$A":"bar"}, 42]}</tt></pre>

<p>However, it requires very little JavaScript code to convert objects
with the <tt>"!$R"</tt>, <tt>"$T"</tt>, and <tt>"$A"</tt> notation (for records, tuples,
and atoms) into whatever object is most convenient.</p>

<table><tr>
<td class="icon">
Tip
</td>
<td class="content">Gemini Mobile Technologies, Inc. has implemented a module for
classifying the input character set< to detect non-UTF8 JSON inputs.
This module has been released to the open-source world
(<a href="http://github.com/hibari/gmt-util/blob/master/src/gmt_charset.erl">http://github.com/hibari/gmt-util/blob/master/src/gmt_charset.erl</a>)
and copied to this repository as the jsf_charset.erl module.</td>
</tr></table>



<h3 id="_what_is_json_rpc">What is JSON-RPC?</h3>
<p>JSON-RPC is a remote procedure call protocol encoded in JSON.  See
<a href="http://json-rpc.org/">http://json-rpc.org/</a> for full details.</p>




<h2 id="_to_download">To download</h2>

<ol class="arabic">
<li>
<p>
Configure your e-mail and name for Git
</p>


<pre><tt>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</tt></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><tt>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</tt></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u https://github.com/ubf/manifests.git -m ubf-jsonrpc-default.xml</tt></pre>


<table><tr>
<td class="icon">
Note
</td>
<td class="content">Your "Git" identity is needed during the init step.  Please
enter the name and email of your GitHub account if you have one.  Team
members having read-write access are recommended to use "repo init -u
<a href="mailto:git@github.com">git@github.com</a>:ubf/manifests.git -m ubf-jsonrpc-default-rw.xml".</td>
</tr></table>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">If you want to checkout the latest development version, please
append " -b dev" to the repo init command.</td>
</tr></table>

</li>
<li>
<p>
Download Git repositories
</p>


<pre><tt>$ cd working-directory-name
$ repo sync</tt></pre>

</li>
</ol>
<p>For futher information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R13B04 or newer, R15B has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
Git - <a href="http://git-scm.com/">http://git-scm.com/</a>
</p>
<ul>
<li>
<p>
<strong>Git 1.5.4 or newer, Git 1.7.9.3 has been tested recently</strong>
</p>
</li>
<li>
<p>
<em>required for Repo and GitHub</em>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
<li>
<p>
Python - <a href="http://www.python.org">http://www.python.org</a>
</p>
<ul>
<li>
<p>
<strong>Python 2.4 or newer, Python 2.7.1 has been tested most recently
    (CAUTION: Python 3.x might be too new)</strong>
</p>
</li>
<li>
<p>
<em>required for Repo</em>
</p>
</li>
</ul>
</li>
<li>
<p>
Rebar - <a href="https://github.com/basho/rebar/wiki">https://github.com/basho/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.md">http://source.android.com/source/git-repo.html</a>
</p>
</li>
</ul>



<h2 id="_to_build_basic_recipe">To build - basic recipe</h2>

<ol class="arabic">
<li>
<p>
Get and install an erlang system <a href="http://www.erlang.org">http://www.erlang.org</a>
</p>
</li>
<li>
<p>
Build
</p>


<pre><tt>$ cd working-directory-name
$ make compile</tt></pre>

</li>
<li>
<p>
Run the unit tests
</p>


<pre><tt>$ cd working-directory-name
$ make eunit</tt></pre>

</li>
</ol>



<h2 id="_to_build_optional_features">To build - optional features</h2>

<ol class="upperalpha">
<li>
<p>
Dialyzer Testing <em>basic recipe</em>
</p>
<ol class="arabic">
<li>
<p>
Build Dialyzer's PLT <em>(required once)</em>
</p>


<pre><tt>$ cd working-directory-name
$ make build-plt</tt></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">Check Makefile and dialyzer's documentation for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze with specs
</p>


<pre><tt>$ cd working-directory-name
$ make dialyze</tt></pre>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">If you manually run dialyzer with the "-r" option, execute
"make clean compile" first to avoid finding duplicate beam files
underneath rebar's .eunit directory.  Check Makefile for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze without specs
</p>


<pre><tt>$ cd working-directory-name
$ make dialyze-nospec</tt></pre>

</li>
</ol>
</li>
</ol>



<h2 id="_credits">Credits</h2>

<p>Many, many thanks to Joe Armstrong, UBF's designer and original
implementor.</p>
<p>Thanks to MochiWeb.  UBF-JSONRPC relies on the MochiWeb
(i.e. mochijson2.erl) application for encoding and decoding JSON in
erlang.</p>
<p>Gemini Mobile Technologies, Inc. has approved the release of this
repository under an MIT license.</p>




##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/ubf/ubf-jsonrpc/blob/master/doc/jsf.md" class="module">jsf</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-jsonrpc/blob/master/doc/jsf_charset.md" class="module">jsf_charset</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-jsonrpc/blob/master/doc/jsf_driver.md" class="module">jsf_driver</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-jsonrpc/blob/master/doc/jsf_utils.md" class="module">jsf_utils</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-jsonrpc/blob/master/doc/ubf_jsonrpc.md" class="module">ubf_jsonrpc</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-jsonrpc/blob/master/doc/ubf_jsonrpc_inets_httpc_simple.md" class="module">ubf_jsonrpc_inets_httpc_simple</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-jsonrpc/blob/master/doc/ubf_jsonrpc_inets_httpd_simple.md" class="module">ubf_jsonrpc_inets_httpd_simple</a></td></tr>
<tr><td><a href="https://github.com/ubf/ubf-jsonrpc/blob/master/doc/ubf_jsonrpc_inets_httpd_simple_auth.md" class="module">ubf_jsonrpc_inets_httpd_simple_auth</a></td></tr></table>

