-module(erlxslt_test).

-export([run/0]).

run() ->
    test_single(),
    test_multi(10000),
    test_extfun(),
    ok.

stylesheet() ->
    "<xsl:stylesheet version='1.0'
                     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
                     xmlns:x='foo:bar'>
<xsl:output media-type='text/plain' method='text' omit-xml-declaration='yes'/>
<xsl:template match='extfun'>
  0 <xsl:value-of select='x:arity0()'/>
  1 <xsl:value-of select='x:arity1(23)'/>
  2 <xsl:value-of select='x:arity2(23,42)'/>
  29 <xsl:value-of select='x:arity29()'/>
</xsl:template>
<xsl:template match='document[count(*) = 0]'>
  <xsl:value-of select='string()'/>
</xsl:template>
</xsl:stylesheet>".

test_single() ->
    {ok, X} = erlxslt:start_link(),
    erlxslt:set_xslt(X, "style.xsl", stylesheet()),
    erlxslt:set_xml(X, "doc.xml", "<document>Hello World</document>"),
    {ok, "text/plain", Data} = erlxslt:process(X),
    io:format("Result: ~p~n", [Data]),
    erlxslt:stop(X).

test_extfun() ->
    {ok, X} = erlxslt:start_link(),
    erlxslt:register_function(X, "foo:bar", "arity0",
			      fun() -> ok end),
    erlxslt:register_function(X, "foo:bar", "arity1",
			      fun(23) -> ok end),
    erlxslt:register_function(X, "foo:bar", "arity2",
			      fun(23, 42) -> ok end),
    erlxslt:set_xslt(X, "style.xsl", stylesheet()),
    erlxslt:set_xml(X, "doc.xml", "<extfun/>"),
    io:format("process -> ~p~n",[erlxslt:process(X)]),
    erlxslt:stop(X).    

test_multi(N) ->
    {ok, X} = erlxslt:start_link(),
    test_multi(X, N),
    erlxslt:stop(X).

test_multi(X, 0) ->
    erlxslt:stop(X);

test_multi(X, N) ->
    erlxslt:set_xslt(X, "style.xsl", stylesheet()),
    erlxslt:set_xml(X, "doc.xml", "<document>Hello World</document>"),
    erlxslt:set_params(X, [{"foo","bar"},{"baz","foobar"}]),
    {ok, "text/plain", _} = erlxslt:process(X),
    test_multi(X, N - 1).

