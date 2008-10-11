-module(erlxslt_test).

-export([run/0]).

run() ->
    %%test_single(),
    %%test_multi(10000),
    test_extfun(),
    ok.

stylesheet() ->
    "<xsl:stylesheet version='1.0'
                     xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
                     xmlns:x='foo:bar'>
<xsl:output method='text' omit-xml-declaration='yes'/>
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
    erlxslt:process(X, stylesheet(), "<document>Hello World</document>"),
    erlxslt:stop(X).

test_extfun() ->
    {ok, X} = erlxslt:start_link(),
    erlxslt:register_function(X, "foo:bar", "arity0",
			      fun() -> ok end),
    erlxslt:register_function(X, "foo:bar", "arity1",
			      fun(23) -> ok end),
    erlxslt:register_function(X, "foo:bar", "arity2",
			      fun(23, 42) -> ok end),
    io:format("process -> ~p~n",[erlxslt:process(X, stylesheet(), "<extfun/>")]),
    erlxslt:stop(X).    

test_multi(N) ->
    {ok, X} = erlxslt:start_link(),
    test_multi(X, N),
    erlxslt:stop(X).

test_multi(_, 0) -> ok;
test_multi(X, N) ->
    erlxslt:process(X, stylesheet(), "<document>Hello World</document>"),
    test_multi(X, N - 1).

