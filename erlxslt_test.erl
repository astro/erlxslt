-module(erlxslt_test).

-export([run/0]).

run() ->
    test_single(),
    test_multi(10000).

stylesheet() ->
    "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
<xsl:output method='text'/>
<xsl:template match='*[count(*) = 0]'>
  <xsl:value-of select='concat(name(), \": \", string())'/>
</xsl:template>
</xsl:stylesheet>".

test_single() ->
    {ok, X} = erlxslt:start_link(),
    erlxslt:set_xslt(X, "style.xsl", stylesheet()),
    erlxslt:set_xml(X, "doc.xml", "<document>Hello World</document>"),
    {ok, Data} = erlxslt:process(X),
    io:format("Result: ~p~n", [Data]),
    erlxslt:exit(X).

test_multi(N) ->
    {ok, X} = erlxslt:start_link(),
    test_multi(X, N),
    erlxslt:exit(X).

test_multi(_, 0) -> ok;
test_multi(X, N) ->
    erlxslt:set_xslt(X, "style.xsl", stylesheet()),
    erlxslt:set_xml(X, "doc.xml", "<document>Hello World</document>"),
    {ok, _} = erlxslt:process(X),
    test_multi(X, N - 1).

