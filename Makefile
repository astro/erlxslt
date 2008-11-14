ERLC=erlc
ERLCFLAGS=
CC=gcc
CFLAGS=-g -O2 -Wall
LD=gcc

all: erlxslt 	$(patsubst %.erl, %.beam, \
		$(wildcard *.erl))

%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) $<

erlxslt: erlxslt.c
	$(CC) $(CFLAGS) $< -o $@ \
	`pkg-config --cflags --libs libxml-2.0 libxslt libexslt` \
	-I`echo /usr/lib/erlang/lib/erl_interface*/include/` \
	-L`echo /usr/lib/erlang/lib/erl_interface*/lib/` \
	-lei

