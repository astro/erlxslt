erlxslt: erlxslt.c
	gcc -g $< -o $@ -Wall \
	`pkg-config --cflags --libs libxml-2.0 libxslt libexslt` \
	-I`echo /usr/lib/erlang/lib/erl_interface*/include/` \
	-L`echo /usr/lib/erlang/lib/erl_interface*/lib/` \
	-lei

