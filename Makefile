erlxslt: erlxslt.c
	gcc $< -o $@ -Wall -g \
	`pkg-config --cflags --libs libxml-2.0 libxslt libexslt`
