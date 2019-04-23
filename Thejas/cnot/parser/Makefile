.PHONY: clean

../parser.sml: cmlib.sml lex.cmlex.sml lexer.sml parse.cmyacc.sml parser.sml
	cat local.sml cmlib.sml lex.cmlex.sml lexer.sml parse.cmyacc.sml parser.sml in_end.sml > ../parser.sml

cmlib.sml:
	$(MAKE) -C cmlib

lex.cmlex.sml: lex.cmlex
	$(shell which cmlex) lex.cmlex

parse.cmyacc.sml: parse.cmyacc
	$(shell which cmyacc) parse.cmyacc

clean:
	$(MAKE) clean -C cmlib
	rm -f ../parser.sml lex.cmlex.sml parse.cmyacc.sml
