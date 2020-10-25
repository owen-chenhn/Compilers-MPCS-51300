CC = g++
CFLAG = -Wall -Wextra -pedantic -std=c++11 -g

all: ekcc

ekcc: lexer.l parser.y
	bison -d parser.y
	flex -olexer.c lexer.l
	$(CC) $(CFLAG) -o ekcc lexer.c parser.tab.c



clean: 
	rm -rf *.o lexer.c parser.tab.* ekcc *.yaml *.dSYM
