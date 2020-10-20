CC = g++
CFLAG = -Wall -Wextra -pedantic -std=c++11 -g

all: ekcc.o
	$(CC) $(CFLAG) -o ekcc $^

ekcc.o: lexer.l parser.y
	bison -d parser.y
	flex -olexer.c lexer.l
	$(CC) -c lexer.c parser.tab.c



clean: 
	rm -rf *.o lexer.c parser.tab.* ekcc
