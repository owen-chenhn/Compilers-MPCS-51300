CC = g++
CFLAG = -Wall -Wextra -pedantic -std=c++11 -g

all: lexer.o
	$(CC) $(CFLAG) -o lexer $^

lexer.o: lexer.l
	flex -olexer.c lexer.l
	$(CC) -c lexer.c



clean: 
	rm *.o lexer.c lexer
