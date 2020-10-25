CXXFLAG = -Wall -Wextra -pedantic -std=c++11 -g

all: ekcc

ekcc: ekcc.cpp parser lexer
	$(CXX) $(CXXFLAG) -o ekcc lexer.c parser.tab.c ekcc.cpp

parser: parser.y
	bison -d parser.y

lexer: lexer.l parser
	flex -olexer.c lexer.l


clean: 
	rm -rf *.o lexer.c parser.tab.* ekcc *.yaml *.dSYM
