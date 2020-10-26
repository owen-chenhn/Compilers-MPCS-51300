CXXFLAG = -Wall -Wextra -pedantic -std=c++11 -g

all: ekcc

ekcc: parser lexer main
	$(CXX) $(CXXFLAG) -o bin/ekcc bin/*

parser: parser.y
	bison -d parser.y		# bison -L c++ -o parser.cpp -d parser.y
	$(CXX) $(CXXFLAG) -c -o bin/parser.o parser.tab.c

lexer: lexer.l parser
	flex -olexer.c lexer.l		# flex -+ -o lexer.cpp lexer.l
	$(CXX) $(CXXFLAG) -c -o bin/lexer.o lexer.c

main: ekcc.cpp
	$(CXX) $(CXXFLAG) -c -o bin/main.o ekcc.cpp


clean: 
	rm -rf bin/* lexer.c parser.tab.* *.yaml *.dSYM
