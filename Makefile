CXXFLAG = -Wall -Wextra -pedantic -std=c++11 -g

all: ekcc

ekcc: parser lexer ast ast-llvm main
	rm -f bin/ekcc
	$(CXX) $(CXXFLAG) -o bin/ekcc bin/*

parser: parser.y ast.h
	bison -d parser.y		# bison -L c++ -o parser.cpp -d parser.y
	$(CXX) $(CXXFLAG) -c -o bin/parser.o parser.tab.c

lexer: lexer.l parser
	flex -olexer.c lexer.l		# flex -+ -o lexer.cpp lexer.l
	$(CXX) $(CXXFLAG) -c -o bin/lexer.o lexer.c

ast: ast.cpp ast.h
	$(CXX) $(CXXFLAG) -c -o bin/ast.o ast.cpp

ast-llvm: ast-llvm.cpp ast.h
	$(CXX) $(CXXFLAG) -c -o bin/ast-llvm.o ast-llvm.cpp

main: ekcc.cpp ekcc.h ast.h parser
	$(CXX) $(CXXFLAG) -c -o bin/main.o ekcc.cpp


clean: 
	rm -rf bin/* lexer.c parser.tab.* *.yaml *.dSYM
