CXXFLAG = -std=c++11 #-Wall -Wextra -pedantic 
LLVMFLAG = `llvm-config --cxxflags --ldflags --system-libs --libs`
CXX=clang++

all: ekcc lib

ekcc: parser lexer ast ast-llvm main
	rm -f bin/ekcc
	$(CXX) $(CXXFLAG) -o bin/ekcc bin/* $(LLVMFLAG)

parser: parser.y ast.h
	bison -d parser.y		# bison -L c++ -o parser.cpp -d parser.y
	$(CXX) $(CXXFLAG) -c -o bin/parser.o parser.tab.c $(LLVMFLAG)

lexer: lexer.l parser
	flex -olexer.c lexer.l		# flex -+ -o lexer.cpp lexer.l
	$(CXX) $(CXXFLAG) -c -o bin/lexer.o lexer.c

ast: ast.cpp ast.h
	$(CXX) $(CXXFLAG) -c -o bin/ast.o ast.cpp $(LLVMFLAG)

ast-llvm: ast-llvm.cpp ast.h
	$(CXX) $(CXXFLAG) -c -o bin/ast-llvm.o ast-llvm.cpp $(LLVMFLAG)

main: ekcc.cpp ekcc.h ast.h parser
	$(CXX) $(CXXFLAG) -c -o bin/main.o ekcc.cpp $(LLVMFLAG)

lib: lib-llvm.cpp
	$(CXX) $(CXXFLAG) -c -o lib.o lib-llvm.cpp
	#ar -crv libllvm.a lib.o
	#$(CXX) $(CXXFLAG) lib.o -shared -fPIC -o libllvm.so


clean: 
	rm -rf bin/* lexer.c parser.tab.* *.o *.ll *.a *.yaml *.out
