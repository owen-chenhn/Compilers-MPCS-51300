/* The driver source file for the application. */
#include <iostream>
#include <string>
#include <getopt.h> 
#include <fstream>

#include "parser.tab.h"
#include "ast.h"
#include "ekcc.h"

using namespace std;

void yyerror(const char *s) {
  cerr << "Parse error!  Message: " << s << endl;
}

extern int yyparse();

void test_my_compiler(const uint8_t *Data, size_t Size) {
    extern FILE *yyin;
    extern prog *the_prog;
    ostream &os = cout;
    FILE *in_f = fmemopen((void*) Data, Size, "r");

    if (!in_f) {
        cout << "error: empty input.\n";
        return;
    }

    yyin = in_f;
    do {
        yyparse();
    } while (!feof(yyin)); 

    the_prog->yaml(os, "");

    delete the_prog;
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t Size) {
    test_my_compiler(Data, Size);
    return 0;
}