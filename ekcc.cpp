/* The driver source file for the application. */
#include <iostream>
#include <string>
#include <getopt.h> 
#include <fstream>

#include "parser.tab.h"
#include "ast.h"
#include "ekcc.h"

using namespace std;

/* Print helper messages. */
void Header() {
    cout << "EKCC - A compiler for language Extended-Kaleidoscope.\n" << 
            "Author: \n\tHaoning Chen (owenchen97@uchicago.edu), Yi Ding (yiding1@uchicago.edu)\n";
}

void Usage() {
    cout << "Usage: ./bin/ekcc [-h|-?] [-v] [-O] [-emit-ast|-emit-llvm] -o <output-file> <input-file>\n" << 
            "\t" << "-h|-?: Print helper message.\n" << 
            "\t" << "-v: Enable verbose mode.\n" << 
            "\t" << "-O: Enable optimization.\n" << 
            "\t" << "-emit-ast: Produce the AST of the input program to the output file.\n" << 
            "\t" << "-emit-llvm: Produce the LLVM IR of the input program to the output file.\n" << 
            "\t" << "-o <output-file>: Path to the output file.\n" << 
            "\t" << "<input-file>: Path to the input ek program.\n";
}

extern int yyparse();

int main(int argc, char* argv[]) {
    extern FILE *yyin;
    extern prog *the_prog;

    bool verbose = false, optimize = false, emit_ast = false, emit_llvm = false;
    string output = "";

    // Command-line options parsing
    const char *optstring = "h?vOo:";
    const struct option longopts[] = {
        {"emit-ast", no_argument, nullptr, 'a'}, 
        {"emit-llvm", no_argument, nullptr, 'l'},
        {0, 0, 0, 0}
    };
    int optret;
    int longindex;
    while ((optret = getopt_long_only(argc, argv, optstring, longopts, &longindex)) != -1) {
        switch(optret) {
        case 'h' :
        case '?' :
            Header();
            Usage();
            exit(0);
        case 'v' :
            verbose = true;
            break;
        case 'O' :
            optimize = true;
            break;
        case 'o' :
            output = optarg;
            break;
        case 'a' :
            emit_ast = true;
            break;
        case 'l' :
            emit_llvm = true;
            break;
        }
    }
    
    if (emit_ast && emit_llvm) {
        cout << "error: flag -emit-ast and flag -emit-llvm cannot be set simultaneously.\n";
        exit(-1);
    }
    if (output.size() == 0) {
        cout << "error: no output file specified.\n";
        exit(-1);
    }
    if (optind == argc) {
        cout << "error: no input file specified.\n";
        exit(-1);
    }

    FILE *in_f = fopen(argv[optind], "r");
    if (!in_f) {
        cout << "error: input file: " << argv[optind] << " not found.\n";
        return -1;
    }
    yyin = in_f;

    do {
        yyparse();
    } while (!feof(yyin)); 

    ofstream os(output, ios::out);
    if (!os) {
        cout << "error: failed to open output file: " << output << endl;
        exit(-1);
    }

    if (emit_ast) {
        os << "---" << endl;
        the_prog->yaml(os, "");
        os << "..." << endl;
    }

    delete the_prog;
    return 0;
}