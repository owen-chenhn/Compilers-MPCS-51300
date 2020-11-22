/* The driver source file for the application. */
#include <iostream>
#include <string>
#include <getopt.h> 
#include <fstream>
#include <memory>
#include <system_error>
#include <cstdlib>

#include "parser.tab.h"
#include "ast.h"
#include "ekcc.h"

#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Bitcode/BitcodeWriter.h"

using namespace std;
using namespace llvm;

/* Print helper messages. */
void Header() {
    cout << "EKCC - A compiler for language Extended-Kaleidoscope.\n" << 
            "Author: \n\tHaoning Chen (owenchen97@uchicago.edu), Yi Ding (yiding1@uchicago.edu)\n";
}

void Usage() {
    cout << "Usage: ./bin/ekcc [-h|-?] [-v] [-O] [-emit-ast|-emit-llvm|-jit] -o <output-file> <input-file>\n" << 
            "\t" << "-h|-?: Print helper message.\n" << 
            "\t" << "-v: Enable verbose mode.\n" << 
            "\t" << "-O: Enable optimization.\n" << 
            "\t" << "-emit-ast: Produce the AST of the input program to the output file.\n" << 
            "\t" << "-emit-llvm: Produce the LLVM IR of the input program to the output file.\n" << 
            "\t" << "-jit: Produce the executable of the input program as the output file.\n" <<
            "\t" << "-o <output-file>: Path to the output file.\n" << 
            "\t" << "<input-file>: Path to the input ek program.\n";
}

extern int yyparse();

int main(int argc, char* argv[]) {
    extern FILE *yyin;
    extern prog *the_prog;

    bool verbose = false, optimize = false, emit_ast = false, emit_llvm = false, jit = false;
    string output = "";

    // Command-line options parsing
    const char *optstring = "h?vOo:";
    const struct option longopts[] = {
        {"emit-ast", no_argument, nullptr, 'a'}, 
        {"emit-llvm", no_argument, nullptr, 'l'},
        {"jit", no_argument, nullptr, 'j'},
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
        case 'j' :
            jit = true;
            break; 
        }
    }
    
    if (emit_ast && emit_llvm || emit_ast && jit || emit_llvm && jit) {
        cout << "error: At most one of the flags -emit-ast / -emit-llvm / -jit can be set.\n";
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

    if (verbose) {
        Header();
        cout << "Input file path: " << argv[optind] << "\nStarting parsing input program and build AST.\n";
    }

    do {
        yyparse();
    } while (!feof(yyin)); 

    if (verbose) {
        cout << "Finished input parsing.\n";
    }

    ofstream os(output, ios::out);
    if (!os) {
        cout << "error: failed to open output file: " << output << endl;
        exit(-1);
    }

    if (emit_ast) {
        if (verbose) cout << "Emit AST to output file: " << output << endl;
        os << "---" << endl;
        the_prog->yaml(os, "");
        os << "..." << endl;
        if (verbose) cout << "Finished emitting AST.\n";
    } else { // need to codegen 
        if (verbose) cout << "Start generating LLVM IR code.\n";
        Module *the_module = the_prog->code_gen();

        // Add optimization
        if (optimize) { cout << "Start code optimization.\n"; }

        error_code EC;
        string raw_OS_path = emit_llvm ? output : "intermediate.ll";
        raw_fd_ostream OS(raw_OS_path, EC);
        the_module->print(OS, nullptr);
        OS.flush();
        if (verbose) cout << "Finished generating LLVM IR code.\n";

        if (emit_llvm) {
            // No more things need to do.
            goto quit;
        } else if (jit) {
            cout << "Run jit.\n";
            the_prog->jit();
        } else { // generate exe 
            if (verbose) cout << "Compile code to an executable.\n";
            string command = "llc -filetype=obj intermediate.ll && clang++ lib.o intermediate.o -o " + output;
            std::system(command.c_str());
            std::system("rm intermediate.ll");
        }
    }

quit:
    delete the_prog;
    if (verbose) cout << "Compilation finished. Quit.\n";
    return 0;
}
