/* The driver source file for the application. */
#include <iostream>
#include <string>
#include <getopt.h> 
#include <fstream>
#include <memory>
#include <system_error>
#include <cstdlib>
#include <chrono> 

#include "parser.tab.h"
#include "ast.h"
#include "ekcc.h"

#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/SROA.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/IPO/Inliner.h"
#include "llvm/IR/LegacyPassManager.h"
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
    cout << "Usage: ./bin/ekcc [-h|-?] [-v] [-O] [-funcinline] [-instcmb] [-reassoc] [-cfgsimp] [-emit-ast|-emit-llvm] [-jit] [-t] -o <output-file> <input-file>\n" << 
            "\t" << "-h|-?: Print helper message.\n" << 
            "\t" << "-v: Enable verbose mode.\n" << 
            "\t" << "-O: Enable optimizations.\n" <<
            "\t" << "-funcinline: Enable llvm -inline optimization if -O enabled. \n" <<
            "\t" << "-instcmb: Enable llvm -instcombine optimization if -O enabled. \n" <<
            "\t" << "-reassoc: Enable llvm -reassociate optimization if -O enabled. \n" <<
            "\t" << "-cfgsimp: Enable llvm -simplifycfg optimization if -O enabled. \n" <<                      
            "\t" << "-emit-ast: Produce the AST of the input program to the output file.\n" << 
            "\t" << "-emit-llvm: Produce the LLVM IR of the input program to the output file.\n" << 
            "\t" << "-jit: Run input program directly. [Note: when this flag is present, <output-file> (-o flag) does not need to be specified]\n" <<
            "\t" << "-t: print the run time of the program if -jit provided, and compile time optimization costs if optimization is on. \n" << 
            "\t" << "-o <output-file>: Path to the output file. [Note: whenever -jit flag is present, an output file is always produced]\n" << 
            "\t" << "<input-file>: Path to the input ek program.\n";
}

extern int yyparse();

int main(int argc, char* argv[]) {
    extern FILE *yyin;
    extern prog *the_prog;

    bool verbose = false, optimize = false, emit_ast = false, emit_llvm = false, out_flag = false, jit = false, 
         time = false, func_inline = false, inst_comb = false, reassoc = false, cfg_simp = false;
    string output = "";

    // Command-line options parsing
    const char *optstring = "h?vOto:";
    const struct option longopts[] = {
        {"funcinline", no_argument, nullptr, 'f'},
        {"instcmb", no_argument, nullptr, 'i'},
        {"reassoc", no_argument, nullptr, 'r'},
        {"cfgsimp", no_argument, nullptr, 'c'},       
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
            out_flag = true;
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
        case 't' : 
            time = true;
            break;
        case 'f':
            func_inline = true;
            break;
        case 'r':
            reassoc = true;
            break;
        case 'i':
            inst_comb = true;
            break;
        case 'c':
            cfg_simp = true;
            break;
        }
    }
    
    if (emit_ast && emit_llvm) {
        cout << "error: At most one of the flags -emit-ast / -emit-llvm can be set.\n";
        exit(-1);
    }
    if ((emit_ast || emit_llvm) && !out_flag) {
        cout << "error: emit flag (-emit-*) set but output path (-o) not set.\n";
        exit(-1);
    }
    if (!out_flag && !jit) {
        cout << "error: You need to specify either an output path (-o) or JIT (-jit).\n";
        exit(-1);
    }
    if (optind == argc) {
        cout << "error: No input file specified.\n";
        exit(-1);
    }

    if (!optimize) {
        if (func_inline || sroa || inst_comb || reassoc || cfg_simp) {
            cout << "error: detailed optimizations flags are not allowed without enabling -O first. \n";
            exit(-1);
        }
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
        cout << "Start generating LLVM IR code.\n";
    }
    Module* the_module = the_prog->code_gen();

    // Add optimization
    if (optimize) { 
        if (verbose) cout << "Start code optimization.\n"; 
        
        auto opt_start = chrono::system_clock::now();

        unique_ptr<legacy::FunctionPassManager> FPM(new legacy::FunctionPassManager(the_module));

        FPM->add(createBasicAAWrapperPass()); 

        if (inst_comb) FPM->add(createInstructionCombiningPass());
        if (reassoc) FPM->add(createReassociatePass());
        if (cfg_simp) FPM->add(createCFGSimplificationPass()); 
        //if (sroa) FPM->add(createSROAPass());
        
        unique_ptr<legacy::PassManager> MPM(new legacy::PassManager);

        if (func_inline) MPM->add(createFunctionInliningPass());
/*
        PassManagerBuilder PMBuilder;
        PMBuilder.OptLevel = 3;
        PMBuilder.SizeLevel = 0;
        PMBuilder.Inliner = llvm::createFunctionInliningPass(PMBuilder.OptLevel, PMBuilder.SizeLevel, false);
        PMBuilder.LoopVectorize = true;
        PMBuilder.populateFunctionPassManager(*FPM);
        PMBuilder.populateModulePassManager(*MPM);
*/
        FPM->doInitialization();
        for (Function &F : *the_module)
            FPM->run(F);
        FPM->doFinalization();

        MPM->run(*the_module);

        if (time) {
            cout << "Compile time cost of the optimizations: " 
                << chrono::duration<double>(chrono::system_clock::now() - opt_start).count() 
                << " seconds" << endl;
        }
    }

    if (jit) {
        if (verbose) cout << "Run program using JIT.\n";
        // Set the argument pointer
        int argidx = optind + 1;

        auto run_start = chrono::system_clock::now();

        the_prog->jit(argc-argidx, argv+argidx);

        if (time) {
            cout << "Running time of the program: " 
             << chrono::duration<double>(chrono::system_clock::now() - run_start).count() 
             << " seconds" << endl;
        }
    }

    if (out_flag) {
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
            goto quit;
        }

        error_code EC;
        string raw_OS_path = emit_llvm ? output : "intermediate.ll";
        raw_fd_ostream OS(raw_OS_path, EC);
        the_module->print(OS, nullptr);
        OS.flush();
        if (!emit_llvm)  {
            // generate exe 
            if (verbose) cout << "Compile code to an executable.\n";
            string command = optimize ? 
                             "llc -filetype=obj -O3 intermediate.ll && clang++ lib.o intermediate.o -O3 -o " : 
                             "llc -filetype=obj intermediate.ll && clang++ lib.o intermediate.o -o ";
            command += output;
            std::system(command.c_str());
            std::system("rm intermediate.ll");
        }
    }

quit:
    delete the_prog;
    if (verbose) cout << "Compilation finished. Quit.\n";
    return 0;
}
