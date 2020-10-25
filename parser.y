%error-verbose

%{  
    #include "ast.h"
    #include "ekcc.h"
    #include <iostream>
    #include <string>

    #include <getopt.h> 
    #include <fstream>

    using namespace std;

    // Declare stuff from Flex that Bison needs to know about:
    extern "C" {
        int yylex();
        int yyparse();

        extern FILE *yyin;
        extern char *yytext;

        void yyerror(const char *s);

        int yywrap() {return 1;}
    }

    prog *the_prog;
%}

// add struct beforehand or typedef it somewhere else 

%union {
    int ival;
    float fval;
    char* sval;
    struct type *a_type;
    struct id *a_id;
    struct vdecl *a_vdecl;
    struct tdecls *a_tdecls;
    struct vdecls *a_vdecls;
    struct exp *a_exp;
    struct uop *a_uop;
    struct binop *a_binop;
    struct assign *a_assign;
    struct exps *a_exps;
    struct stmt *a_stmt;
    struct stmts *a_stmts;
    struct blk *a_blk;
    struct func *a_func;
    struct funcs *a_funcs;
    struct ext *a_ext;
    struct exts *a_exts;
}

/* Token definitions */
// Keywords
%token EXTERN DEF RETURN WHILE IF ELSE PRINT 

// Types
%token INT CINT FLOAT BOOL VOID NOALIAS REF
%token EQUAL AND OR

// Identifiers and literals
%token <ival> LITERAL_INT
%token <fval> LITERAL_FLOAT 
%token <sval> IDENTIFIER GIDENTIFIER LITERAL_STR 

// type 
%type <a_id> varid globid 
%type <a_type> type
%type <a_vdecl> vdecl 
%type <a_tdecls> tdecls 
%type <a_vdecls> vdecls 
%type <a_uop> uop 
%type <a_binop> binop 
%type <a_assign> assign 
%type <a_exp> exp 
%type <a_exps> exps 
%type <a_stmt> stmt 
%type <a_stmts> stmts 
%type <a_blk> blk 
%type <a_func> func 
%type <a_funcs> funcs 
%type <a_ext> extern
%type <a_exts> externs 

%nonassoc "then" "else"

// sequence of both within and between lines matter! 
%left OR AND EQ 
%left '<' '>' 
%left '+' '-' 
%left '*' '/'
%right '=' "uminus" '!'

%%
/* Grammer rules of the parser */

/*         inputs                                 actions */
prog     : externs funcs                          {the_prog = new prog($2, $1); } // reverse 
         | funcs                                  {the_prog = new prog($1); }                
         ;

externs  : externs extern                         {$$ = $1; $$->externs.push_back($2); } 
         | extern                                 {$$ = new exts(); $$->externs.push_back($1); } // end of externs 
         ;

extern   : EXTERN type globid '(' tdecls ')' ';'  {$$ = new ext($2, $3, $5); } // once 
         | EXTERN type globid '(' ')' ';'         {$$ = new ext($2, $3); } // none 

funcs    : funcs func                             {$$ = $1; $$->functions.push_back($2); }
         | func                                   {$$ = new funcs(); $$->functions.push_back($1); }
         ;

func     : DEF type globid '(' vdecls ')'blk      {$$ = new func($2, $3, $7, $5); }
         | DEF type globid '(' ')' blk            {$$ = new func($2, $3, $6); }
         ;

blk      : '{' stmts '}'                          {$$ = new blk($2); } // once 
         | '{' '}'                                {$$ = new blk(); } // none 
         ; 

stmts    : stmts stmt                             {$$ = $1; $$->statements.push_back($2); }
         | stmt                                   {$$ = new stmts(); $$->statements.push_back($1); }
         ;

stmt     : blk                                    {$$ = $1; }  
         | RETURN exp ';'                         {$$ = new ret($2); }
         | RETURN ';'                             {$$ = new ret(); }
         | vdecl '=' exp ';'                      {$$ = new vdeclstmt($1, $3); }
         | exp ';'                                {$$ = new expstmt($1); }
         | WHILE '(' exp ')' stmt                 {$$ = new whilestmt($3, $5); }
         | IF '(' exp ')' stmt ELSE stmt %prec "else" {$$ = new ifstmt($3, $5, $7); }
         | IF '(' exp ')' stmt %prec "then"       {$$ = new ifstmt($3, $5); }
         | PRINT exp ';'                          {$$ = new print($2); }
         | PRINT LITERAL_STR ';'                  {$$ = new printslit($2); free($2); }
         ;

exps     : exp                                    {$$ = new exps(); $$->expressions.push_back($1); }
         | exps ',' exp                           {$$ = $1; $$->expressions.push_back($3); }
         ; 

exp      : '(' exp ')'                            {$$ = $2; }
         | binop                                  {$$ = $1; }
         | assign                                 {$$ = $1; }
         | uop                                    {$$ = $1; }
         | LITERAL_INT                            {$$ = new lit($1); }
         | LITERAL_FLOAT                          {$$ = new flit($1); }
         | varid                                  {$$ = new varval($1); }
         | globid '(' exps ')'                    {$$ = new funccall($1, $3); }
         | globid '(' ')'                         {$$ = new funccall($1); }
         | '[' type ']' exp                       {$$ = new castexp($2, $4); }
         ; 

assign   : varid '=' exp                            {$$ = new assign($1, $3); }  
         ;

/* here binop did not follow strictly to the language specifications 
to save up additional type def for arith_ops and logic_ops */
binop    : exp '*' exp                            {$$ = new binop(binop::bop_mul, $1, $3); }
         | exp '/' exp                            {$$ = new binop(binop::bop_div, $1, $3); }
         | exp '+' exp                            {$$ = new binop(binop::bop_add, $1, $3); }
         | exp '-' exp                            {$$ = new binop(binop::bop_sub, $1, $3); }
         | exp EQUAL exp                          {$$ = new binop(binop::bop_eq, $1, $3); }
         | exp '<' exp                            {$$ = new binop(binop::bop_lt, $1, $3); }    
         | exp '>' exp                            {$$ = new binop(binop::bop_gt, $1, $3); }
         | exp AND exp                            {$$ = new binop(binop::bop_and, $1, $3); }
         | exp OR exp                             {$$ = new binop(binop::bop_or, $1, $3); }                
         ;

uop      : '!' exp                                {$$ = new uop(uop::uop_not, $2); }
         | '-' exp %prec "uminus"                 {$$ = new uop(uop::uop_minus, $2); }
         ;

// lit and slit and ident done in lexer.l and header  

varid    : IDENTIFIER                             {$$ = new id($1); free($1); }
         ;

globid   : GIDENTIFIER                            {$$ = new id($1); free($1); }
         ; 

type     : INT                                    {$$ = new type(type::t_int); } 
         | CINT                                   {$$ = new type(type::t_cint); }
         | FLOAT                                  {$$ = new type(type::t_float); }
         | BOOL                                   {$$ = new type(type::t_bool); }
         | VOID                                   {$$ = new type(type::t_void); }
         | NOALIAS REF type                       {$$ = $3, $$->ref = true; $$->noalias = true;}              
         | REF type                               {$$ = $2, $$->ref = true; }
         ; 

vdecls   : vdecl                                  {$$ = new vdecls(); $$->variables.push_back($1); }
         | vdecls ',' vdecl                       {$$ = $1; $$->variables.push_back($3); }
         ;

tdecls   : type                                   {$$ = new tdecls(); $$->types.push_back($1); }
         | tdecls ',' type                        {$$ = $1; $$->types.push_back($3); }
         ; 

vdecl    : type varid                             {$$ = new vdecl($1, $2); }
         ;

%%       

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

int main(int argc, char* argv[]) {
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

    ofstream os(output, ios::out);
    if (!os) {
        cout << "error: failed to open output file: " << output << endl;
        exit(-1);
    }

    do {
        yyparse();
    } while (!feof(yyin)); 

    if (emit_ast) {
        os << "---" << endl;
        the_prog->yaml(os, "");
        os << "..." << endl;
    }

    delete the_prog;
    return 0;
}

void yyerror(const char *s) {
  cout << "Parse error!  Message: " << s << endl;
  exit(-1);
}
