%error-verbose

%{
    #include <iostream>
    #include <string>
    using namespace std;

    // Declare stuff from Flex that Bison needs to know about:
    int yylex();
    int yyparse();

    extern FILE *yyin;
    extern char *yytext;

    void yyerror(const char *s);
%}

%union {
    int ival;
    float fval;
    char* sval;
}

/* Token definitions */
// Keywords
%token EXTERN DEF RETURN WHILE IF ELSE PRINT 

// Types
%token INT CINT FLOAT BOOL VOID NOALIAS REF
%token TRUE FALSE EQUAL AND OR

// Identifiers and literals
%token <sval> IDENTIFIER LITERAL_STR
%token <ival> LITERAL_INT
%token <fval> LITERAL_FLOAT


%%
/* Grammer rules of the parser */
exps: exp 
    | exps exp

exp: INT '$' IDENTIFIER '=' LITERAL_INT ';' { cout << "IDENTIFIER: " << $3 << "; " << "LITERAL_INT: " << $5 << endl; free($3); } 
    | PRINT LITERAL_STR ';' { cout << "LITERAL_STR: " << $2 << endl; free($2); }

%%

int main(int argc, char* argv[]) {
    if (argc > 1) {
        FILE *f = fopen(argv[1], "r");
        if (!f) {
            cout << "Input file: " << argv[1] << " not found.\n";
            return -1;
        }
        yyin = f;
    }

    yyparse();
    return 0;
}

void yyerror(const char *s) {
  cout << "Parse error!  Message: " << s << endl;
  exit(-1);
}
