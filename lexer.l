%{  
    #include "parser.tab.h"
    #include <iostream>
    #include <string>
    #include <cstdlib>

    using namespace std;
%}
%option noyywrap
%%
[ \t\n]+     { /* Ignore white space. */ }
[#].*\n      { /* Ignore comments. */ }

extern       { return EXTERN; }
def          { return DEF; }
return       { return RETURN; }
while        { return WHILE; }
if           { return IF; }
else         { return ELSE; }
print        { return PRINT; }
true         { return TRUE; }
false        { return FALSE; }
int          { return INT; }
cint         { return CINT; }
float        { return FLOAT; }
bool         { return BOOL; }
void         { return VOID; }
noalias      { return NOALIAS; }
ref          { return REF; }

"=="         { return EQUAL; }
"&&"         { return AND; }
"||"         { return OR; }
[(){}\[\],;=<>!$*/+-]   { return yytext[0]; }

[a-zA-Z_][a-zA-Z0-9_]*  { yylval.sval = strdup(yytext); return IDENTIFIER; }
[0-9]+                  { yylval.ival = atoi(yytext); return LITERAL_INT; }
[0-9]+\.[0-9]+          { yylval.fval = atof(yytext); return LITERAL_FLOAT; }
\"[^"\n\r]*\"           { yylval.sval = strdup(yytext); return LITERAL_STR; }

%%
