%{  
    #include "ekcc.h"
    #include <iostream>
    using std::cout;
    using std::endl; 

    extern "C" int yylex();
%}
%option noyywrap
%%
[ \t\n]+     { /* Ignore white space. */ }
[#].*\n      { /* Ignore comments. */ }
"+"          { cout << "PLUS " << yytext << endl; }
"-"          { cout << "MINUS " << yytext << endl; }
"*"          { cout << "TIMES " << yytext << endl; }
"/"          { cout << "SLASH " << yytext << endl; }
"=="         { cout << "EQUAL " << yytext << endl; }
"<"          { cout << "LESS " << yytext << endl; }
">"          { cout << "GREATER " << yytext << endl; }
"&&"         { cout << "AND " << yytext << endl; }
"||"         { cout << "OR " << yytext << endl; }
"!"          { cout << "NOT " << yytext << endl; }
"="          { cout << "ASSIGN " << yytext << endl; }
","          { cout << "COMMA " << yytext << endl; }
";"          { cout << "SEMICOLON " << yytext << endl; }
"("          { cout << "L_PAREN " << yytext << endl; }
")"          { cout << "R_PAREN " << yytext << endl; }
"["          { cout << "L_BRACKET " << yytext << endl; }
"]"          { cout << "R_BRACKET " << yytext << endl; }
"{"          { cout << "L_BRACE " << yytext << endl; }
"}"          { cout << "R_BRACE " << yytext << endl; }
"$"          { cout << "DOLLAR " << yytext << endl; }
extern       { cout << "EXTERN " << yytext << endl; }
def          { cout << "DEF " << yytext << endl; }
return       { cout << "RETURN " << yytext << endl; }
while        { cout << "WHILE " << yytext << endl; }
if           { cout << "IF " << yytext << endl; }
else         { cout << "ELSE " << yytext << endl; }
print        { cout << "PRINT " << yytext << endl; }
true         { cout << "TRUE " << yytext << endl; }
false        { cout << "FALSE " << yytext << endl; }
int          { cout << "INT " << yytext << endl; }
cint         { cout << "CINT " << yytext << endl; }
float        { cout << "FLOAT " << yytext << endl; }
bool         { cout << "BOOL " << yytext << endl; }
void         { cout << "VOID " << yytext << endl; }
noalias      { cout << "NOALIAS " << yytext << endl; }
ref          { cout << "REF " << yytext << endl; }

[a-zA-Z_][a-zA-Z0-9_]*  { cout << "IDENTIFIER " << yytext << endl; }
[0-9]+                  { cout << "LITERAL_INT " << yytext << endl; }
[0-9]+\.[0-9]+          { cout << "LITERAL_FLOAT " << yytext << endl; }
\"[^"\n\r]*\"           { cout << "LITERAL_STR " << yytext << endl; }

%%

int main() {
    yylex();
    return 0;
}
