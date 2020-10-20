/** 
 * Header file of the EKCC compiler. 
 */

#ifndef _EHCC_H_
#define _EHCC_H_

/* Terminal token types. */
enum Token {
    // symbols
    PLUS, MINUS, TIMES, SLASH, EQUAL, LESS, GREATER, AND, OR, NOT, ASSIGN, COMMA, 
    SEMICOLON, L_PAREN, R_PAREN, L_BRACE, R_BRACE, L_BRACKET, R_BRACKET, DOLLAR, 
    // keywords
    EXTERN, DEF, RETURN, WHILE, IF, ELSE, PRINT, TRUE, FALSE, INT, CINT, FLOAT, 
    BOOL, VOID, NOALIAS, REF, 
    // other token types
    IDENTIFIER, LITERAL_INT, LITERAL_FLOAT, LITERAL_STR
};




#endif /* _EHCC_H_ */