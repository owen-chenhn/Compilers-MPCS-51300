#ifndef _AST_H_
#define _AST_H_

#include <vector>
#include <unordered_map>
#include <iostream>
#include <cassert>

#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"

using namespace std;
using namespace llvm;

struct node {
    virtual void yaml(ostream &os, string prefix) = 0;
    virtual ~node() {}
    virtual void error(string err_msg) {
        cout << "error: " << err_msg << endl;
        exit(1);
    }
};

struct type {
    enum type_kind {
        t_void,
        t_bool,
        t_int,
        t_cint,
        t_float,
    } kind;
    bool ref;
    bool noalias; // applies only to reference. 

    type(type_kind kd) : kind(kd), ref(false), noalias(false) {}

    string name() {
        string nm;
        if (noalias) nm += "noalias ";
        if (ref) nm += "ref ";

        switch (kind) {
            case t_void : nm += "void" ;break; 
            case t_bool : nm += "bool" ;break;   
            case t_int  : nm += "int"  ;break;
            case t_cint : nm += "cint" ;break; 
            case t_float: nm += "float";break;
        }

        return nm;
    }

    void check_and_make_ref();

    void error(const string& err_msg);
};

// type of varid and globid
struct id {
    string identifier;

    id(char *ident) : identifier(ident) {}
};

struct vdecl : public node {
    type *tp; 
    id *variable;

    vdecl(type *t, id *var);
    string getName() { return variable->identifier; }
    type::type_kind getTypeKind() { return tp->kind; }

    virtual void yaml(ostream &os, string prefix);

    ~vdecl() { delete tp; delete variable; }
};

struct tdecls : public node {
    vector<type *> types;

    virtual void yaml(ostream &os, string prefix);

    ~tdecls() {
        for (auto t : types) { delete t; }
    }
};

struct vdecls : public node {
    vector<vdecl *> variables;

    virtual void yaml(ostream &os, string prefix);

    ~vdecls() {
        for (auto var : variables) { delete var; }
    }
};


struct exp : public node {
    type *exp_type;

    exp(type *t): exp_type(t) {}

    // Check whether this expression is a variable (lvalue) or not (rvalue). 
    // Only when exp is a struct varval, this function returns true. 
    virtual bool is_variable() { return false; }

    // Check and determine the type of this expression. 
    virtual void check_type() { assert (exp_type != nullptr); }

    // Generate llvm-IR code
    virtual Value *code_gen() = 0;
};

struct exps : public node {
    vector<exp *> expressions;

    virtual void yaml(ostream &os, string prefix);

    ~exps() {
        for (auto exp : expressions) { delete exp; }
    }
};

struct lit : public exp {
    int it;

    lit(int i): exp(new type(type::t_int)), it(i)  {}

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();
};

struct flit : public exp {
    float flt;

    flit(float f): exp(new type(type::t_float)), flt(f) {}

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();
};

struct varval : public exp {
    id *variable;

    varval(id *v);

    bool is_variable() { return true; }

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();

    ~varval() { delete variable; }
};

struct assign : public exp {
    varval *variable;
    exp *expression;

    assign(varval *v, exp *e): exp(v->exp_type), variable(v), expression(e) {}
    void check_type();

    virtual void yaml(ostream &os, string prefix);

    ~assign() { delete variable; delete expression; }
};

struct funccall : public exp {
    id *globid;
    exps *params; 

    funccall(id *gid, exps *p = 0): exp(nullptr), globid(gid), params(p) {}
    void check_type();

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();

    ~funccall() { delete globid; delete params; }
};

struct uop : public exp {
    enum uop_kind {
        uop_not,
        uop_minus,
    } kind;

    exp *expression;

    uop(uop_kind kd, exp *e): exp(nullptr), kind(kd), expression(e) {}
    void check_type();

    string kind_name() {
        switch (kind) {
            case uop_not   : return "not"  ;
            case uop_minus : return "minus";
        };
    }

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();

    ~uop() { delete expression; }
};

struct binop : public exp {
    enum binop_kind {
        bop_mul,
        bop_div,
        bop_add,
        bop_sub,
        bop_eq,
        bop_lt,
        bop_gt,
        bop_and,
        bop_or
    } kind;

    exp *lhs, *rhs; 

    binop(binop_kind kd, exp *left, exp *right): exp(nullptr), kind(kd), lhs(left), rhs(right) {}
    void check_type();

    string kind_name() {
        switch (kind) {
            case bop_mul : return "mul";
            case bop_div : return "div";
            case bop_add : return "add";
            case bop_sub : return "sub";            
            case bop_eq  : return "eq" ;         
            case bop_lt  : return "lt" ;
            case bop_gt  : return "gt" ;
            case bop_and : return "and";
            case bop_or  : return "or" ;                 
        };
    }

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();

    ~binop() { delete lhs; delete rhs; }
};

struct castexp : public exp {
    type *tp;
    exp *expression;

    castexp(type *t, exp *e): exp(t), tp(t), expression(e) {}
    void check_type();

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();

    ~castexp() { delete tp; delete expression; }
};

struct stmt : public node {
    // Check whether this statement is a return statement.
    virtual bool is_return() { return false; }
    // Check types of the statement's all expressions.
    virtual void check_exp() {}
    //Generate code for the statement. 
    virtual Value *code_gen() = 0;
}; 

struct stmts : public node {
    vector<stmt *> statements;

    virtual void yaml(ostream &os, string prefix);

    ~stmts() {
        for (auto stmt : statements) {
            delete stmt;
        }
    }
};

struct blk : public stmt {
    stmts *statements;

    blk(stmts *ss = 0) : statements(ss) {}
    void check_exp() {
        if (statements) {
            for (stmt *st : statements->statements) st->check_exp();
        }
    }

    Value *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~blk() { delete statements; }
};

struct ret : public stmt {
    exp *expression;

    ret(exp *e = 0): expression(e) {}
    bool is_return() { return true; }
    void check_exp() { if (expression) expression->check_type(); }

    virtual void yaml(ostream &os, string prefix);

    ~ret() { delete expression; }
};

struct vdeclstmt : public stmt {
    vdecl *variable;
    exp *expression;

    vdeclstmt(vdecl *v, exp *e);
    void check_exp();

    virtual void yaml(ostream &os, string prefix);

    ~vdeclstmt() { delete variable; delete expression; }
};

struct expstmt : public stmt {
    exp *expression;

    expstmt(exp *e) : expression(e) {}
    void check_exp() { expression->check_type(); }

    virtual void yaml(ostream &os, string prefix);

    ~expstmt() { delete expression; }
};

struct whilestmt : public stmt {
    exp *condition;
    stmt *statement;
    
    whilestmt(exp *c, stmt *s) : condition(c), statement(s) {}
    void check_exp() { condition->check_type(); statement->check_exp(); }

    virtual void yaml(ostream &os, string prefix);

    ~whilestmt() { delete condition; delete statement; }
};

struct ifstmt : public stmt {
    exp *condition;
    stmt *statement;
    stmt *else_statement;

    ifstmt(exp *e, stmt *s, stmt *es = 0) : 
        condition(e), statement(s), else_statement(es) {}

    void check_exp() { 
        condition->check_type(); 
        statement->check_exp(); 
        if (else_statement) else_statement->check_exp(); 
    }

    virtual void yaml(ostream &os, string prefix);

    ~ifstmt() { delete condition; delete statement; delete else_statement; }
};

struct print : public stmt {
    exp *expression;
    
    print(exp *e) : expression(e) {}
    void check_exp() { expression->check_type(); }

    virtual void yaml(ostream &os, string prefix);

    ~print() { delete expression; }
};

struct printslit : public stmt {
    string str;

    printslit(string s) : str(s) {}

    virtual void yaml(ostream &os, string prefix);
};

struct func : public node {
    type *rt;
    id *globid;
    blk *block;
    vdecls *variable_declarations;

    func(type *r, id *g, blk *b, vdecls *v = 0);
    Function *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~func() { delete rt; delete globid; delete block; delete variable_declarations; }
};

struct funcs : public node {
    vector<func *> functions;

    virtual void yaml(ostream &os, string prefix);
    
    ~funcs() {
        for (auto f : functions) { delete f; }
    }
};

struct ext : public node {
    type *rt;
    id *globid;
    tdecls *type_declarations;

    ext(type *r, id *g, tdecls *t = 0);
    Function *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~ext() { delete rt; delete globid; delete type_declarations; }
};

struct exts : public node {
    vector<ext *> externs;

    virtual void yaml(ostream &os, string prefix);

    ~exts() {
        for (auto ext : externs) { delete ext; }
    }
};

struct prog : public node {
    funcs *functions;
    exts *e;

    prog(funcs *f, exts *e = 0);

    virtual void yaml(ostream &os, string prefix);

    ~prog() { delete functions; delete e; }
};

#endif /* _AST_H_ */