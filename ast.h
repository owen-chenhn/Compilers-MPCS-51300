#ifndef _AST_H_
#define _AST_H_

#include <vector>
#include <unordered_map>
#include <iostream>
#include <cassert>

#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

using namespace std;
using namespace llvm;

struct node {
    virtual void yaml(ostream &os, string prefix) = 0;
    virtual ~node() {}
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

    string name();

    void check_and_make_ref();
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
    AllocaInst *code_gen();

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


struct expr : public node {
    type *exp_type;

    expr(type *t): exp_type(t) {}

    // Check whether this expression is a variable (lvalue) or not (rvalue). 
    // Only when exp is a struct varval, this function returns true. 
    virtual bool is_variable() { return false; }

    // Check and determine the type of this expression. 
    virtual void check_type() { assert (exp_type != nullptr); }

    // Generate llvm-IR code
    virtual Value *code_gen() = 0;
};

struct exps : public node {
    vector<expr *> expressions;

    virtual void yaml(ostream &os, string prefix);

    ~exps() {
        for (auto e : expressions) { delete e; }
    }
};

struct lit : public expr {
    int it;

    lit(int i, bool bool_flag): expr(new type(bool_flag ? type::t_bool : type::t_int)), it(i) {}

    virtual void yaml(ostream &os, string prefix);

    string get_str();
    Value* code_gen();
};

struct flit : public expr {
    float flt;

    flit(float f): expr(new type(type::t_float)), flt(f) {}

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();
};

struct varval : public expr {
    id *variable;

    varval(id *v);

    bool is_variable() { return true; }

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();
    AllocaInst* get_var_pointer();

    ~varval() { delete variable; }
};

struct assign : public expr {
    varval *variable;
    expr *expression;

    assign(varval *v, expr *e): expr(v->exp_type), variable(v), expression(e) {}
    void check_type();
    Value *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~assign() { delete variable; delete expression; }
};

struct funccall : public expr {
    id *globid;
    exps *params; 

    funccall(id *gid, exps *p = 0): expr(nullptr), globid(gid), params(p) {
        if (!p) params = new exps();
    }

    void check_type();

    virtual void yaml(ostream &os, string prefix);

    Value* code_gen();

    ~funccall() { delete globid; delete params; }
};

struct uop : public expr {
    enum uop_kind {
        uop_not,
        uop_minus,
    } kind;

    expr *expression;

    uop(uop_kind kd, expr *e): expr(nullptr), kind(kd), expression(e) {}
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

struct binop : public expr {
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

    expr *lhs, *rhs; 

    binop(binop_kind kd, expr *left, expr *right): expr(nullptr), kind(kd), lhs(left), rhs(right) {}
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

struct castexp : public expr {
    type *tp;
    expr *expression;

    castexp(type *t, expr *e): expr(t), tp(t), expression(e) {}
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

    blk(stmts *ss = 0) : statements(ss) { if (!statements) statements = new stmts(); }

    void check_exp() {
        for (stmt *st : statements->statements) st->check_exp();
    }

    Value *code_gen() {
        Value *ret = nullptr;
        for (stmt *st : statements->statements) {
            ret = st->code_gen();
        }
        return ret;
    }

    virtual void yaml(ostream &os, string prefix);

    ~blk() { delete statements; }
};

struct ret : public stmt {
    expr *expression;

    ret(expr *e = 0): expression(e) {}
    bool is_return() { return true; }
    void check_exp() { if (expression) expression->check_type(); }
    Value *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~ret() { delete expression; }
};

struct vdeclstmt : public stmt {
    vdecl *variable;
    expr *expression;

    vdeclstmt(vdecl *v, expr *e);
    void check_exp();
    Value *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~vdeclstmt() { delete variable; delete expression; }
};

struct expstmt : public stmt {
    expr *expression;

    expstmt(expr *e) : expression(e) {}
    void check_exp() { expression->check_type(); }
    Value *code_gen() { return expression->code_gen(); }

    virtual void yaml(ostream &os, string prefix);

    ~expstmt() { delete expression; }
};

struct whilestmt : public stmt {
    expr *condition;
    stmt *statement;
    
    whilestmt(expr *c, stmt *s) : condition(c), statement(s) {}
    void check_exp();
    Value *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~whilestmt() { delete condition; delete statement; }
};

struct ifstmt : public stmt {
    expr *condition;
    stmt *statement;
    stmt *else_statement;

    ifstmt(expr *e, stmt *s, stmt *es = 0) : 
        condition(e), statement(s), else_statement(es) {}

    void check_exp();
    Value *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~ifstmt() { delete condition; delete statement; delete else_statement; }
};

struct print : public stmt {
    expr *expression;
    
    print(expr *e) : expression(e) {}
    void check_exp() { expression->check_type(); }
    Value *code_gen();

    virtual void yaml(ostream &os, string prefix);

    ~print() { delete expression; }
};

struct printslit : public stmt {
    string str;

    printslit(string s) : str(s.substr(1,s.size() - 2)) {}
    Value *code_gen();

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
    exts *externs;

    prog(funcs *f, exts *e = 0);
    Module *code_gen();
    void jit(int argc, char** argv);

    virtual void yaml(ostream &os, string prefix);

    ~prog() { delete functions; delete externs; }
};

#endif /* _AST_H_ */