#ifndef _AST_H_
#define _AST_H_

#include <vector>
#include <unordered_map>
#include <iostream>

using namespace std;

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
        t_void = 0,
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
    // Easy-to-find bugs
    //exp(type *t): exp_type(t) {}

    // Check whether this expression is a variable (lvalue) or not (rvalue). 
    // Only when exp is a struct varval, this function returns true. 
    virtual bool is_variable() { return false; }
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

    lit(int i);

    virtual void yaml(ostream &os, string prefix);
};

struct flit : public exp {
    float flt;

    flit(float f);

    virtual void yaml(ostream &os, string prefix);
};

struct varval : public exp {
    id *variable;

    varval(id *v);

    bool is_variable() { return true; }

    virtual void yaml(ostream &os, string prefix);

    ~varval() { delete variable; }
};

struct assign : public exp {
    id *variable;
    exp *expression;

    assign(id *v, exp *e);

    virtual void yaml(ostream &os, string prefix);

    ~assign() { delete variable; delete expression; }
};

struct funccall : public exp {
    id *globid;
    exps *params; 

    funccall(id *gid, exps *p = 0);

    virtual void yaml(ostream &os, string prefix);

    ~funccall() { delete globid; delete params; }
};

struct uop : public exp {
    enum uop_kind {
        uop_not,
        uop_minus,
    } kind;

    exp *expression;

    uop(uop_kind kd, exp *e);

    string kind_name() {
        switch (kind) {
            case uop_not   : return "not"  ;
            case uop_minus : return "minus";
        };
    }

    virtual void yaml(ostream &os, string prefix);

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

    binop(binop_kind kd, exp *left, exp *right);

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

    ~binop() { delete lhs; delete rhs; }
};

struct castexp : public exp {
    type *tp;
    exp *expression;

    castexp(type *t, exp *e);

    virtual void yaml(ostream &os, string prefix);

    ~castexp() { delete tp; delete expression; }
};

struct stmt : public node {
    // Check whether this statement is a return statement.
    virtual bool is_return() { return false; }
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

    virtual void yaml(ostream &os, string prefix);

    ~blk() { delete statements; }
};

struct ret : public stmt {
    exp *expression;

    ret(exp *e = 0);
    bool is_return() { return true; }

    virtual void yaml(ostream &os, string prefix);

    ~ret() { delete expression; }
};

struct vdeclstmt : public stmt {
    vdecl *variable;
    exp *expression;

    vdeclstmt(vdecl *v, exp *e);

    virtual void yaml(ostream &os, string prefix);

    ~vdeclstmt() { delete variable; delete expression; }
};

struct expstmt : public stmt {
    exp *expression;

    expstmt(exp *e) : expression(e) {}

    virtual void yaml(ostream &os, string prefix);

    ~expstmt() { delete expression; }
};

struct whilestmt : public stmt {
    exp *condition;
    stmt *statement;
    
    whilestmt(exp *c, stmt *s) : condition(c), statement(s) {}

    virtual void yaml(ostream &os, string prefix);

    ~whilestmt() { delete condition; delete statement; }
};

struct ifstmt : public stmt {
    exp *condition;
    stmt *statement;
    stmt *else_statement;

    ifstmt(exp *e, stmt *s, stmt *es = 0) : 
        condition(e), statement(s), else_statement(es) {}

    virtual void yaml(ostream &os, string prefix);

    ~ifstmt() { delete condition; delete statement; delete else_statement; }
};

struct print : public stmt {
    exp *expression;
    
    print(exp *e) : expression(e) {}

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