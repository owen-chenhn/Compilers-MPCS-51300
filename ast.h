#ifndef _AST_H_
#define _AST_H_

#include <vector>
#include <unordered_map>
#include <iostream>

using namespace std;

struct node {
    static unordered_map<string, func*> function_table;    // Table of all the declared functions.
    static unordered_map<string, ext*> extern_table;       // Table of all the external functions. 

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
};

// type of varid and globid
struct id {
    string identifier;

    id(char *ident) : identifier(ident) {}
};

struct vdecl : public node {
    type *tp; 
    id *variable;

    vdecl(type *t, id *var) : tp(t), variable(var) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "node: vdecl" << endl;
        os << prefix << "type: " << tp->name() << endl;
        os << prefix << "var: " << variable->identifier << endl;
    }

    ~vdecl() { delete tp; delete variable; }
};

struct tdecls : public node {
    vector<type *> types;

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: tdecls" << endl;
        os << prefix << "types: " << endl;
        for (auto t : types) {
            os << prefix << "  - " << t->name() << endl;
        }
    }

    ~tdecls() {
        for (auto t : types) { delete t; }
    }
};

struct vdecls : public node {
    vector<vdecl *> variables;

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: vdecls" << endl;
        os << prefix << "vars:" << endl;
        for (auto var : variables) {
            os << prefix << "  -" << endl;
            var->yaml(os, prefix + "    ");
        }
    }

    ~vdecls() {
        for (auto var : variables) { delete var; }
    }
};

struct exp : public node {};

struct exps : public node {
    vector<exp *> expressions;

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: exps" << endl;
        os << prefix << "exps:" << endl;
        for (auto e : expressions) {
            os << prefix << "  -" << endl;
            e->yaml(os, prefix + "    ");
        }
    }

    ~exps() {
        for (auto exp : expressions) { delete exp; }
    }
};

struct lit : public exp {
    int it;

    lit(int i) : it(i) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: lit" << endl;
        os << prefix << "value: " << it << endl;
    }
};

struct flit : public exp {
    float flt;

    flit(float f) : flt(f) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: flit" << endl;
        os << prefix << "value: " << flt << endl;
    }
};

struct varval : public exp {
    id *variable;

    varval(id *v) : variable(v) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: varval" << endl;
        os << prefix << "var: " << variable->identifier << endl;
    }

    ~varval() { delete variable; }
};

struct assign : public exp {
    id *variable;
    exp *expression;

    assign(id *v, exp *e) : variable(v), expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: assign" << endl;
        os << prefix << "var: " << variable->identifier << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }

    ~assign() { delete variable; delete expression; }
};

struct funccall : public exp {
    id *globid;
    exps *params; 

    funccall(id *gid, exps *p = 0) : globid(gid), params(p) {
        if (function_table.count(globid->identifier)) {
            func *f = function_table[globid->identifier];
            
        }
        else if (extern_table.count(globid->identifier)) {
            ext *e = extern_table[globid->identifier];
        }
        else error("undeclared function '" + globid->identifier + "' is called.");
    }
    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: funccall" << endl;
        os << prefix << "globid: " << globid->identifier << endl;
        if (!params) return;
        os << prefix << "params:" << endl;
        params->yaml(os, prefix + "  ");
    }

    ~funccall() { delete globid; delete params; }
};

struct uop : public exp {
    enum uop_kind {
        uop_not,
        uop_minus,
    } kind;

    exp *expression;

    uop(uop_kind kd, exp *e) : kind(kd), expression(e) {}

    const char *kind_name() {
        switch (kind) {
            case uop_not   : return "not"  ;
            case uop_minus : return "minus";
        };
    }

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: uop" << endl;
        os << prefix << "op: " << kind_name() << endl; 
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }

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

    binop(binop_kind kd, exp *left, exp *right) : kind(kd), lhs(left), rhs(right) {}

    const char *kind_name() {
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

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: binop" << endl;
        os << prefix << "op: " << kind_name() << endl;
        os << prefix << "lhs:" << endl;
        lhs->yaml(os, prefix + "  ");
        os << prefix << "rhs:" << endl;
        rhs->yaml(os, prefix + "  ");
    }

    ~binop() { delete lhs; delete rhs; }
};

struct castexp : public exp {
    type *tp;
    exp *expression;

    castexp(type *t, exp *e) : tp(t), expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: caststmt" << endl;
        os << prefix << "type: " << tp->name() << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }

    ~castexp() { delete tp; delete expression; }
};

struct stmt : public node {}; 

struct stmts : public node {
    vector<stmt *> statements;

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: stmts" << endl;
        os << prefix << "stmts:" << endl;
        for (auto s : statements) {
            os << prefix << "  -" << endl;
            s->yaml(os, prefix + "    ");
        }
    }

    ~stmts() {
        for (auto stmt : statements) {
            delete stmt;
        }
    }
};

struct blk : public stmt {
    stmts *statements;

    blk(stmts *ss = 0) : statements(ss) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: blk" << endl;
        if (!statements) return;
        os << prefix << "contents:" << endl;
        statements->yaml(os, prefix + "  ");
    }

    ~blk() { delete statements; }
};

struct ret : public stmt {
    exp *expression;

    ret(exp *e = 0) : expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: ret" << endl;
        if (!expression) return;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }

    ~ret() { delete expression; }
};

struct vdeclstmt : public stmt {
    vdecl *variable;
    exp *expression;

    vdeclstmt(vdecl *v, exp *e) : variable(v), expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: vardeclstmt" << endl;
        os << prefix << "vdecl:" << endl;
        variable->yaml(os, prefix + "  ");
        os << prefix << "exp: " << endl;
        expression->yaml(os, prefix + "  ");
    }

    ~vdeclstmt() { delete variable; delete expression; }
};

struct expstmt : public stmt {
    exp *expression;

    expstmt(exp *e) : expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: expstmt" << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }

    ~expstmt() { delete expression; }
};

struct whilestmt : public stmt {
    exp *condition;
    stmt *statement;
    
    whilestmt(exp *c, stmt *s) : condition(c), statement(s) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: while" << endl;
        os << prefix << "cond: " << endl;
        condition->yaml(os, prefix + "  ");
        os << prefix << "stmt: " << endl;
        statement->yaml(os, prefix + "  ");
    }

    ~whilestmt() { delete condition; delete statement; }
};

struct ifstmt : public stmt {
    exp *condition;
    stmt *statement;
    stmt *else_statement;

    ifstmt(exp *e, stmt *s, stmt *es = 0) : 
        condition(e), statement(s), else_statement(es) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: if" << endl;
        os << prefix << "cond:" << endl;
        condition->yaml(os, prefix + "  ");
        os << prefix << "stmt:" << endl;
        statement->yaml(os, prefix + "  ");
        if (!else_statement) return;
        os << prefix << "else_stmt:" << endl;
        else_statement->yaml(os, prefix + "  ");
    }

    ~ifstmt() { delete condition; delete statement; delete else_statement; }
};

struct print : public stmt {
    exp *expression;
    
    print(exp *e) : expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: print" << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }

    ~print() { delete expression; }
};

struct printslit : public stmt {
    string str;

    printslit(string s) : str(s) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: printslit" << endl;
        os << prefix << "string: " << str << endl;
    }
};

struct func : public node {
    type *rt;
    id *globid;
    blk *block;
    vdecls *variable_declaration;

    func(type *r, id *g, blk *b, vdecls *v = 0) : 
        rt(r), globid(g), block(b), variable_declaration(v) 
    {
        if (extern_table.count(globid->identifier) || 
            function_table.count(globid->identifier))
            error("duplicate declaration of function '" + globid->identifier + "'.");
        if (r->ref) error("function return type is a reference.");
        function_table[globid->identifier] = this;
    }

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: func" << endl;
        os << prefix << "ret_type: " << rt->name() << endl;
        os << prefix << "globid: " << globid->identifier << endl;
        os << prefix << "blk:" << endl;
        block->yaml(os, prefix + "  ");
        if (!variable_declaration) return;
        os << prefix << "vdecls:" << endl;
        variable_declaration->yaml(os, prefix + "  ");
    }

    ~func() { delete rt; delete globid; delete block; delete variable_declaration; }
};

struct funcs : public node {
    vector<func *> functions;

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: funcs" << endl;
        os << prefix << "funcs:" << endl;
        for (auto fun : functions) {
            os << prefix << "  -" << endl;
            fun->yaml(os, prefix + "    ");
        }
    }

    ~funcs() {
        for (auto f : functions) { delete f; }
    }
};

struct ext : public node {
    type *rt;
    id *globid;
    tdecls *type_declares;

    ext(type *r, id *g, tdecls *t = 0) : rt(r), globid(g), type_declares(t) {
        if (globid->identifier == "run") error("function 'run' cannot be external.");
        if (extern_table.count(globid->identifier)) error("duplicate declaration of function '" + globid->identifier + "'.");
        if (r->ref) error("function return type is a reference.");
        extern_table[globid->identifier] = this;
    }

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: extern" << endl;
        os << prefix << "ret_type: " << rt->name() << endl;
        os << prefix << "globid: " << globid->identifier << endl;
        if (!rt) return;
        os << prefix << "tdecls:" << endl;
        type_declares->yaml(os, prefix + "  ");
    } 

    ~ext() { delete rt; delete globid; delete type_declares; }
};

struct exts : public node {
    vector<ext *> externs;

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: externs" << endl;
        os << prefix << "externs: " << endl;
        for (auto e : externs) {
            os << prefix << "  -" << endl;
            e->yaml(os, prefix + "    ");
        }
    }

    ~exts() {
        for (auto ext : externs) { delete ext; }
    }
};

struct prog : public node {
    funcs *functions;
    exts *e;

    prog(funcs *f, exts *e = 0) : functions(f), e(e) {
        // Check there is a function named "run"
        if (function_table.count("run") == 0) error("function 'run' not found.");
    }

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: prog" << endl;
        os << prefix << "funcs:" << endl;
        functions->yaml(os, prefix + "  ");
        if (!e) return;
        os << prefix << "externs: " << endl;
        e->yaml(os, prefix + "  ");
    }

    ~prog() {delete functions; delete e; }
};

#endif /* _AST_H_ */