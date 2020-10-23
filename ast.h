#ifndef AST_H

#include <vector>
#include <iostream>

using namespace std;

struct node {
    virtual void yaml(ostream &os, string prefix) = 0;
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

struct vdecl : public node {
    type *tp; 
    string variable;

    vdecl(type *t, string var) : tp(t), variable(var) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "node: vdecl" << endl;
        os << prefix << "type: " << tp->name() << endl;
        os << prefix << "var: " << variable << endl;
    }
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
};

struct exp : public node {};

struct exps : public node {
    vector<struct exp *> expressions;

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: exps" << endl;
        os << prefix << "exps:" << endl;
        for (auto e : expressions) {
            os << prefix << "  -" << endl;
            e->yaml(os, prefix + "    ");
        }
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
    string variable;

    varval(string v) : variable(v) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: varval" << endl;
        os << prefix << "var: " << variable << endl;
    }
};

struct assign : public exp {
    string variable;
    exp *expression;

    assign(string v, exp *e) : variable(v), expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: assign" << endl;
        os << prefix << "var: " << variable << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }
};

struct funccall : public exp {
    string globid;
    exps *params; 

    funccall(string gid, exps *p = 0) : globid(gid), params(p) {}
    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: funccall" << endl;
        os << prefix << "globid: " << globid << endl;
        if (!params) return;
        os << prefix << "params:" << endl;
        params->yaml(os, prefix + "  ");
    }
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
};

struct ret : public stmt {
    struct exp * expression;

    ret(struct exp *e = 0) : expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: ret" << endl;
        if (!expression) return;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }
};

struct vdeclstmt : public stmt {
    vdecl *variable;
    struct exp *expression;

    vdeclstmt(vdecl *v, struct exp *e) : variable(v), expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: vardeclstmt" << endl;
        os << prefix << "vdecl:" << endl;
        variable->yaml(os, prefix + "  ");
        os << prefix << "exp: " << endl;
        expression->yaml(os, prefix + "  ");
    }
};

struct expstmt : public stmt {
    struct exp *expression;

    expstmt(struct exp *e) : expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: expstmt" << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }
};

struct whilestmt : public stmt {
    struct exp *condition;
    stmt *statement;
    
    whilestmt(struct exp *c, stmt *s) : condition(c), statement(s) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: while" << endl;
        os << prefix << "cond: " << endl;
        condition->yaml(os, prefix + "  ");
        os << prefix << "stmt: " << endl;
        statement->yaml(os, prefix + "  ");
    }
};

struct ifstmt : public stmt {
    struct exp *condition;
    stmt *statement;
    stmt *else_statement;

    ifstmt(struct exp *e, stmt *s, stmt *es = 0) : 
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
};

struct print : public stmt {
    struct exp *expression;
    
    print(struct exp *e) : expression(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: print" << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
    }
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
    string globid;
    blk *block;
    vdecls *variable_declaration;

    func(type *r, string g, blk *b, vdecls *v = 0) : 
        rt(r), globid(g), block(b), variable_declaration(v) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: func" << endl;
        os << prefix << "ret_type: " << rt->name() << endl;
        os << prefix << "globid: " << globid << endl;
        os << prefix << "blk:" << endl;
        block->yaml(os, prefix + "  ");
        if (!variable_declaration) return;
        os << prefix << "vdecls:" << endl;
        variable_declaration->yaml(os, prefix + "  ");
    }
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
};

struct ext : public node {
    struct type *rt;
    string globid;
    tdecls *type;

    ext(struct type *r, string g, tdecls *t = 0) : rt(r), globid(g), type(t) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: extern" << endl;
        os << prefix << "ret_type: " << rt->name() << endl;
        os << prefix << "globid: " << globid << endl;
        if (!rt) return;
        os << prefix << "tdecls:" << endl;
        type->yaml(os, prefix + "  ");
    } 
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
};

struct prog : public node {
    funcs *functions;
    exts *e;

    prog(funcs *f, exts *e = 0) : functions(f), e(e) {}

    virtual void yaml(ostream &os, string prefix) {
        os << prefix << "name: prog" << endl;
        os << prefix << "funcs:" << endl;
        functions->yaml(os, prefix + "  ");
        if (!e) return;
        os << prefix << "externs" << endl;
        e->yaml(os, prefix + "  ");
    }
};

#endif AST_H