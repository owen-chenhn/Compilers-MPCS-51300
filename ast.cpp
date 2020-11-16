#include "ast.h"

#include <vector>
#include <unordered_map>
#include <iostream>

using namespace std;

static unordered_map<string, func *> function_table;    // Table of all the declared functions.
static unordered_map<string, ext *> extern_table;       // Table of all the external functions. make
static unordered_map<string, vdecl *> vdecl_table;      // Table of all declared variables. 

static void error(const string& err_msg) {
    cout << "error: " << err_msg << endl;
    exit(1);
}

string type::name() {
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

void type::check_and_make_ref() {
    if (ref) error("Ref type may not refer to a reference.");
    if (kind == type::t_void) error("Ref type may not refer to void type.");
    ref = true;
}

vdecl::vdecl(type *t, id *var): tp(t), variable(var) {
    if (vdecl_table.count(var->identifier)) error("Duplicate variable declaration: " + var->identifier + ".");  
    if (t->kind == type::t_void) error("Variable type may not be void.");
    vdecl_table[var->identifier] = this;
}

void vdecl::yaml(ostream &os, string prefix) {
        os << prefix << "node: vdecl" << endl;
        os << prefix << "type: " << tp->name() << endl;
        os << prefix << "var: " << variable->identifier << endl;
}

void tdecls::yaml(ostream &os, string prefix) {
        os << prefix << "name: tdecls" << endl;
        os << prefix << "types: " << endl;
        for (auto t : types) {
            os << prefix << "  - " << t->name() << endl;
        }
}

void vdecls::yaml(ostream &os, string prefix) {
        os << prefix << "name: vdecls" << endl;
        os << prefix << "vars:" << endl;
        for (auto var : variables) {
            os << prefix << "  -" << endl;
            var->yaml(os, prefix + "    ");
        }
}

void exps::yaml(ostream &os, string prefix) {
        os << prefix << "name: exps" << endl;
        os << prefix << "exps:" << endl;
        for (auto e : expressions) {
            os << prefix << "  -" << endl;
            e->yaml(os, prefix + "    ");
        }
}

void lit::yaml(ostream &os, string prefix) {
        os << prefix << "name: lit" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "value: " << it << endl;
}

void flit::yaml(ostream &os, string prefix) {
        os << prefix << "name: flit" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "value: " << flt << endl;
}

varval::varval(id *v) : expr(nullptr), variable(v) {
    if (!vdecl_table.count(v->identifier)) error("Variable " + v->identifier + " undeclared.");
    exp_type = vdecl_table[v->identifier]->tp;
}

void varval::yaml(ostream &os, string prefix) {
        os << prefix << "name: varval" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "var: " << variable->identifier << endl;
}

void assign::check_type() {
    expression->check_type();
    if (variable->exp_type->kind != expression->exp_type->kind) {
        error("Types should be the same on both sides of assignment, but got " + 
              variable->exp_type->name() + 
              " and " + 
              expression->exp_type->name() + 
              " instead.");
    }
}

void assign::yaml(ostream &os, string prefix) {
        os << prefix << "name: assign" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "var: " << variable->variable->identifier << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
}

funccall::funccall(id* gid, exps *p) : expr(nullptr), globid(gid), params(p) {
    if (!p) params = new exps();
}

void funccall::check_type() {
    if (params) {
        for (expr *e : params->expressions) e->check_type(); 
    }
    
    bool flag;
    if (function_table.count(globid->identifier)) flag = true;
    else if (extern_table.count(globid->identifier)) flag = false;
    else error("Undeclared function '" + globid->identifier + "' is called.");

    func *f = nullptr;
    ext *e = nullptr;
    if (flag) f = function_table[globid->identifier];
    else e = extern_table[globid->identifier];

    unsigned num_params = params ? params->expressions.size() : 0;
    unsigned num_decls;
    if (flag && f->variable_declarations) num_decls = f->variable_declarations->variables.size();
    else if (!flag && e->type_declarations) num_decls = e->type_declarations->types.size();
    else num_decls = 0;

    if (num_params != num_decls) {
        error("Arguments mismatch when calling function '" + globid->identifier + 
                "'. Expect " + to_string(num_decls) + " arguments but got " + 
                to_string(num_params) + ".");
    }
    for (unsigned i = 0; i < num_decls; i++) {
        expr *ex = params->expressions[i];
        type *exp_tp  = ex->exp_type, 
             *decl_tp = flag ? f->variable_declarations->variables[i]->tp 
                             : e->type_declarations->types[i];
        if (exp_tp->kind != decl_tp->kind) {
            error("Function '" + globid->identifier + 
                  "' got wrong argument type. Argument " + to_string(i+1) + 
                  " should be " + decl_tp->name() + " but got " + exp_tp->name());
        }
        if (decl_tp->ref && !ex->is_variable()) {
            error("Function '" + globid->identifier + 
                  "' expects variable (lvalue) for its reference argument " + 
                  to_string(i+1) + ".");
        }
    }

    exp_type = flag ? f->rt : e->rt;
}

void funccall::yaml(ostream &os, string prefix) {
        os << prefix << "name: funccall" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "globid: " << globid->identifier << endl;
        if (!params) return;
        os << prefix << "params:" << endl;
        params->yaml(os, prefix + "  ");
}

void uop::check_type() {
    expression->check_type();
    if (kind == uop_not && expression->exp_type->kind != type::t_bool) error("Bitwise negation (!) must be applied to bools.");
    if (kind == uop_minus) {
        if (expression->exp_type->kind != type::t_int && 
            expression->exp_type->kind != type::t_cint && 
            expression->exp_type->kind != type::t_float) error("Signed negation (-) must be applied to numeric types.");
    }
    exp_type = expression->exp_type;
}

void uop::yaml(ostream &os, string prefix) {
        os << prefix << "name: uop" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "op: " << kind_name() << endl; 
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
}

void binop::check_type() {
    lhs->check_type();
    rhs->check_type();
    if (lhs->exp_type->kind != rhs->exp_type->kind) {
        error("Types should be the same on both sides of " +
              this->kind_name() + 
              " but got " + 
              lhs->exp_type->name() + 
              " and " + 
              rhs->exp_type->name() + 
              " instead.");
    }

    switch (kind) {
    case bop_add:
    case bop_sub:
    case bop_mul:
    case bop_div: exp_type = lhs->exp_type; break;
    default: exp_type = new type(type::t_bool);
    }
}

void binop::yaml(ostream &os, string prefix) {
        os << prefix << "name: binop" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "op: " << kind_name() << endl;
        os << prefix << "lhs:" << endl;
        lhs->yaml(os, prefix + "  ");
        os << prefix << "rhs:" << endl;
        rhs->yaml(os, prefix + "  ");
}

void castexp::check_type() {
    expression->check_type();
    switch(expression->exp_type->kind) {
        case type::t_int : 
        case type::t_cint :
        case type::t_float :
            if (tp->kind == type::t_bool || tp->kind == type::t_void ) 
                error(expression->exp_type->name() + 
                " cannot be casted to " + 
                expression->exp_type->name());
            break;
        case type::t_bool :
            if (tp->kind != type::t_bool) error("Bool should be cast to only bool");
            break;
        default: 
            error("Cannot cast void type.");
    };
}

void castexp::yaml(ostream &os, string prefix) {
        os << prefix << "name: caststmt" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "type: " << tp->name() << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
}

void stmts::yaml(ostream &os, string prefix) {
        os << prefix << "name: stmts" << endl;       
        os << prefix << "stmts:" << endl;
        for (auto s : statements) {
            os << prefix << "  -" << endl;
            s->yaml(os, prefix + "    ");
        }
}

void blk::yaml(ostream &os, string prefix) {
        os << prefix << "name: blk" << endl;       
        if (!statements) return;
        os << prefix << "contents:" << endl;
        statements->yaml(os, prefix + "  ");
}

void ret::yaml(ostream &os, string prefix) {
        os << prefix << "name: ret" << endl;
        if (!expression) return;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
}

vdeclstmt::vdeclstmt(vdecl *v, expr *e) : variable(v), expression(e) {
    if (v->tp->ref && !e->is_variable()) {
        //if (!is_same<varval, decltype(*e)>::value) 
        error("Ref variable initilization expression must be a variable.");
    }
}

void vdeclstmt::check_exp() {
    expression->check_type();
    if (variable->tp->kind != expression->exp_type->kind) {
        error("Variable " + variable->getName() + " is initialized with wrong type. Expect " + 
              variable->tp->name() + " but got " + expression->exp_type->name());
    }
}

void vdeclstmt::yaml(ostream &os, string prefix) {
        os << prefix << "name: vardeclstmt" << endl;
        os << prefix << "vdecl:" << endl;
        variable->yaml(os, prefix + "  ");
        os << prefix << "exp: " << endl;
        expression->yaml(os, prefix + "  ");
}   

void expstmt::yaml(ostream &os, string prefix) {
        os << prefix << "name: expstmt" << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
}

void whilestmt::yaml(ostream &os, string prefix) {
        os << prefix << "name: while" << endl;
        os << prefix << "cond: " << endl;
        condition->yaml(os, prefix + "  ");
        os << prefix << "stmt: " << endl;
        statement->yaml(os, prefix + "  ");
}

void ifstmt::yaml(ostream &os, string prefix) {
        os << prefix << "name: if" << endl;
        os << prefix << "cond:" << endl;
        condition->yaml(os, prefix + "  ");
        os << prefix << "stmt:" << endl;
        statement->yaml(os, prefix + "  ");
        if (!else_statement) return;
        os << prefix << "else_stmt:" << endl;
        else_statement->yaml(os, prefix + "  ");
}

void print::yaml(ostream &os, string prefix) {
        os << prefix << "name: print" << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
}

void printslit::yaml(ostream &os, string prefix) {
        os << prefix << "name: printslit" << endl;
        os << prefix << "string: " << str << endl;
}

func::func(type *r, id *g, blk *b, vdecls *v) : 
    rt(r), globid(g), block(b), variable_declarations(v) 
{
    if (extern_table.count(globid->identifier) || 
        function_table.count(globid->identifier))
        error("Duplicate declaration of function '" + globid->identifier + "'.");
    if (rt->ref) error("Function should not return a reference.");
    if (globid->identifier == "run") {
        // Funtion "run" must return int/cint and take no arguments.
        if (rt->kind != type::t_int && rt->kind != type::t_cint) 
            error("Funtion 'run' must have return type int or cint.");
        if (v != NULL) error("Funtion 'run' cannot have arguments.");
    }
    if (block->statements == NULL && rt->kind != type::t_void) 
        error("Funtion '" + globid->identifier + "' has empty body but non-void return type.");

    function_table[globid->identifier] = this;

    // Check function block and return type
    if (block->statements) {
        for (stmt *st : block->statements->statements) {
            st->check_exp();
            if (st->is_return()) {
                ret *ret_stmt = (ret *) st;
                if (!ret_stmt->expression && rt->kind != type::t_void) {
                    error("Funtion '" + globid->identifier + "' has wrong return type. " + 
                    "Expect " + rt->name() + " but returns nothing.");
                }
                if (ret_stmt->expression && 
                    ret_stmt->expression->exp_type->kind != rt->kind) {
                    error("Funtion '" + globid->identifier + "' has wrong return type. " + 
                    "Expect " + rt->name() + " but got " + 
                    ret_stmt->expression->exp_type->name() + ".");
                }
            }
        }
    }
    vdecl_table.clear();
}

void func::yaml(ostream &os, string prefix) {
        os << prefix << "name: func" << endl;
        os << prefix << "ret_type: " << rt->name() << endl;
        os << prefix << "globid: " << globid->identifier << endl;
        os << prefix << "blk:" << endl;
        block->yaml(os, prefix + "  ");
        if (!variable_declarations) return;
        os << prefix << "vdecls:" << endl;
        variable_declarations->yaml(os, prefix + "  ");
}

void funcs::yaml(ostream &os, string prefix) {
        os << prefix << "name: funcs" << endl;
        os << prefix << "funcs:" << endl;
        for (auto fun : functions) {
            os << prefix << "  -" << endl;
            fun->yaml(os, prefix + "    ");
        }
}

ext::ext(type *r, id *g, tdecls *t) : rt(r), globid(g), type_declarations(t) {
    if (globid->identifier == "run") error("Function 'run' cannot be external.");
    if (extern_table.count(globid->identifier)) error("Duplicate declaration of function '" + globid->identifier + "'.");
    if (rt->ref) error("Function should not return a reference.");
    extern_table[globid->identifier] = this;
}

void ext::yaml(ostream &os, string prefix) {
        os << prefix << "name: extern" << endl;
        os << prefix << "ret_type: " << rt->name() << endl;
        os << prefix << "globid: " << globid->identifier << endl;
        if (!rt) return;
        os << prefix << "tdecls:" << endl;
        type_declarations->yaml(os, prefix + "  ");
} 

void exts::yaml(ostream &os, string prefix) {
        os << prefix << "name: externs" << endl;
        os << prefix << "externs: " << endl;
        for (auto e : externs) {
            os << prefix << "  -" << endl;
            e->yaml(os, prefix + "    ");
        }
}

prog::prog(funcs *f, exts *e) : functions(f), externs(e) {
    // Check there is a function named "run"
    if (function_table.count("run") == 0) error("Function 'run' not found.");
}

void prog::yaml(ostream &os, string prefix) {
        os << prefix << "name: prog" << endl;
        os << prefix << "funcs:" << endl;
        functions->yaml(os, prefix + "  ");
        if (!externs) return;
        os << prefix << "externs: " << endl;
        externs->yaml(os, prefix + "  ");
}
