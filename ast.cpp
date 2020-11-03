#include "ast.h"

#include <vector>
#include <unordered_map>
#include <iostream>
#include <type_traits>

using namespace std;

static unordered_map<string, func *> function_table;    // Table of all the declared functions.
static unordered_map<string, ext *> extern_table;       // Table of all the external functions. make
static unordered_map<string, vdecl *> vdecl_table;      // Table of all declared variables. 

void type::check_and_make_ref() {
    if (ref) error("Ref type may not refer to a reference.");
    if (kind == type::t_void) error("Ref type may not refer to void type.");
    ref = true;
}

void type::error(const string& err_msg) {
    cout << "error: " << err_msg << endl;
    exit(1);
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

lit::lit(int i): exp(new type(type::t_int)), it(i)  {}

void lit::yaml(ostream &os, string prefix) {
        os << prefix << "name: lit" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "value: " << it << endl;
}

flit::flit(float f): exp(new type(type::t_float)), flt(f) {}

void flit::yaml(ostream &os, string prefix) {
        os << prefix << "name: flit" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "value: " << flt << endl;
}

varval::varval(id *v) : exp(nullptr), variable(v) {
    if (!vdecl_table.count(v->identifier)) error("Variable " + v->identifier + " undeclared.");
    exp_type = vdecl_table[v->identifier]->tp;
}

void varval::yaml(ostream &os, string prefix) {
        os << prefix << "name: varval" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "var: " << variable->identifier << endl;
}

assign::assign(id *v, exp *e): exp(e->exp_type), variable(v), expression(e) {
    if (!vdecl_table.count(v->identifier)) error("Initilization of undeclared variable " + v->identifier + ".");
    type *var_type = vdecl_table[v->identifier]->tp;
    if (var_type->kind != e->exp_type->kind) {
        error("Types should be the same on both sides of assignment, but got " + 
              var_type->name() + 
              " and " + 
              e->exp_type->name() + 
              " instead.");
    }
}

void assign::yaml(ostream &os, string prefix) {
        os << prefix << "name: assign" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "var: " << variable->identifier << endl;
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
}

funccall::funccall(id *gid, exps *p) : exp(nullptr), globid(gid), params(p) {
    if (function_table.count(globid->identifier)) {
        func *f = function_table[globid->identifier];
        unsigned num_params = params->expressions.size(), 
                    num_decls = f->variable_declarations->variables.size();
        if (num_params != num_decls) 
            error("Arguments mismatch when calling function '" + globid->identifier + 
                    "'. Expecting " + to_string(num_decls) + 
                    " but get " + to_string(num_params) + ".");
        for (unsigned i = 0; i < num_decls; i++) {
            exp *expr = params->expressions[i];
            vdecl *var_declared = f->variable_declarations->variables[i];
            type *exp_tp = expr->exp_type, *decl_tp = var_declared->tp;
            if (exp_tp->kind != decl_tp->kind) 
                error("Function '" + globid->identifier + 
                        "' got wrong argument type. Argument " + to_string(i+1) + " should be " 
                        + decl_tp->name() + " but got " + exp_tp->name());
            if (decl_tp->ref && !expr->is_variable()) 
                error("Function '" + globid->identifier + 
                        "' expects variable (lvalue) for its reference argument " 
                        + to_string(i+1) + ".");
        }
        exp_type = f->rt;
    }
    else if (extern_table.count(globid->identifier)) {
        ext *e = extern_table[globid->identifier];
        unsigned num_params = params->expressions.size(), 
                    num_decls = e->type_declarations->types.size();
        if (num_params != num_decls) 
            error("Arguments mismatch when calling function '" + globid->identifier + 
                    "'. Expecting " + to_string(num_decls) + 
                    " but get " + to_string(num_params) + ".");
        for (unsigned i = 0; i < num_decls; i++) {
            exp *expr = params->expressions[i];
            type *tp_declared = e->type_declarations->types[i];
            type *exp_tp = expr->exp_type, *decl_tp = tp_declared;
            if (exp_tp->kind != decl_tp->kind) 
                error("Function '" + globid->identifier + 
                        "' got wrong argument type. Argument " + to_string(i+1) + " should be " 
                        + tp_declared->name() + " but get " + exp_tp->name());
            if (tp_declared->ref && !expr->is_variable()) 
                error("Function '" + globid->identifier + 
                        "' expects variable (lvalue) for its reference argument " 
                        + to_string(i+1) + ".");
        }
        exp_type = e->rt;
    }
    else error("Undeclared function '" + globid->identifier + "' is called.");
}

void funccall::yaml(ostream &os, string prefix) {
        os << prefix << "name: funccall" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "globid: " << globid->identifier << endl;
        if (!params) return;
        os << prefix << "params:" << endl;
        params->yaml(os, prefix + "  ");
}

uop::uop(uop_kind kd, exp *e): exp(e->exp_type), kind(kd), expression(e) {}

void uop::yaml(ostream &os, string prefix) {
        os << prefix << "name: uop" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "op: " << kind_name() << endl; 
        os << prefix << "exp:" << endl;
        expression->yaml(os, prefix + "  ");
}

binop::binop(binop_kind kd, exp *left, exp *right) : exp(left->exp_type), kind(kd), lhs(left), rhs(right) {
    if (left->exp_type->kind != right->exp_type->kind) {
        error("Types should be the same on both sides of " +
              this->kind_name() + 
              " but got " + 
              left->exp_type->name() + 
              " and " + 
              right->exp_type->name() + 
              " instead.");
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

castexp::castexp(type *t, exp *e) : exp(t), tp(t), expression(e) {}

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

vdeclstmt::vdeclstmt(vdecl *v, exp *e) : variable(v), expression(e) {
    if (v->tp->ref) {
        if (!is_same<varval, decltype(*e)>::value) error("Ref variable initilization expression must be a variable.");
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
    if (rt->ref) error("Function return type is a reference.");
    if (globid->identifier == "run") {
        // Funtion "run" must return int/cint and take no arguments.
        if (rt->kind != type::t_int && rt->kind != type::t_cint) 
            error("Funtion 'run' must have return type int or cint.");
        if (v != NULL) error("Funtion 'run' cannot have arguments.");
    }
    // Check return type
    if (block->statements == NULL && rt->kind != type::t_void) 
        error("Funtion '" + globid->identifier + "' has wrong return type. " + 
        "Empty body but non-void return type.");
    else {
        for (stmt *st : block->statements->statements) {
            if (st->is_return()) {
                type *ret_type = ((ret *) st)->expression->exp_type;
                if (ret_type->kind != rt->kind) 
                    error("Funtion '" + globid->identifier + "' has wrong return type. " + 
                    "Expect " + rt->name() + " but get " + 
                    ret_type->name() + ".");
            }
        }
    }
    function_table[globid->identifier] = this;
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
    if (rt->ref) error("Function return type is a reference.");
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

prog::prog(funcs *f, exts *e) : functions(f), e(e) {
    // Check there is a function named "run"
    if (function_table.count("run") == 0) error("Function 'run' not found.");
}

void prog::yaml(ostream &os, string prefix) {
        os << prefix << "name: prog" << endl;
        os << prefix << "funcs:" << endl;
        functions->yaml(os, prefix + "  ");
        if (!e) return;
        os << prefix << "externs: " << endl;
        e->yaml(os, prefix + "  ");
}