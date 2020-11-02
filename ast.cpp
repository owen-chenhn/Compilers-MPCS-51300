#include "ast.h"

#include <vector>
#include <unordered_map>
#include <iostream>

using namespace std;

static unordered_map<string, func *> function_table;    // Table of all the declared functions.
static unordered_map<string, ext *> extern_table;       // Table of all the external functions. make

static const string type_names[5] = {
    "void", 
    "bool", 
    "int", 
    "cint", 
    "float"
};

void type::check_and_make_ref() {
    if (ref) error("Ref type may not refer to a reference.");
    if (kind == type::t_void) error("Ref type may not refer to void type.");
    ref = true;
}

void type::error(const string& err_msg) {
    cout << "error: " << err_msg << endl;
    exit(1);
}

vdecl::vdecl(type *t, id *var) :tp(t), variable(var) {
    if (t->kind == type::t_void) error("Variable type may not be void.");
}

funccall::funccall(id *gid, exps *p) : globid(gid), params(p) {
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
            vdecl *var_declare = f->variable_declarations->variables[i];
            type::type_kind exp_tp = expr->exp_type->kind, decl_tp = var_declare->tp->kind;
            if (exp_tp != decl_tp) 
                error("Function '" + globid->identifier + 
                        "' got wrong argument type. Argument " + to_string(i+1) + " should be " 
                        + type_names[decl_tp] + " but get " + type_names[exp_tp]);
            if (var_declare->tp->ref && !expr->is_variable()) 
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
            type *tp_declare = e->type_declarations->types[i];
            type::type_kind exp_tp = expr->exp_type->kind, decl_tp = tp_declare->kind;
            if (exp_tp != decl_tp) 
                error("Function '" + globid->identifier + 
                        "' got wrong argument type. Argument " + to_string(i+1) + " should be " 
                        + type_names[decl_tp] + " but get " + type_names[exp_tp]);
            if (tp_declare->ref && !expr->is_variable()) 
                error("Function '" + globid->identifier + 
                        "' expects variable (lvalue) for its reference argument " 
                        + to_string(i+1) + ".");
        }
        exp_type = e->rt;
    }
    else error("Undeclared function '" + globid->identifier + "' is called.");
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
                type::type_kind ret_type = ((ret *) st)->expression->exp_type->kind;
                if (ret_type != rt->kind) 
                    error("Funtion '" + globid->identifier + "' has wrong return type. " + 
                    "Expect " + type_names[rt->kind] + " but get " + 
                    type_names[ret_type] + ".");
            }
        }
    }
    function_table[globid->identifier] = this;
}


ext::ext(type *r, id *g, tdecls *t) : rt(r), globid(g), type_declarations(t) {
    if (globid->identifier == "run") error("Function 'run' cannot be external.");
    if (extern_table.count(globid->identifier)) error("Duplicate declaration of function '" + globid->identifier + "'.");
    if (rt->ref) error("Function return type is a reference.");
    extern_table[globid->identifier] = this;
}


prog::prog(funcs *f, exts *e) : functions(f), e(e) {
    // Check there is a function named "run"
    if (function_table.count("run") == 0) error("Function 'run' not found.");
}
