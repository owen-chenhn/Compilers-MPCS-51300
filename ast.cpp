#include "ast.h"

#include <vector>
#include <unordered_map>
#include <iostream>
#include <type_traits>

#include "llvm/IR/Type.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"

using namespace std;
using namespace llvm;

static unordered_map<string, func *> function_table;    // Table of all the declared functions.
static unordered_map<string, ext *> extern_table;       // Table of all the external functions. make
static unordered_map<string, vdecl *> vdecl_table;      // Table of all declared variables. 
static unordered_map<string, Value *> name_to_Value;    // Table of all Value* s 

// Classes for llvm code generation
static unique_ptr<LLVMContext> context;
static unique_ptr<IRBuilder<> > builder = llvm::make_unique<IRBuilder<> >(*context);
static unique_ptr<Module> module = llvm::make_unique<Module>("EKProgram", *context);

// Helper function: map class type to llvm Type*
Type *map_llvm_type(type::type_kind t) {
    Type *type_ptr;
    switch (t) {
        case type::t_void  : type_ptr = Type::getVoidTy(*context);  break;
        case type::t_bool  : type_ptr = Type::getInt1Ty(*context);  break;
        case type::t_int   : type_ptr = Type::getInt32Ty(*context); break;
        case type::t_cint  : type_ptr = Type::getInt32Ty(*context); break;
        case type::t_float : type_ptr = Type::getFloatTy(*context); break;
    };
    return type_ptr;
}

// method to print code gen log error and return Value * 
Value *LogErrorV(const string& str) {
    cout << "Error: " + str << endl;
    return nullptr;
}

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

void lit::yaml(ostream &os, string prefix) {
        os << prefix << "name: lit" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "value: " << it << endl;
}

Value* lit::code_gen() {
    return ConstantInt::get(map_llvm_type(type::t_int), it);
}

void flit::yaml(ostream &os, string prefix) {
        os << prefix << "name: flit" << endl;
        os << prefix << "type: " << exp_type->name() << endl;
        os << prefix << "value: " << flt << endl;
}

Value* flit::code_gen() {
    return ConstantFP::get(map_llvm_type(type::t_float), flt);
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

Value* varval::code_gen() {
    if (!name_to_Value.count(variable->identifier)) 
        return LogErrorV("Unknown variable name");
    return name_to_Value[variable->identifier];
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

void funccall::check_type() {
    if (params) {
        for (exp *expr : params->expressions) expr->check_type(); 
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
        exp *expr = params->expressions[i];
        type *exp_tp  = expr->exp_type, 
             *decl_tp = flag ? f->variable_declarations->variables[i]->tp 
                             : e->type_declarations->types[i];
        if (exp_tp->kind != decl_tp->kind) {
            error("Function '" + globid->identifier + 
                  "' got wrong argument type. Argument " + to_string(i+1) + 
                  " should be " + decl_tp->name() + " but got " + exp_tp->name());
        }
        if (decl_tp->ref && !expr->is_variable()) {
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

Value* funccall::code_gen() {
    Function* called_func = module->getFunction(globid->identifier);
    if (!called_func) return LogErrorV("Unknown function referenced");

    if (called_func->arg_size() != params->expressions.size()) return LogErrorV("Incorrect # of arguments");

    vector<Value* > args;
    for (int i = 0; i < params->expressions.size(); i++) {
        args.push_back(params->expressions[i]->code_gen());
    }

    return builder->CreateCall(called_func, args, "calltmp");
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

Value* uop::code_gen() {
    if (kind == uop_not) return builder->CreateNot(expression->code_gen(), "nottmp");
    return builder->CreateNeg(expression->code_gen(), "negtmp", true, true);
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

Value* binop::code_gen() {
    Value* L = lhs->code_gen();
    Value* R = rhs->code_gen();

    switch (kind) {
        case bop_mul : 
            return builder->CreateFAdd(L, R, "multmp");
        case bop_div : 
            return builder->CreateFDiv(L, R, "divtmp");
        case bop_add : 
            return builder->CreateFAdd(L, R, "addtmp");
        case bop_sub : 
            return builder->CreateFSub(L, R, "subtmp");
        case bop_eq  : 
            return builder->CreateFCmpUEQ(L, R, "eqtmp");
        case bop_lt  : 
            return builder->CreateFCmpULT(L, R, "lttmp");
        case bop_gt  : 
            return builder->CreateFCmpUGT(L, R, "gtmp");
        case bop_and :
            return builder->CreateAnd(L, R, "andtmp");
        case bop_or  : 
            return builder->CreateOr(L, R, "ortmp");           
    };
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

// assuming type check has already been performed 
Value* castexp::code_gen() {
    Value* e = expression->code_gen();
    switch(tp->kind) {
        case type::t_int :
            return builder->CreateIntCast(e, map_llvm_type(type::t_int), false, "castinttmp");
        case type::t_float:
            return builder->CreateFPCast(e, map_llvm_type(type::t_float), "castfloattmp");
        case type::t_bool:
            return e;
    }
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

Function *func::code_gen() {
    unsigned params = variable_declarations ? variable_declarations->variables.size() : 0;
    vector<Type *> param_types;
    for (unsigned i = 0; i < params; i++) {
        type::type_kind tkind = variable_declarations->variables[i]->getTypeKind();
        param_types.push_back(map_llvm_type(tkind));
    }

    Type *ret_type = map_llvm_type(rt->kind);
    FunctionType *ft = FunctionType::get(ret_type, param_types, true);
    Function *f = Function::Create(ft, Function::ExternalLinkage, globid->identifier, module.get());

    unsigned i = 0;
    for (auto &arg : f->args()) {
        arg.setName(variable_declarations->variables[i]->getName());
    }

    // function body
    BasicBlock *bb = BasicBlock::Create(context, "entry", f);
    builder.SetInsertPoint(bb);
    Value *ret_val = block->code_gen();
    builder.CreateRet(ret_val);
    
    // verify the generated code
    if (verifyFunction(*f))
        error("Verification of code generation failed.");
    return f;
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

Function *ext::code_gen() {
    unsigned params = type_declarations ? type_declarations->types.size() : 0;
    vector<Type *> param_types;
    for (unsigned i = 0; i < params; i++) {
        type::type_kind tkind = type_declarations->types[i]->kind;
        param_types.push_back(map_llvm_type(tkind));
    }

    Type *ret_type = map_llvm_type(rt->kind);
    FunctionType *ft = FunctionType::get(ret_type, param_types, false);
    Function *f = Function::Create(ft, Function::ExternalLinkage, globid->identifier, module);

    return f;
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