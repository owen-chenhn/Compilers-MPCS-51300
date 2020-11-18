/* Implementation of all the code_gen() member functions of AST nodes. */
#include "ast.h"

#include <vector>
#include <unordered_map>
#include <iostream>
#include <memory>

#include "llvm/IR/Type.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Verifier.h"
#include "llvm/ADT/StringRef.h"

using namespace std;
using namespace llvm;

// Table of all (mutable) variable addresses
static unordered_map<string, AllocaInst *> name_to_Value;

// Classes for llvm code generation
unique_ptr<LLVMContext> context = make_unique<LLVMContext>();
unique_ptr<IRBuilder<> > builder = make_unique<IRBuilder<> >(*context);
unique_ptr<Module> module = make_unique<Module>("EKProgram", *context);

// Helper function: map class type to llvm Type*
Type *map_llvm_type(type::type_kind t, bool ref) {
    switch (t) {
    case type::t_bool: 
        if (ref) return Type::getInt1PtrTy(*context);
        else return Type::getInt1Ty(*context);

    case type::t_cint:
    case type::t_int:
        if (ref) return Type::getInt32PtrTy(*context);
        else return Type::getInt32Ty(*context);

    case type::t_float:
        if (ref) return Type::getFloatPtrTy(*context);
        else return Type::getFloatTy(*context);

    default: return Type::getVoidTy(*context);
    };
}

// method to print code gen log error and return Value * 
Value *LogErrorV(const string& str) {
    cout << "Error: " + str << endl;
    return nullptr;
}

// method to print code gen log error and return Function * 
Function *LogErrorF(const string& str) {
    cout << "Error: " + str << endl;
    return nullptr;
}

// Helper function to dereference a pointer value
Value* expr_deref(Value *expression) {
    if (expression && expression->getType()->isPointerTy()) {
        return builder->CreateLoad(expression, "deref");
    }
    return expression;
}

AllocaInst *vdecl::code_gen() {
    AllocaInst *alloc_var = builder->CreateAlloca(map_llvm_type(tp->kind, tp->ref), 
                                    nullptr, variable->identifier);
    name_to_Value[variable->identifier] = alloc_var;
    return alloc_var;
}

Value* lit::code_gen() {
    return ConstantInt::get(map_llvm_type(type::t_int, false), it);
}

Value* flit::code_gen() {
    return ConstantFP::get(map_llvm_type(type::t_float, false), flt);
}

AllocaInst* varval::get_var_pointer() {
    return name_to_Value[variable->identifier];
}

Value* varval::code_gen() {
    AllocaInst *alloc_v = get_var_pointer();
    if (!alloc_v) 
        return LogErrorV("Unknown variable name");
    return builder->CreateLoad(alloc_v, variable->identifier);
}

Value *assign::code_gen() {
    Value *exp_v = expr_deref(expression->code_gen());
    if (!exp_v) return nullptr;
    Value *alloc_v = name_to_Value[variable->variable->identifier];
    if (!alloc_v) return LogErrorV("Unknown variable name");
    if (variable->exp_type->ref) 
        alloc_v = builder->CreateLoad(alloc_v, "deref");

    builder->CreateStore(exp_v, alloc_v);
    return exp_v;
}

Value* funccall::code_gen() {
    Function* called_func = module->getFunction(globid->identifier);
    if (!called_func) return LogErrorV("Unknown function referenced");

    if (called_func->arg_size() != params->expressions.size())
        return LogErrorV("Incorrect # of arguments");

    vector<Value* > args;
    unsigned i = 0;
    for (auto &Arg : called_func->args()) {
        expr *expression = params->expressions[i++];
        Value *arg_v = Arg.getType()->isPointerTy() ? 
                        ((varval *)expression)->get_var_pointer() : 
                        expr_deref(expression->code_gen());
        args.push_back(arg_v);
    }

    if (called_func->getReturnType()->isVoidTy()) 
        return builder->CreateCall(called_func, args);
    return builder->CreateCall(called_func, args, "calltmp");
}

Value* uop::code_gen() {
    if (kind == uop_not) return builder->CreateNot(expr_deref(expression->code_gen()), "nottmp");
    return expression->exp_type->kind == type::t_float ? 
           builder->CreateFNeg(expr_deref(expression->code_gen()), "negfptmp") : 
           builder->CreateNeg(expr_deref(expression->code_gen()), "negtmp", true, true);
}

Value* binop::code_gen() {
    Value* L = expr_deref(lhs->code_gen());
    Value* R = expr_deref(rhs->code_gen());

    switch (kind) {
        case bop_mul : 
            return lhs->exp_type->kind == type::t_float ?
                   builder->CreateFMul(L, R, "mulfptmp") : 
                   builder->CreateMul(L, R, "multmp", true, true);
        case bop_div : 
            return builder->CreateFDiv(L, R, "divfptmp");
        case bop_add : 
            return lhs->exp_type->kind == type::t_float ?
                   builder->CreateFAdd(L, R, "addfptmp") : 
                   builder->CreateAdd(L, R, "addtmp", true, true);
        case bop_sub : 
            return lhs->exp_type->kind == type::t_float ?
                   builder->CreateFSub(L, R, "subfptmp") : 
                   builder->CreateSub(L, R, "subtmp", true, true);
        case bop_eq  : 
            return lhs->exp_type->kind == type::t_float ?
                   builder->CreateFCmpUEQ(L, R, "eqfptmp") : 
                   builder->CreateICmpEQ(L, R, "eqtmp");
        case bop_lt  : 
            return lhs->exp_type->kind == type::t_float ?
                   builder->CreateFCmpULT(L, R, "ltfptmp") : 
                   builder->CreateICmpULT(L, R, "lttmp");
        case bop_gt  : 
            return lhs->exp_type->kind == type::t_float ?
                   builder->CreateFCmpUGT(L, R, "gtfptmp") : 
                   builder->CreateICmpUGT(L, R, "gttmp");
        case bop_and :
            return builder->CreateAnd(L, R, "andtmp");
        case bop_or  : 
            return builder->CreateOr(L, R, "ortmp");           
    };
}

// assuming type check has already been performed 
Value* castexp::code_gen() {
    Value* e = expr_deref(expression->code_gen());
    switch(tp->kind) {
        case type::t_int :
            return builder->CreateIntCast(e, map_llvm_type(type::t_int, false), false, "castinttmp");
        case type::t_float:
            return builder->CreateFPCast(e, map_llvm_type(type::t_float, false), "castfloattmp");
        case type::t_bool:
            return e;
    }
}

Value *ret::code_gen() {
    if (expression) {
        Value *exp_v = expr_deref(expression->code_gen());
        if (!exp_v) return nullptr;
        return builder->CreateRet(exp_v);
    }
    else
        return builder->CreateRetVoid();
}

Value *vdeclstmt::code_gen() {
    AllocaInst *alloc_v = variable->code_gen();
    Value *exp_v;
    if (variable->tp->ref && !expression->exp_type->ref) {
        exp_v = ((varval *)expression)->get_var_pointer();
    }
    else {
        exp_v = expression->code_gen();
        if (!variable->tp->ref && expression->exp_type->ref) 
            exp_v = expr_deref(exp_v);
    }
    if (!exp_v || !alloc_v) return nullptr;

    builder->CreateStore(exp_v, alloc_v);
    return exp_v;
}

Value *whilestmt::code_gen() {
    Function *the_func = builder->GetInsertBlock()->getParent();
    BasicBlock *head_bb = BasicBlock::Create(*context, "loop_head", the_func), 
               *body_bb = BasicBlock::Create(*context, "loop_body"), 
               *exit_bb = BasicBlock::Create(*context, "loop_exit");
    
    builder->CreateBr(head_bb);
    builder->SetInsertPoint(head_bb);
    Value *cond_v = expr_deref(condition->code_gen());
    if (!cond_v) return nullptr;
    builder->CreateCondBr(cond_v, body_bb, exit_bb);

    // loop body
    the_func->getBasicBlockList().push_back(body_bb);
    builder->SetInsertPoint(body_bb);
    statement->code_gen();
    builder->CreateBr(head_bb);

    // loop exit
    the_func->getBasicBlockList().push_back(exit_bb);
    builder->SetInsertPoint(exit_bb);
    return Constant::getNullValue(Type::getInt32Ty(*context));
}

Value *ifstmt::code_gen() {
    Value *cond_v = expr_deref(condition->code_gen());
    if (!cond_v) return nullptr;

    Function *the_func = builder->GetInsertBlock()->getParent();
    BasicBlock *then_bb = BasicBlock::Create(*context, "then", the_func), 
               *else_bb = BasicBlock::Create(*context, "else"), 
               *merge_bb = BasicBlock::Create(*context, "if_exit");
    builder->CreateCondBr(cond_v, then_bb, else_bb);

    // insert then's code
    builder->SetInsertPoint(then_bb);
    Value *then_v = statement->code_gen();
    if (!then_v) return nullptr;
    builder->CreateBr(merge_bb);

    // insert else's code
    the_func->getBasicBlockList().push_back(else_bb);
    builder->SetInsertPoint(else_bb);
    if (else_statement) {
        if (!else_statement->code_gen()) return nullptr;
    }
    builder->CreateBr(merge_bb);

    // merge block
    the_func->getBasicBlockList().push_back(merge_bb);
    builder->SetInsertPoint(merge_bb);
    return Constant::getNullValue(Type::getInt32Ty(*context));
}

Value *print::code_gen() {
    Value *exp_v = expr_deref(expression->code_gen());
    string format_str = (expression->exp_type->kind == type::t_float) ?
                        "%f\n" : 
                        "%d\n";
    Value *str_v = builder->CreateGlobalStringPtr(StringRef(format_str));

    vector<Value *> args = { str_v, exp_v };
    Function* printFunc = module->getFunction("printf");
    if (!printFunc) return LogErrorV("Function printf undeclared");

    return builder->CreateCall(printFunc, args, "callprintf");
}

Value *printslit::code_gen() {
    // generate code for string
    Value *str_v = builder->CreateGlobalStringPtr(StringRef(str+'\n'));

    Function* printFunc = module->getFunction("printf");
    if (!printFunc) return LogErrorV("Function printf undeclared");

    vector<Value *> args = { str_v };
    return builder->CreateCall(printFunc, args, "callprintf");
}

Function *func::code_gen() {
    unsigned params = variable_declarations ? variable_declarations->variables.size() : 0;
    vector<Type *> param_types;
    for (unsigned i = 0; i < params; i++) {
        type *tp = variable_declarations->variables[i]->tp;
        param_types.push_back(map_llvm_type(tp->kind, tp->ref));
    }

    Type *ret_type = map_llvm_type(rt->kind, false);
    FunctionType *ft = FunctionType::get(ret_type, param_types, false);
    Function *f = Function::Create(ft, Function::ExternalLinkage, globid->identifier, module.get());

    unsigned i = 0;
    for (auto &arg : f->args()) {
        arg.setName(variable_declarations->variables[i++]->getName());
    }

    // function body
    BasicBlock *bb = BasicBlock::Create(*context, "entry", f);
    builder->SetInsertPoint(bb);
    // Setup function argument variables
    name_to_Value.clear();
    i = 0;
    for (auto &arg : f->args()) {
        AllocaInst *alloc_param = variable_declarations->variables[i++]->code_gen();
        builder->CreateStore(&arg, alloc_param);
    }

    block->code_gen();
    if (rt->kind == type::t_void)
        builder->CreateRetVoid();
    
    // verify the generated code
    if (verifyFunction(*f))
        return LogErrorF("Verification of code generation failed.");
    return f;
}

Function *ext::code_gen() {
    unsigned params = type_declarations ? type_declarations->types.size() : 0;
    vector<Type *> param_types;
    for (unsigned i = 0; i < params; i++) {
        type *tp = type_declarations->types[i];
        param_types.push_back(map_llvm_type(tp->kind, tp->ref));
    }

    Type *ret_type = map_llvm_type(rt->kind, false);
    FunctionType *ft = FunctionType::get(ret_type, param_types, false);
    Function *f = Function::Create(ft, Function::ExternalLinkage, globid->identifier, module.get());

    return f;
}


/* Helper functions to declare library print functions. */
static void declare_printf() {
    // declare printStr
    FunctionType *ft = FunctionType::get(Type::getInt32Ty(*context), 
                        Type::getInt8PtrTy(*context), true);
    module->getOrInsertFunction("printf", ft);
}

Module *prog::code_gen() {
    if (externs) {
        for (ext *e : externs->externs) 
            e->code_gen();
    }

    declare_printf();

    for (func *f : functions->functions) 
        f->code_gen();

    return module.get();
}
