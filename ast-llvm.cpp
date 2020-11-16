/* Implementation of all the code_gen() member functions of AST nodes. */
#include "ast.h"

#include <vector>
#include <unordered_map>
#include <iostream>

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

using namespace std;
using namespace llvm;

// Table of all (mutable) variable addresses
static unordered_map<string, AllocaInst *> name_to_Value;

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

AllocaInst *vdecl::code_gen() {
    AllocaInst *alloc_var = builder->CreateAlloca(map_llvm_type(tp->kind), 
                                        variable->identifier);
    name_to_Value[variable->identifier] = alloc_var;
    return alloc_var;
}

Value* lit::code_gen() {
    return ConstantInt::get(map_llvm_type(type::t_int), it);
}

Value* flit::code_gen() {
    return ConstantFP::get(map_llvm_type(type::t_float), flt);
}

Value* varval::code_gen() {
    if (!name_to_Value.count(variable->identifier)) 
        return LogErrorV("Unknown variable name");
    AllocaInst *alloc_v = name_to_Value[variable->identifier];
    return builder->CreateLoad(alloc_v, variable->identifier);
}

Value *assign::code_gen() {
    Value *exp_v = expression->code_gen();
    if (!exp_v) return nullptr;
    AllocaInst *alloc_v = name_to_Value[variable->variable->identifier];
    if (!alloc_v) return LogErrorV("Unknown variable name");

    builder->CreateStore(exp_v, alloc_v);
    return exp_v;
}

Value* funccall::code_gen() {
    Function* called_func = module->getFunction(globid->identifier);
    if (!called_func) return LogErrorV("Unknown function referenced");

    if (called_func->arg_size() != params->expressions.size()) 
        return LogErrorV("Incorrect # of arguments");

    vector<Value* > args;
    for (int i = 0; i < params->expressions.size(); i++) {
        args.push_back(params->expressions[i]->code_gen());
    }

    return builder->CreateCall(called_func, args, "calltmp");
}

Value* uop::code_gen() {
    if (kind == uop_not) return builder->CreateNot(expression->code_gen(), "nottmp");
    return expression->exp_type->kind == type::t_float ? 
           builder->CreateFNeg(expression->code_gen(), "negfptmp") : 
           builder->CreateNeg(expression->code_gen(), "negtmp", true, true);
}

Value* binop::code_gen() {
    Value* L = lhs->code_gen();
    Value* R = rhs->code_gen();

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

Value *ret::code_gen() {
    if (expression) {
        Value *exp_v = expression->code_gen();
        if (!exp_v) return nullptr;
        return builder->CreateRet(exp_v);
    }
    else
        return builder->CreateRetVoid();
}

Value *vdeclstmt::code_gen() {
    AllocaInst *alloc_v = variable->code_gen();
    Value *exp_v = expression->code_gen();
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
    Value *cond_v = condition->code_gen();
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
    return Constant::getNullValue(Type::getVoidTy(*context));
}

Value *ifstmt::code_gen() {
    Value *cond_v = condition->code_gen();
    if (!cond_v) return nullptr;

    Function *the_func = builder->GetInsertBlock()->getParent();
    BasicBlock *then_bb = BasicBlock::Create(*context, "then", the_func), 
               *else_bb = BasicBlock::Create(*context, "else"), 
               *merge_bb = BasicBlock::Create(*context, "if_exit");
    builder->CreateCondBr(cond_v, then_bb, else_bb);

    // insert then's code
    builder->SetInsertPoint(then_bb);
    Value *then_v = statement->code_gen();
    builder->CreateBr(merge_bb);
    then_bb = builder->GetInsertBlock();

    // insert else's code
    the_func->getBasicBlockList().push_back(else_bb);
    builder->SetInsertPoint(else_bb);
    if (else_statement) else_statement->code_gen();
    builder->CreateBr(merge_bb);
    else_bb = builder->GetInsertBlock();

    // merge block
    the_func->getBasicBlockList().push_back(merge_bb);
    builder->SetInsertPoint(merge_bb);
    return Constant::getNullValue(Type::getVoidTy(*context));
}

Value *print::code_gen() {}

Value *printslit::code_gen() {}

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
        arg.setName(variable_declarations->variables[i++]->getName());
    }

    // function body
    BasicBlock *bb = BasicBlock::Create(*context, "entry", f);
    builder->SetInsertPoint(bb);
    // Setup function argument variables
    name_to_Value.clear();
    unsigned i = 0;
    for (auto &arg : f->args()) {
        AllocaInst *alloc_param = variable_declarations->variables[i++]->code_gen();
        builder->CreateStore(&arg, alloc_param);
    }

    block->code_gen();
    if (rt->kind == type::t_void)
        builder->CreateRetVoid();
    
    // verify the generated code
    if (verifyFunction(*f))
        error("Verification of code generation failed.");
    return f;
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
    Function *f = Function::Create(ft, Function::ExternalLinkage, globid->identifier, module.get());

    return f;
}

Module *prog::code_gen() {
    if (externs) {
        for (ext *e : externs->externs) 
            e->code_gen();
    }

    for (func *f : functions->functions) 
        f->code_gen();

    return module.get();
}
