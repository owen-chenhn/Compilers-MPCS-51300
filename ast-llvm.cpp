/* Implementation of all the code_gen() member functions of AST nodes. */
#include "ast.h"

#include <vector>
#include <unordered_map>
#include <iostream>
#include <memory>
#include <cstdlib>
#include <climits>

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
#include "llvm/IR/GlobalValue.h"

#include "llvm/Support/TargetSelect.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>

using namespace std;
using namespace llvm;

// Table of all (mutable) variable addresses
static unordered_map<string, AllocaInst *> name_to_Value;
static Constant *printf_int_fmt = nullptr, *printf_float_fmt = nullptr;

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
    Value *exp_v = builder->CreateLoad(alloc_v, variable->identifier);
    if (exp_v && exp_v->getType()->isPointerTy()) 
        exp_v = builder->CreateLoad(exp_v, "loadtmp");
    return exp_v;
}

Value *assign::code_gen() {
    Value *exp_v = expression->code_gen();
    if (!exp_v) return nullptr;
    Value *alloc_v = variable->get_var_pointer();
    if (!alloc_v) return LogErrorV("Unknown variable name");
    if (variable->exp_type->ref) 
        alloc_v = builder->CreateLoad(alloc_v, "loadtmp");

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
                        expression->code_gen();
        args.push_back(arg_v);
    }

    if (called_func->getReturnType()->isVoidTy()) 
        return builder->CreateCall(called_func, args);
    return builder->CreateCall(called_func, args, "calltmp");
}


// Helper functions to generate code for overflow check of cint calculations
static void check_code_gen(Value *flag) {
    Function *the_func = builder->GetInsertBlock()->getParent();
    BasicBlock *overflow_bb = BasicBlock::Create(*context, "overflow", the_func), 
               *normal_bb = BasicBlock::Create(*context, "normal");
    builder->CreateCondBr(flag, overflow_bb, normal_bb);

    builder->SetInsertPoint(overflow_bb);
    // print error message and call exit(-1)
    Value *str_v = builder->CreateGlobalStringPtr(StringRef("Runtime error: cint overflows.\n"));
    Function* printFunc = module->getFunction("printf");
    builder->CreateCall(printFunc, str_v, "callprintf");

    // declare and call exit()
    FunctionType *ft = FunctionType::get(Type::getVoidTy(*context), Type::getInt32Ty(*context), false);
    Constant *exit_func = module->getOrInsertFunction("exit", ft);
    builder->CreateCall(exit_func, ConstantInt::get(Type::getInt32Ty(*context), -1));
    builder->CreateBr(normal_bb);

    the_func->getBasicBlockList().push_back(normal_bb);
    builder->SetInsertPoint(normal_bb);
}

static Value* check_overflow(Value *aggregate) {
    Value *result = builder->CreateExtractValue(aggregate, 0, "extract_res"), 
          *overflow = builder->CreateExtractValue(aggregate, 1, "extract_flag");
    
    check_code_gen(overflow);
    return result;
}

/* Functions that generate code for cint type. All the parameters must be of type t_cint. */
static Value* cint_neg(Value *unary) {
    Value *v = builder->CreateBinaryIntrinsic(Intrinsic::ssub_with_overflow, 
                    ConstantInt::get(Type::getInt32Ty(*context), 0), unary, "cint_negtmp");
    return check_overflow(v);
}

static Value* cint_add(Value *lhs, Value *rhs) {
    Value *v = builder->CreateBinaryIntrinsic(Intrinsic::sadd_with_overflow, lhs, rhs, "cint_addtmp");
    return check_overflow(v);
}

static Value* cint_sub(Value *lhs, Value *rhs) {
    Value *v = builder->CreateBinaryIntrinsic(Intrinsic::ssub_with_overflow, lhs, rhs, "cint_subtmp");
    return check_overflow(v);
}

static Value* cint_mul(Value *lhs, Value *rhs) {
    Value *v = builder->CreateBinaryIntrinsic(Intrinsic::smul_with_overflow, lhs, rhs, "cint_multmp");
    return check_overflow(v);
}

static Value* cint_div(Value *lhs, Value *rhs) {
    // Check whether (rhs == 0 || (lhs == INT_MIN && rhs == -1))
    Value *div_by_zero = builder->CreateICmpEQ(rhs, ConstantInt::get(Type::getInt32Ty(*context), 0), "eqzerotmp"); 
    Value *intmin_overflow = builder->CreateAnd(
                                builder->CreateICmpEQ(lhs, ConstantInt::get(Type::getInt32Ty(*context), INT_MIN), "eqintmintmp"), 
                                builder->CreateICmpEQ(rhs, ConstantInt::get(Type::getInt32Ty(*context), -1), "eqnegonetmp"), 
                                "cint_divandtmp");
    Value *flag = builder->CreateOr(div_by_zero, intmin_overflow, "cint_divcheck");
    check_code_gen(flag);
    return builder->CreateSDiv(lhs, rhs, "cint_divtmp");
}

Value* uop::code_gen() {
    Value *exp_v = expression->code_gen();
    type::type_kind t = expression->exp_type->kind;

    if (kind == uop_not) return builder->CreateNot(exp_v, "nottmp");
    return t == type::t_float ? 
           builder->CreateFNeg(exp_v, "negfptmp") : (
               t == type::t_cint ? 
               cint_neg(exp_v) :
               builder->CreateNeg(exp_v, "negtmp", true, true)
           );
}

Value* binop::code_gen() {
    Value* L = lhs->code_gen();
    Value* R = rhs->code_gen();
    type::type_kind t = lhs->exp_type->kind;

    switch (kind) {
        case bop_mul : 
            return t == type::t_float ?
                   builder->CreateFMul(L, R, "mulfptmp") : (
                       t == type::t_cint ? 
                       cint_mul(L, R) :
                       builder->CreateMul(L, R, "multmp", true, true)
                    );
        case bop_div : 
            return t == type::t_float ? 
                   builder->CreateFDiv(L, R, "divfptmp") : (
                       t == type::t_cint ? 
                       cint_div(L, R) :
                       builder->CreateSDiv(L, R, "sdivtmp")
                   );
        case bop_add : 
            return t == type::t_float ?
                   builder->CreateFAdd(L, R, "addfptmp") : (
                       t == type::t_cint ? 
                       cint_add(L, R) :
                       builder->CreateAdd(L, R, "addtmp", true, true)
                   );
        case bop_sub : 
            return t == type::t_float ?
                   builder->CreateFSub(L, R, "subfptmp") : (
                       t == type::t_cint ? 
                       cint_sub(L, R) :
                       builder->CreateSub(L, R, "subtmp", true, true)
                   );
        case bop_eq  : 
            return t == type::t_float ?
                   builder->CreateFCmpUEQ(L, R, "eqfptmp") : 
                   builder->CreateICmpEQ(L, R, "eqtmp");
        case bop_lt  : 
            return t == type::t_float ?
                   builder->CreateFCmpULT(L, R, "ltfptmp") : 
                   builder->CreateICmpULT(L, R, "lttmp");
        case bop_gt  : 
            return t == type::t_float ?
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
            return expression->exp_type->kind == type::t_float ? 
                builder->CreateFPToUI(e, map_llvm_type(type::t_int, false), "castfpittmp") : 
                builder->CreateIntCast(e, map_llvm_type(type::t_int, false), false, "castitittmp");
        case type::t_float:
            return expression->exp_type->kind == type::t_float ? 
                builder->CreateFPCast(e, map_llvm_type(type::t_float, false), "castfpfptmp") : 
                builder->CreateUIToFP(e, map_llvm_type(type::t_float, false), "castitfptmp") ;
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
    Value *exp_v;
    if (!variable->tp->ref) {
        exp_v = expression->code_gen();
    } else {
        exp_v = ((varval *)expression)->get_var_pointer();
        if (expression->exp_type->ref) 
            exp_v = builder->CreateLoad(exp_v, "loadtmp");
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
    return Constant::getNullValue(Type::getInt32Ty(*context));
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
    bool float_flag = expression->exp_type->kind == type::t_float;
    string format_str = float_flag ? "%f\n" : "%d\n";
    Value *str_v = builder->CreateGlobalStringPtr(StringRef(format_str));

    Value *exp_v = expression->code_gen();
    if (float_flag) 
        exp_v = builder->CreateFPExt(exp_v, Type::getDoubleTy(*context), "fpexttmp");

    vector<Value *> args = { str_v, exp_v };
    Function* printFunc = module->getFunction("printf");
    if (!printFunc) return LogErrorV("Function printf undeclared");

    return builder->CreateCall(printFunc, args, "callprintf");
}

Value *printslit::code_gen() {
    // generate code for string
    Value *str_v = builder->CreateGlobalStringPtr(StringRef(str + '\n'));

    Function* printFunc = module->getFunction("printf");
    if (!printFunc) return LogErrorV("Function printf undeclared");

    vector<Value *> args = { str_v };
    return builder->CreateCall(printFunc, args, "callprintf");
}

Function *func::code_gen() {
    unsigned params = variable_declarations ? variable_declarations->variables.size() : 0;
    vector<Type *> param_types;
    vector<unsigned> noalis_index;
    for (unsigned i = 0; i < params; i++) {
        type *tp = variable_declarations->variables[i]->tp;
        param_types.push_back(map_llvm_type(tp->kind, tp->ref));

        if (tp->noalias) noalis_index.push_back(i);
    }

    Type *ret_type = map_llvm_type(rt->kind, false);
    FunctionType *ft = FunctionType::get(ret_type, param_types, false);
    Function *f = Function::Create(ft, Function::ExternalLinkage, globid->identifier, module.get());

    for (unsigned idx : noalis_index) {
        f->addParamAttr(idx, Attribute::NoAlias);
    }

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
    verifyFunction(*f, &llvm::errs());
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

    verifyModule(*module, &llvm::errs());
    return module.get();
}

// Define function pointer of the run function.
typedef int (*runFunc)();

// Static variables to implement arg/argf functions. 
static int argcount = 0;
static char** argstrings = NULL;

extern "C" {
    int arg(int i) {
        if (i < 0 || i >= argcount) {
            cout << "Error: arg()'s index out of bound.\n";
            exit(-1);
        }
        return atoi(argstrings[i]);
    }

    float argf(int i) {
        if (i < 0 || i >= argcount) {
            cout << "Error: argf()'s index out of bound.\n";
            exit(-1);
        }
        return atof(argstrings[i]);
    }
}

/* The JIT implementation. */
void prog::jit(int argc, char** argv) {
    argcount = argc;
    argstrings = argv;
    
    LLVMLinkInMCJIT();
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    ExecutionEngine *ee = EngineBuilder(std::move(module)).create();
    ee->finalizeObject();
    
    runFunc run = (runFunc) ee->getFunctionAddress("run");
    run();
}
