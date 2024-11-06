#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/TargetParser/Host.h>

#include "../include/irgen.h"

IRGen::IRGen(
    std::vector<std::unique_ptr<DecoratedFunctionDecl>> dast,
    std::string_view src_path)
    : dast(std::move(dast)), builder(context),
      module(std::make_unique<llvm::Module>("<trans_unit>", context)) {
  module->setSourceFileName(src_path);
  module->setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

void IRGen::generate_function_decl(const DecoratedFunctionDecl &function_decl) {
    auto *ret_type = generate_type(function_decl.type);

    std::vector<llvm::Type *> param_types;
    for (auto &&param : function_decl.params)
        param_types.emplace_back(generate_type(param->type));

    auto type = llvm::FunctionType::get(ret_type, param_types, false);

    llvm::Function::Create(type, llvm::Function::ExternalLinkage,
            function_decl.identifier, *module);
}

llvm::AllocaInst *
IRGen::allocate_stack_variable(llvm::Function *function,
        const std::string_view identifier) {
    llvm::IRBuilder<> tmp(context);
    tmp.SetInsertPoint(alloca_insert_point);

    return tmp.CreateAlloca(tmp.getDoubleTy());
}

llvm::Value *IRGen::generate_call_expr(const DecoratedCallExpr &call) {
    llvm::Function *callee = module->getFunction(call.callee->identifier);

    std::vector<llvm::Value *> args;
    for (auto &&arg : call.arguments)
        args.emplace_back(generate_expr(*arg));

    return builder.CreateCall(callee, args);
}

llvm::Value *IRGen::generate_expr(const DecoratedExpr &expr) {
    if (auto *number = dynamic_cast<const DecoratedNumberLiteral *>(&expr))
        return llvm::ConstantFP::get(builder.getDoubleTy(), number->value);

    if (auto *dre = dynamic_cast<const DecoratedDeclRefExpr *>(&expr))
        return builder.CreateLoad(builder.getDoubleTy(), declarations[dre->decl]);

    if (auto *call = dynamic_cast<const DecoratedCallExpr *>(&expr))
        return generate_call_expr(*call);

    llvm_unreachable("unexpected expression");
}

llvm::Value *IRGen::generate_return_stmt(const DecoratedReturnStmt &stmt) {
  if (stmt.expr)
    builder.CreateStore(generate_expr(*stmt.expr), ret_val);

  return builder.CreateBr(ret_block);
}

llvm::Value *IRGen::generate_stmt(const DecoratedStmt &stmt) {
    if (auto *expr = dynamic_cast<const DecoratedExpr *>(&stmt))
        return generate_expr(*expr);

    if (auto *rstmt = dynamic_cast<const DecoratedReturnStmt *>(&stmt))
        return generate_return_stmt(*rstmt);

    llvm_unreachable("unknown statement");
    return nullptr;
}

void IRGen::generate_block(const DecoratedBlock &block) {
    for (auto &&stmt : block.statements) {
        generate_stmt(*stmt);

        // Don't need to parse after the first return statement
        if (dynamic_cast<const DecoratedReturnStmt *>(stmt.get())) {
            builder.ClearInsertionPoint();
            break;
        }
    }
}

void IRGen::generate_function_body(const DecoratedFunctionDecl &function_decl) {
    auto *function = module->getFunction(function_decl.identifier);

    auto *entry = llvm::BasicBlock::Create(context, "entry", function);
    builder.SetInsertPoint(entry);

    llvm::Value *undef = llvm::UndefValue::get(builder.getInt32Ty());
    alloca_insert_point = new llvm::BitCastInst(undef, undef->getType(),
            "alloca.placeholder", entry);

    bool is_void = function_decl.type.kind == Type::Kind::Void;
    if (!is_void)
        ret_val = allocate_stack_variable(function, "ret_val");
    ret_block = llvm::BasicBlock::Create(context, "return");

    int idx = 0;
    for (auto &&arg : function->args()) {
        const auto *param_decl = function_decl.params[idx].get();
        arg.setName(param_decl->identifier);

        llvm::Value *var = allocate_stack_variable(function, param_decl->identifier);
        builder.CreateStore(&arg, var);

        declarations[param_decl] = var;

        idx++;
    }

    if (function_decl.identifier == "display")
        generate_builtin_display_body(function_decl);
    else
        generate_block(*function_decl.body);

    if (ret_block->hasNPredecessorsOrMore(1)) {
        builder.CreateBr(ret_block);
        ret_block->insertInto(function);
        builder.SetInsertPoint(ret_block);
    }

    alloca_insert_point->eraseFromParent();
    alloca_insert_point = nullptr;

    if (is_void) {
        builder.CreateRetVoid();
        return;
    }

    builder.CreateRet(builder.CreateLoad(builder.getDoubleTy(), ret_val));
}

void IRGen::generate_builtin_display_body(const DecoratedFunctionDecl &display) {
    auto *type = llvm::FunctionType::get(builder.getInt32Ty(),
					 {llvm::PointerType::getUnqual(builder.getInt8Ty())}, true);

    auto *printf = llvm::Function::Create(type, llvm::Function::ExternalLinkage,
            "printf", *module);

    auto *format = builder.CreateGlobalString("%.15g\n");

    llvm::Value *param = builder.CreateLoad(
            builder.getDoubleTy(), declarations[display.params[0].get()]);

    builder.CreateCall(printf, {format, param});
}

std::unique_ptr<llvm::Module> IRGen::generate_ir() {
    for (auto &&function : dast)
        generate_function_decl(*function);

    for (auto &&function : dast)
        generate_function_body(*function);

    generate_main_wrapper();

    return std::move(module);
}

void IRGen::generate_main_wrapper() {
    auto *builtin_main = module->getFunction("main");
    builtin_main->setName("__builtin_main__");

    auto *main = llvm::Function::Create(
            llvm::FunctionType::get(builder.getInt32Ty(), {}, false),
            llvm::Function::ExternalLinkage, "main", *module);

    auto *entry = llvm::BasicBlock::Create(context, "entry", main);
    builder.SetInsertPoint(entry);

    builder.CreateCall(builtin_main);
    builder.CreateRet(llvm::ConstantInt::getSigned(builder.getInt32Ty(), 0));
}

llvm::Type *IRGen::generate_type(Type type) {
    if (type.kind == Type::Kind::Number)
        return builder.getDoubleTy();

    return builder.getVoidTy();
}
