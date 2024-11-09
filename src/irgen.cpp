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

llvm::Function *IRGen::get_function() {
    return builder.GetInsertBlock()->getParent();
};

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

    return tmp.CreateAlloca(tmp.getDoubleTy(), nullptr, identifier);
}

void IRGen::generate_conditional_op(const DecoratedExpr &op,
                                    llvm::BasicBlock *true_block,
                                    llvm::BasicBlock *false_block) {
    const auto *binop = dynamic_cast<const DecoratedBinaryOP *>(&op);

    if (binop && binop->op == TokenKind::PipePipe) {
        llvm::BasicBlock *next_block = 
            llvm::BasicBlock::Create(context, "or.lhs.false", true_block->getParent());
        generate_conditional_op(*binop->lhs, true_block, next_block);

        builder.SetInsertPoint(next_block);
        generate_conditional_op(*binop->rhs, true_block, false_block);
        return;
    }

    if (binop && binop->op == TokenKind::AmpAmp) {
        llvm::BasicBlock *next_block = 
            llvm::BasicBlock::Create(context, "and.lhs.true", true_block->getParent());
        generate_conditional_op(*binop->lhs, next_block, false_block);

        builder.SetInsertPoint(next_block);
        generate_conditional_op(*binop->rhs, true_block, false_block);
        return;
    }


    llvm::Value *v = double_to_bool(generate_expr(op));
    builder.CreateCondBr(v, true_block, false_block);
    return;
}

llvm::Value *IRGen::generate_call_expr(const DecoratedCallExpr &call) {
    llvm::Function *callee = module->getFunction(call.callee->identifier);

    std::vector<llvm::Value *> args;
    for (auto &&arg : call.arguments)
    args.emplace_back(generate_expr(*arg));

    return builder.CreateCall(callee, args);
}

llvm::Value *IRGen::generate_unary_op(const DecoratedUnaryOP &unop) {
    llvm::Value *rhs = generate_expr(*unop.operand);

    if (unop.op == TokenKind::Minus)
        return builder.CreateFNeg(rhs);
    if (unop.op == TokenKind::NEQ)
        return bool_to_double(builder.CreateNot(double_to_bool(rhs)));

    llvm_unreachable("unknown unary op");
    return nullptr;
}

llvm::Value *IRGen::generate_binary_op(const DecoratedBinaryOP &binop) {
    TokenKind op = binop.op;

    if (op == TokenKind::AmpAmp || op == TokenKind::PipePipe) {
        llvm::Function *function = get_function();
        bool is_or = op == TokenKind::PipePipe;

        auto *rhs_tag = is_or ? "or.rhs" : "and.rhs";
        auto *merge_tag = is_or ? "or.merge" : "and.merge";

        auto rhs = llvm::BasicBlock::Create(context, rhs_tag, function);
        auto merge = llvm::BasicBlock::Create(context, merge_tag, function);

        llvm::BasicBlock *false_block = is_or ? rhs : merge;
        llvm::BasicBlock *true_block = is_or ? merge : rhs;
        generate_conditional_op(*binop.lhs, true_block, false_block);


        builder.SetInsertPoint(rhs);
        llvm::Value *rhs_v = double_to_bool(generate_expr(*binop.rhs));
        builder.CreateBr(merge);

        rhs = builder.GetInsertBlock();
        builder.SetInsertPoint(merge);
        llvm::PHINode *phi = builder.CreatePHI(builder.getInt1Ty(), 2);

        for (auto it = pred_begin(merge); it != pred_end(merge); ++it) {
            if (*it == rhs)
                phi->addIncoming(rhs_v, rhs);
            else 
                phi->addIncoming(builder.getInt1(is_or), *it);
        }

        return bool_to_double(phi);
    }

    llvm::Value *lhs = generate_expr(*binop.lhs);
    llvm::Value *rhs = generate_expr(*binop.rhs);

    if (op == TokenKind::Plus)
        return builder.CreateFAdd(lhs, rhs);

    if (op == TokenKind::Minus)
        return builder.CreateFSub(lhs, rhs);

    if (op == TokenKind::Star)
        return builder.CreateFMul(lhs, rhs);

    if (op == TokenKind::Slash)
        return builder.CreateFDiv(lhs, rhs);

    if (op == TokenKind::EqualEqual)
        return bool_to_double(builder.CreateFCmpOEQ(lhs, rhs));

    if (op == TokenKind::LT)
        return bool_to_double(builder.CreateFCmpOLT(lhs, rhs));

    if (op == TokenKind::GT)
        return bool_to_double(builder.CreateFCmpOGT(lhs, rhs));

    llvm_unreachable("unexpected binary operator");
    return nullptr;
}

llvm::Value *IRGen::generate_expr(const DecoratedExpr &expr) {
    if (auto val = expr.get_value())
        return llvm::ConstantFP::get(builder.getDoubleTy(), *val);

    if (auto *number = dynamic_cast<const DecoratedNumberLiteral *>(&expr))
        return llvm::ConstantFP::get(builder.getDoubleTy(), number->value);

    if (auto *dre = dynamic_cast<const DecoratedDeclRefExpr *>(&expr))
        return builder.CreateLoad(builder.getDoubleTy(), declarations[dre->decl]);

    if (auto *call = dynamic_cast<const DecoratedCallExpr *>(&expr))
        return generate_call_expr(*call);

    if (auto *unop = dynamic_cast<const DecoratedUnaryOP *>(&expr))
        return generate_unary_op(*unop);

    if (auto *binop = dynamic_cast<const DecoratedBinaryOP *>(&expr))
        return generate_binary_op(*binop);

    if (auto *grouping = dynamic_cast<const DecoratedGroupingExpr *>(&expr))
        return generate_expr(*grouping->expr);

    llvm_unreachable("unexpected expression");
}

llvm::Value *IRGen::generate_return_stmt(const DecoratedReturnStmt &stmt) {
    if (stmt.expr)
        builder.CreateStore(generate_expr(*stmt.expr), ret_val);

    return builder.CreateBr(ret_block);
}

llvm::Value *IRGen::generate_if_stmt(const DecoratedIfStmt &stmt) {
    llvm::Function *function = get_function();

    auto *true_block = llvm::BasicBlock::Create(context, "if.true");
    auto *exit_block = llvm::BasicBlock::Create(context, "if.exit");

    llvm::BasicBlock *else_block = exit_block;
    if (stmt.false_block)
        else_block = llvm::BasicBlock::Create(context, "if.false");

    llvm::Value *condition = generate_expr(*stmt.condition);
    builder.CreateCondBr(double_to_bool(condition), true_block, else_block);

    true_block->insertInto(function);
    builder.SetInsertPoint(true_block);
    generate_block(*stmt.true_block);
    builder.CreateBr(exit_block);

    if (stmt.false_block) {
        else_block->insertInto(function);

        builder.SetInsertPoint(else_block);
        generate_block(*stmt.false_block);
        builder.CreateBr(exit_block);
    }

    exit_block->insertInto(function);
    builder.SetInsertPoint(exit_block);
    return nullptr;
}

llvm::Value *IRGen::generate_while_stmt(const DecoratedWhileStmt &stmt) {
    llvm::Function *function = get_function();

    auto *header = llvm::BasicBlock::Create(context, "while.header", function);
    auto *body = llvm::BasicBlock::Create(context, "while.body", function);
    auto *exit = llvm::BasicBlock::Create(context, "while.exit", function);

    builder.CreateBr(header);

    builder.SetInsertPoint(header);
    llvm::Value *cond = generate_expr(*stmt.condition);
    builder.CreateCondBr(double_to_bool(cond), body, exit);

    builder.SetInsertPoint(body);
    generate_block(*stmt.body);
    builder.CreateBr(header);

    builder.SetInsertPoint(exit);
    return nullptr;
}

llvm::Value *IRGen::generate_decl_stmt(const DecoratedDeclStmt &stmt) {
    llvm::Function *function = get_function();
    const auto *decl = stmt.var_decl.get();

    llvm::AllocaInst *var = allocate_stack_variable(function, decl->identifier);

    if (const auto &init = decl->init)
        builder.CreateStore(generate_expr(*init), var);

    declarations[decl] = var;
    return nullptr;
}

llvm::Value *IRGen::generate_stmt(const DecoratedStmt &stmt) {
    if (auto *expr = dynamic_cast<const DecoratedExpr *>(&stmt))
        return generate_expr(*expr);

    if (auto *rstmt = dynamic_cast<const DecoratedReturnStmt *>(&stmt))
        return generate_return_stmt(*rstmt);

    if (auto *if_stmt = dynamic_cast<const DecoratedIfStmt *>(&stmt))
        return generate_if_stmt(*if_stmt);

    if (auto *while_stmt = dynamic_cast<const DecoratedWhileStmt *>(&stmt))
        return generate_while_stmt(*while_stmt);

    if (auto *decl_stmt = dynamic_cast<const DecoratedDeclStmt *>(&stmt))
        return generate_decl_stmt(*decl_stmt);

    if (auto *assignment = dynamic_cast<const DecoratedAssignment *>(&stmt))
        return generate_assignment(*assignment);

    llvm_unreachable("unknown statement");
    return nullptr;
}

llvm::Value *IRGen::generate_assignment(const DecoratedAssignment &stmt) {
    return builder.CreateStore(generate_expr(*stmt.expr),
                               declarations[stmt.variable->decl]);
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

llvm::Value *IRGen::double_to_bool(llvm::Value *v) {
    return builder.CreateFCmpONE(
        v, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), "to.bool");
}

llvm::Value *IRGen::bool_to_double(llvm::Value *v) {
    return builder.CreateUIToFP(v, builder.getDoubleTy(), "to.double");
}
