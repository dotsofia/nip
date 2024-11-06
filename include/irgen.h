#ifndef _NIP_IRGEN_H
#define _NIP_IRGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include <map>
#include <memory>
#include <vector>

#include "ast.h"

class IRGen {
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;

    llvm::Instruction *alloca_insert_point;
    llvm::Value *ret_val = nullptr;
    llvm::BasicBlock *ret_block = nullptr;

    std::vector<std::unique_ptr<DecoratedFunctionDecl>> dast;
    std::map<const DecoratedDecl *, llvm::Value *> declarations;

    llvm::Type *generate_type(Type type);
    llvm::AllocaInst *allocate_stack_variable(llvm::Function *function,
            const std::string_view identifier);

    void generate_block(const  DecoratedBlock &block);
    void generate_function_body(const DecoratedFunctionDecl &function_decl);
    void generate_function_decl(const DecoratedFunctionDecl &function_decl);

    void generate_builtin_display_body(const DecoratedFunctionDecl &function_decl);
    void generate_main_wrapper();
    
    llvm::Value *generate_stmt(const DecoratedStmt &stmt);
    llvm::Value *generate_return_stmt(const DecoratedReturnStmt &stmt);

    llvm::Value *generate_expr(const DecoratedExpr &expr);
    llvm::Value *generate_call_expr(const DecoratedCallExpr &expr);

public:
    explicit IRGen(
      std::vector<std::unique_ptr<DecoratedFunctionDecl>> dast,
      std::string_view src_path);
    
    std::unique_ptr<llvm::Module> generate_ir();
};

#endif
