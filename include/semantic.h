#ifndef _SEMANTIC_NIP_H
#define _SEMANTIC_NIP_H

#include <memory>
#include <optional>
#include <vector>

#include "ast.h"
#include "constexpr.h"
#include "cfg.h"

class Semantic {
    ConstExprEval cee;
    std::vector<std::unique_ptr<FunctionDecl>> ast;
    std::vector<std::vector<DecoratedDecl *>> scopes;

    DecoratedFunctionDecl *current_function;

    // Helper class for creating scopes
    class Scope {
        Semantic *sema;

    public:
        Scope(Semantic *sema) : sema(sema) { sema->scopes.emplace_back(); }
        ~Scope() { sema->scopes.pop_back(); }
    };

    std::pair<DecoratedDecl *, int> search_decl(const std::string id);
    bool insert_to_current_scope(DecoratedDecl &decl);
    std::unique_ptr<DecoratedFunctionDecl> create_builtin_display();
    bool check_variable_initialization(const DecoratedFunctionDecl &fn,
                                       const CFG &cfg);

    std::optional<Type> resolve_type(Type parsed_type);

    bool run_flow_checks(const DecoratedFunctionDecl &fn);
    bool check_return_on_all_paths(const DecoratedFunctionDecl &fn,
                                         const CFG &cfg);

    std::unique_ptr<DecoratedDeclRefExpr>
    decorate_decl_ref_expr(const DeclRefExpr &decl_ref_expr, bool is_callee = false);
    std::unique_ptr<DecoratedCallExpr> decorate_call_expr(const CallExpr &call);
    std::unique_ptr<DecoratedExpr> decorate_expr(const Expr &expr);
    std::unique_ptr<DecoratedAssignment> decorate_assignment(const Assignment &assignment);

    std::unique_ptr<DecoratedBlock> decorate_block(const Block &block);

    std::unique_ptr<DecoratedStmt> decorate_stmt(const Stmt &stmt);
    std::unique_ptr<DecoratedReturnStmt> decorate_return_stmt(const ReturnStmt &rtst);
    std::unique_ptr<DecoratedIfStmt> decorate_if_stmt(const IfStmt &if_stmt);
    std::unique_ptr<DecoratedWhileStmt> decorate_while_stmt(const WhileStmt &while_stmt);
    std::unique_ptr<DecoratedDeclStmt> decorate_decl_stmt(const DeclStmt &decl_stmt);

    std::unique_ptr<DecoratedParamDecl> decorate_param_decl(const ParamDecl &param);
    std::unique_ptr<DecoratedFunctionDecl>
    decorate_function_decl(const FunctionDecl &function);
    std::unique_ptr<DecoratedVarDecl> decorate_var_decl(const VarDecl &var_decl);

    std::unique_ptr<DecoratedUnaryOP> decorate_unary_op(const UnaryOP &unary);
    std::unique_ptr<DecoratedBinaryOP> decorate_binary_op(const BinaryOP &binary);
    std::unique_ptr<DecoratedGroupingExpr> decorate_grouping_expr(const GroupingExpr &gexpr);

public:
    Semantic(std::vector<std::unique_ptr<FunctionDecl>> ast)
        : ast(std::move(ast)) {}

    std::vector<std::unique_ptr<DecoratedFunctionDecl>> decorate_ast();
};

#endif
