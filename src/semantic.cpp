#include <cassert>

#include "../include/semantic.h"

std::pair<DecoratedDecl *, int> Semantic::search_decl(const std::string id) {
    int idx = 0;
    for (auto it = scopes.rbegin(); it != scopes.rend(); it++) {
        // Search for every declaration in the current scope
        for (auto &&decl : *it) {
            if (decl->identifier == id)
                return {decl, idx};
        }
        ++idx;
    }
    return {nullptr, -1};
}

bool Semantic::insert_to_current_scope(DecoratedDecl &decl) {
    const auto &[found_decl, scope_idx] = search_decl(decl.identifier);

    // Declaration found in the inner-most scope, report error
    if (found_decl && scope_idx == 0) {
        log_error(decl.location, "redeclaration of '" + decl.identifier + '\'');
        return false;
    }
    
    scopes.back().emplace_back(&decl);
    return true;
}

std::optional<Type> Semantic::resolve_type(Type parsed_type) {
    if (parsed_type.kind == Type::Kind::Custom)
        return std::nullopt;

    return parsed_type;
}

std::unique_ptr<DecoratedParamDecl>
Semantic::decorate_param_decl(const ParamDecl &param) {
    std::optional<Type> type = resolve_type(param.type);

    if (!type || type->kind == Type::Kind::Void)
        return log_error(param.location, "parameter '" + param.identifier + 
                "' has invalid '" + param.type.name + "' type");

    return std::make_unique<DecoratedParamDecl>(param.location, param.identifier,
            *type);
}

std::unique_ptr<DecoratedFunctionDecl>
Semantic::decorate_function_decl(const FunctionDecl &function) {
    std::optional<Type> type = resolve_type(function.type);    

    if (!type)
        return log_error(function.location, "function '" + function.identifier +
                "' has invalid '" + function.type.name + "' type");

    // The main function has special restrictions
    if (function.identifier == "main") {
        if (function.type.kind != Type::Kind::Void)
            return log_error(function.location, "'main' function expected to have return type 'void'");
        if (!function.params.empty())
            return log_error(function.location, "'main' function expected to take no arguments");
    }

    // Resolve parameters
    std::vector<std::unique_ptr<DecoratedParamDecl>> decorated_params;

    Scope param(this);
    for (auto &param : function.params) {
        auto decorated_param = decorate_param_decl(*param);

        if (!decorated_param || !insert_to_current_scope(*decorated_param))
            return nullptr;

        decorated_params.emplace_back(std::move(decorated_param));
    }

    // We don't care about the body for now
    return std::make_unique<DecoratedFunctionDecl>(
            function.location, function.identifier, *type, std::move(decorated_params), nullptr);
}

std::unique_ptr<DecoratedDeclRefExpr>
Semantic::decorate_decl_ref_expr(const DeclRefExpr &decl_ref_expr, bool is_callee) {
    DecoratedDecl *decl = search_decl(decl_ref_expr.identifier).first;
    if (!decl) {
        return log_error(decl_ref_expr.location,
                "symbol '" + decl_ref_expr.identifier + "' not found");
    }

    if (!is_callee && dynamic_cast<DecoratedFunctionDecl*>(decl))
        return log_error(decl_ref_expr.location,
                "expected call to function '" + decl_ref_expr.identifier + "'");

    return std::make_unique<DecoratedDeclRefExpr>(decl_ref_expr.location, *decl);
}

std::unique_ptr<DecoratedCallExpr>
Semantic::decorate_call_expr(const CallExpr &call) {
    auto *dre = dynamic_cast<const DeclRefExpr*>(call.identifier.get());

    if (!dre)
        return log_error(call.location, "expression is not a function");

    store_result(decorated_callee, decorate_decl_ref_expr(*call.identifier, true));

    const auto *decorated_function_decl =
        dynamic_cast<const DecoratedFunctionDecl*>(decorated_callee->decl);

    if (!decorated_function_decl)
        return log_error(call.location, "symbol is not a function");

    if (call.arguments.size() != decorated_function_decl->params.size())
        return log_error(call.location, "mismatch occurred in function call");
    
    std::vector<std::unique_ptr<DecoratedExpr>> decorated_args;
    int idx = 0;
    for (auto &&arg : call.arguments) {
        store_result(decorated_arg, decorate_expr(*arg));

        if (decorated_arg->type.kind != decorated_function_decl->params[idx]->type.kind)
            return log_error(decorated_arg->location, "argument type does not match function paramater type");

        idx++;
        decorated_args.emplace_back(std::move(decorated_arg));
    }

    return std::make_unique<DecoratedCallExpr>(
            call.location, *decorated_function_decl, std::move(decorated_args));
}

std::unique_ptr<DecoratedUnaryOP>
Semantic::decorate_unary_op(const UnaryOP &unary) {
    store_result(decorated_rhs, decorate_expr(*unary.operand));

    if (decorated_rhs->type.kind == Type::Kind::Void)
        return log_error(
                decorated_rhs->location,
                "void expression cannot be used as operand for unary operator");

    return std::make_unique<DecoratedUnaryOP>(unary.location, std::move(decorated_rhs),
                                              unary.op);
}

std::unique_ptr<DecoratedBinaryOP>
Semantic::decorate_binary_op(const BinaryOP &binary) {
    store_result(decorated_lhs, decorate_expr(*binary.lhs));
    store_result(decorated_rhs, decorate_expr(*binary.rhs));

    if (decorated_lhs->type.kind == Type::Kind::Void)
        return log_error(
                decorated_lhs->location,
                "void expression cannot be used as LHS for binary operator");
    if (decorated_rhs->type.kind == Type::Kind::Void)
        return log_error(
                decorated_lhs->location,
                "void expression cannot be used as RHS for binary operator");

    // TODO: Remember to check the type compatibility when other types are implemented
    return std::make_unique<DecoratedBinaryOP>(
        binary.location, std::move(decorated_lhs), std::move(decorated_rhs), binary.op);
}

std::unique_ptr<DecoratedGroupingExpr>
Semantic::decorate_grouping_expr(const GroupingExpr &gexpr) {
    store_result(decorated_expr, decorate_expr(*gexpr.expr));
    return std::make_unique<DecoratedGroupingExpr>(gexpr.location,
                                                   std::move(decorated_expr));
}

std::unique_ptr<DecoratedExpr> Semantic::decorate_expr(const Expr &expr) {
    if (const auto *number = dynamic_cast<const NumberLiteral*>(&expr))
        return std::make_unique<DecoratedNumberLiteral>(number->location,
                std::stod(number->value));

    if (const auto *decl_ref_expr = dynamic_cast<const DeclRefExpr*>(&expr))
        return decorate_decl_ref_expr(*decl_ref_expr);

    if (const auto *call_expr = dynamic_cast<const CallExpr*>(&expr))
        return decorate_call_expr(*call_expr);

    if (const auto *unary_op = dynamic_cast<const UnaryOP*>(&expr))
        return decorate_unary_op(*unary_op);

    if (const auto *binary_op = dynamic_cast<const BinaryOP*>(&expr))
        return decorate_binary_op(*binary_op);

    if (const auto *gexpr = dynamic_cast<const GroupingExpr*>(&expr))
        return decorate_grouping_expr(*gexpr);

    assert(false && "unexpected expression");
    return nullptr;
}

std::unique_ptr<DecoratedReturnStmt>
Semantic::decorate_return_stmt(const ReturnStmt &rstmt) {
    // Tried returning expression on void function
    if (current_function->type.kind == Type::Kind::Void && rstmt.expr)
        return log_error(rstmt.location, "unexpected return type from void function");
    // Non-void function without a return expr
    if (current_function->type.kind != Type::Kind::Void && !rstmt.expr) {
        return log_error(rstmt.location, "expected return value");
    }

    std::unique_ptr<DecoratedExpr> decorated_expr;
    if (rstmt.expr) {
        decorated_expr = decorate_expr(*rstmt.expr);
        if (!decorated_expr)
            return nullptr;

        if (current_function->type.kind != decorated_expr->type.kind) {
            log_error(decorated_expr->location, "invalid return type");
        }
    }

    return std::make_unique<DecoratedReturnStmt>(rstmt.location,
						 std::move(decorated_expr));
}

std::unique_ptr<DecoratedStmt> Semantic::decorate_stmt(const Stmt &stmt) {
    if (auto *expr = dynamic_cast<const Expr *>(&stmt))
        return decorate_expr(*expr);

    if (auto *return_stmt = dynamic_cast<const ReturnStmt *>(&stmt))
        return decorate_return_stmt(*return_stmt);

    // If statement doesn't match any of those, this should be unreachable
    assert(false && "unexpected statement");
    return nullptr;
}

std::unique_ptr<DecoratedBlock> Semantic::decorate_block(const Block &block) {
    std::vector<std::unique_ptr<DecoratedStmt>> decorated_statements;

    bool had_error = false;
    int unreachable_count = 0;
    for (auto &&stmt : block.statements) {
        auto decorated_stmt = decorate_stmt(*stmt);

        had_error |= !decorated_statements.emplace_back(std::move(decorated_stmt));
        if (had_error)
            continue;
        // Only report the first ureachable statement.
        if (unreachable_count == 1) {
            log_error(stmt->location, "unreachable statement", true);
            unreachable_count++;
        }

        if (dynamic_cast<ReturnStmt*>(stmt.get())) {
            unreachable_count++;
        }
    }

    if (had_error)
        return nullptr;

    return std::make_unique<DecoratedBlock>(block.location, std::move(decorated_statements));
}

std::vector<std::unique_ptr<DecoratedFunctionDecl>> Semantic::decorate_ast() {
    Scope global{this};
    std::vector<std::unique_ptr<DecoratedFunctionDecl>> dast;

    // Insert the builtin functions to the global scope and the Decorated AST
    auto display = create_builtin_display();
    insert_to_current_scope(*dast.emplace_back(std::move(display)));
    bool had_error = false;
    for (auto &&fn : ast) {
        auto decorated_function_decl = decorate_function_decl(*fn);

        if (!decorated_function_decl || 
            !insert_to_current_scope(*decorated_function_decl)) {
            had_error = true;
            continue;
        }
        
        dast.emplace_back(std::move(decorated_function_decl));
    }

    if (had_error) return {};

    // Decorate every function in the resolved tree besides the builtin functions
    for (size_t i = 1; i < dast.size(); i++) {
        Scope param(this);
        current_function = dast[i].get();    

        for (auto &&param : current_function->params)
            insert_to_current_scope(*param);

        auto decorated_body = decorate_block(*ast[i - 1]->body);
        if (!decorated_body) {
            had_error = true;
            continue;
        }

        current_function->body = std::move(decorated_body);
    }

    if (had_error) return {};
    return dast;
}

std::unique_ptr<DecoratedFunctionDecl> Semantic::create_builtin_display() {
    SourceLocation location{"<builtin>", 0, 0};

    auto param = std::make_unique<DecoratedParamDecl>(location, "n", Type::Number());

    std::vector<std::unique_ptr<DecoratedParamDecl>> params;
    params.emplace_back(std::move(param));

    auto body = std::make_unique<DecoratedBlock>(
						location, std::vector<std::unique_ptr<DecoratedStmt>>());

    return std::make_unique<DecoratedFunctionDecl>(
            location, "display", Type::Void(), std::move(params), std::move(body));
}
