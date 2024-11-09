#include "llvm/Support/ErrorHandling.h"

#include <optional>

#include "../include/constexpr.h" 

static inline std::optional<bool>
to_bool(std::optional<double> v) {
    if (!v)
        return std::nullopt;

    return v != 0.0;
}

std::optional<double>
ConstExprEval::evaluate_binary_op(
     const DecoratedBinaryOP &binop, bool allow_side_effects) {
    std::optional<double> lhs = evaluate(*binop.lhs, allow_side_effects);

    if (!lhs && !allow_side_effects)
        return std::nullopt;

    if (binop.op == TokenKind::PipePipe) {
        if (to_bool(lhs) == true)
            return 1.0;

        std::optional<double> rhs = evaluate(*binop.rhs, allow_side_effects);
        if (to_bool(rhs) == true)
            return 1.0;

        if (lhs && rhs)
            return 0.0;

        return std::nullopt;
    }

    if (binop.op == TokenKind::AmpAmp) {
        if (to_bool(lhs) == false)
            return 0.0;
        
        std::optional<double> rhs = evaluate(*binop.rhs, allow_side_effects);
        if (to_bool(rhs) == false)
            return 0.0;
            
        if (lhs && rhs)
            return 1.0;

        return std::nullopt;
    }
    
    if (!lhs)
        return std::nullopt;

    std::optional<double> rhs = evaluate(*binop.rhs, allow_side_effects);
    if (!rhs)
        return std::nullopt;

    switch (binop.op) {
        case TokenKind::Plus:
            return *lhs + *rhs;
        case TokenKind::Minus:
            return *lhs - *rhs;
        case TokenKind::Star:
            return *lhs * *rhs;
        case TokenKind::Slash:
            return *lhs / *rhs;
        case TokenKind::EqualEqual:
            return *lhs == *rhs;
        case TokenKind::GT:
            return *lhs > *rhs;
        case TokenKind::LT:
            return *lhs < *rhs;
        default:
            llvm_unreachable("unexpected binary operator");
    }
}

std::optional<double>
ConstExprEval::evaluate_unary_op(
    const DecoratedUnaryOP &op, bool allow_side_effects) {
    std::optional<double> operand = evaluate(*op.operand, allow_side_effects);
    if (!operand)
        return std::nullopt;

    if (op.op == TokenKind::NEQ)
        return !*to_bool(operand);

    if (op.op == TokenKind::Minus)
        return -*operand;

    llvm_unreachable("unexpected unary operator");
}

std::optional<double>
ConstExprEval::evaluate_decl_ref_expr(const DecoratedDeclRefExpr &dre, bool allow_side_effects) {
    const auto *res_var = dynamic_cast<const DecoratedVarDecl *>(dre.decl);
    if (!res_var || res_var->is_mutable || !res_var->init)
        return std::nullopt;

    return evaluate(*res_var->init, allow_side_effects);
}

std::optional<double>
ConstExprEval::evaluate(const DecoratedExpr &expr, bool allow_side_effects) {
    // Dont evaluate the same expression more than once
    if (std::optional<double> val = expr.get_value())
        return val;

    if (const auto *num = 
            dynamic_cast<const DecoratedNumberLiteral *>(&expr))
        return num->value;

    if (const auto *gexpr =
            dynamic_cast<const DecoratedGroupingExpr *>(&expr))
        return evaluate(*gexpr->expr, allow_side_effects);

    if (const auto *unop =
            dynamic_cast<const DecoratedUnaryOP *>(&expr))
        return evaluate_unary_op(*unop, allow_side_effects);

    if (const auto *binop =
            dynamic_cast<const DecoratedBinaryOP *>(&expr))
        return evaluate_binary_op(*binop, allow_side_effects);

    if (const auto *decl_ref_expr =
            dynamic_cast<const DecoratedDeclRefExpr *>(&expr))
        return evaluate_decl_ref_expr(*decl_ref_expr, allow_side_effects);

    return std::nullopt;
}
