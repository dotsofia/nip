#ifndef _NIP_CONSTEXPR_H
#define _NIP_CONSTEXPR_H

#include <optional>

#include "ast.h"

class ConstExprEval {
    std::optional<double>
    evaluate_binary_op(const DecoratedBinaryOP &binop,
                       bool allow_side_effects);
    std::optional<double> evaluate_unary_op(const DecoratedUnaryOP &op,
                                            bool allow_side_effects);
    std::optional<double> evaluate_decl_ref_expr(const DecoratedDeclRefExpr &dre,
                                                bool allow_side_effects);

public:
    std::optional<double> evaluate(const DecoratedExpr &expr,
                                   bool allow_side_effects);
};

#endif
