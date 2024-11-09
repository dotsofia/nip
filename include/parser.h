#ifndef _NIP_PARSER_H
#define _NIP_PARSER_H

#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include "ast.h"
#include "lexer.h"

class Parser {
    Lexer *lexer;
    Token next_token;
    bool is_ast_incomplete = false; // AST may be incomplete and we need to report back or recover.

    // Consumes the token and returns the folowing one.
    void advance() { next_token = lexer->get_next_token(); }
    void synchronize();
    void synchronize_on(TokenKind kind) {
        is_ast_incomplete = true;

        // Consumes token until the next synchronization token.
        while (next_token.kind != kind && next_token.kind != TokenKind::_EOF)
            advance();
    }

    // Methods to parse the AST
    std::unique_ptr<FunctionDecl> parse_function_decl();
    std::unique_ptr<ParamDecl> parse_param_decl();
    std::unique_ptr<VarDecl> parse_var_decl(bool is_mutable);

    std::unique_ptr<Stmt> parse_stmt();
    std::unique_ptr<Stmt> parse_assignment_or_expr();
    std::unique_ptr<Assignment> parse_assignment_rhs(std::unique_ptr<DeclRefExpr> lhs);

    std::unique_ptr<ReturnStmt> parse_return_stmt();
    std::unique_ptr<IfStmt> parse_if_stmt();
    std::unique_ptr<WhileStmt> parse_while_stmt();
    std::unique_ptr<DeclStmt> parse_decl_stmt();

    std::unique_ptr<Block> parse_block();

    std::unique_ptr<Expr> parse_expr();
    std::unique_ptr<Expr> parse_primary();

    std::unique_ptr<Expr> parse_expr_RHS(std::unique_ptr<Expr> lhs, int precedence);
    std::unique_ptr<Expr> parse_prefix_expr();

    // Helpers
    typedef std::vector<std::unique_ptr<Expr>> ArgumentList;
    std::optional<ArgumentList> parse_argument_list();

    typedef std::vector<std::unique_ptr<ParamDecl>> ParameterList;
    std::optional<ParameterList> parse_parameter_list();

    std::optional<Type> parse_type();

public:
    Parser(Lexer &lexer)
        : lexer(&lexer),
          next_token(lexer.get_next_token()) {}

    // Only functions are allowed on top level for now.
    std::pair<std::vector<std::unique_ptr<FunctionDecl>>, bool> parse_file();
};

#endif
