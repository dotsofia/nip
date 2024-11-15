#include "../include/parser.h"
#include "../include/utility.h"

int get_tok_precedence(TokenKind tok) {
    switch (tok) {
        case TokenKind::Star:
        case TokenKind::Slash:
            return 6;
        case TokenKind::Plus:
        case TokenKind::Minus:
            return 5;
        case TokenKind::LT:
        case TokenKind::GT:
            return 4;
        case TokenKind::EqualEqual:
            return 3;
        case TokenKind::AmpAmp:
            return 2;
        case TokenKind::PipePipe:
            return 1;
        default:
            return -1;
    }
}

// Synchronize on:
// - start of function
// - end of current block
// - end of current statement
// - EOF
void Parser::synchronize() {
    is_ast_incomplete = true;

    // Keep track of which block we are in
    int braces = 0;
    while (true) {
        TokenKind kind = next_token.kind;

        if (kind == TokenKind::LBrace) {
            braces++;
        } else if (kind == TokenKind::RBrace) { // Synchronizes at the of block
            // Reached end of current block
            if (braces == 0)
                break;

            if (braces == 1) {
                advance(); // eat '}'
                break;
            }

            braces--;
        } else if (kind == TokenKind::SemiColon && braces == 0) { // Synchronizes at end of statement
            advance(); // eat ';'
            break;
        } else if (kind == TokenKind::KeywordFn || kind == TokenKind::_EOF)
            break;

        advance();
    }
}

// <function> 
//  ::= 'fn' <identifier> <parameter_list> ':' <type> <block>
std::unique_ptr<FunctionDecl>
Parser::parse_function_decl() {
    SourceLocation location = next_token.location;
    advance(); // eat fn

    match(TokenKind::Identifier, "expected identifier");
    
    std::string identifier = *next_token.value;
    advance(); // eat identifier

    store_result(parameter_list, parse_parameter_list());

    match(TokenKind::Colon, "expected ':'");
    advance(); // eat ':'

    store_result(type, parse_type());   

    match(TokenKind::LBrace, "expected function body");
    store_result(block, parse_block());

    return std::make_unique<FunctionDecl>(location, identifier,
                 *type, std::move(*parameter_list), std::move(block));
}

// <param_decl>
//   ::= <identifier> : <type>
std::unique_ptr<ParamDecl> Parser::parse_param_decl() {
    SourceLocation location = next_token.location;
    
    std::string identifier = *next_token.value;
    advance(); // eat identifier
    
    match(TokenKind::Colon, "Expected ':' after identifier");
    advance(); // eat ':'

    store_result(type, parse_type());

    return std::make_unique<ParamDecl>(location, std::move(identifier),
            std::move(*type));
}

// <type>
//  ::= 'number'
//  |   'void'
//  |   <identifier>
std::optional<Type> Parser::parse_type() {
    TokenKind kind = next_token.kind;

    if (kind == TokenKind::KeywordNumber) {
        advance(); // 'number'
        return Type::Number();
    }

    if (kind == TokenKind::KeywordVoid) {
        advance(); // eat 'void'
        return Type::Void();
    }

    if (kind == TokenKind::Identifier) {
        auto t = Type::Custom(*next_token.value);
        advance(); // eat identifier
        return t;
    }

    log_error(next_token.location, "expected type specifier");
    return std::nullopt;
}

// <block>
// ::= '{'<statement>* '}'
std::unique_ptr<Block> Parser::parse_block() {
    SourceLocation location = next_token.location;
    advance(); // eat '{'
    
    std::vector<std::unique_ptr<Stmt>> statements;
    for (;;) {
        // Parse statements until end of the block;
        if (next_token.kind == TokenKind::RBrace)
            break;

        // If EOF or Function encountered, most likely the block was not closed.
        if (next_token.kind == TokenKind::_EOF || next_token.kind == TokenKind::KeywordFn)
            return log_error(next_token.location, "expected '}' at the end of a block");

        std::unique_ptr<Stmt> stmt = parse_stmt();
        if (!stmt) {
            synchronize();
            continue;
        }

        statements.emplace_back(std::move(stmt));
    }
    
    advance(); // eat '}'

    return std::make_unique<Block>(location, std::move(statements));
}

// <assigment>
//   ::= <decl_ref_expr> '=' <expr>
 
// <var_decl>
//   ::= (':' <type>)? ('=' <expr>)?
std::unique_ptr<VarDecl> Parser::parse_var_decl(bool is_mutable) {
    SourceLocation location = next_token.location;

    std::string identifier = *next_token.value;
    advance(); // eat identifier

    std::optional<Type> type;
    if (next_token.kind == TokenKind::Colon) {
        advance(); // eat ':'
        
        type = parse_type();
        if (!type)
            return nullptr;
    }

    if (next_token.kind != TokenKind::Equal)
        return std::make_unique<VarDecl>(location, identifier, type, !is_mutable);

    advance(); // eat '='
    
    store_result(init, parse_expr());

    return std::make_unique<VarDecl>(location, identifier, type, !is_mutable,
                                     std::move(init));
}

// <decl_stmt>
//   ::= ('let' | 'const') <var_decl> ';'
std::unique_ptr<DeclStmt> Parser::parse_decl_stmt() {
    Token tok = next_token;
    
    advance(); // eat 'let' | 'const'
    
    match(TokenKind::Identifier, "expected identifier");
    store_result(var_decl, parse_var_decl(tok.kind == TokenKind::KeywordConst));

    match(TokenKind::SemiColon, "expected ';' after variable declaration");
    advance(); // eat ';'

    return std::make_unique<DeclStmt>(tok.location, std::move(var_decl));
}

// <return_stmt>
//   ::= 'return' <expr>? ';'
std::unique_ptr<ReturnStmt> Parser::parse_return_stmt() {
    SourceLocation location = next_token.location;
    advance(); // eat 'return'

    std::unique_ptr<Expr> expr;
    // If theres is not a ';' then there should be an expression.
    if (next_token.kind != TokenKind::SemiColon) {
        expr = parse_expr();
        // Failed to parse expression
        if (!expr)
            return nullptr;
    }

    match(TokenKind::SemiColon, "expected ';' at the end of a return statement");
    advance(); // eat ';'

    return std::make_unique<ReturnStmt>(location, std::move(expr));
}

// <if_stmt>
//   ::= 'if' <expr> <block> ('else' (<if_stmt> | <block>))?
std::unique_ptr<IfStmt> Parser::parse_if_stmt() {
    SourceLocation location = next_token.location;
    advance(); // eat 'if'

    store_result(condition, parse_expr());

    match(TokenKind::LBrace, "expected body after if statement");

    store_result(true_block, parse_block());

    if (next_token.kind != TokenKind::KeywordElse)
        return std::make_unique<IfStmt>(location, std::move(condition),
                                        std::move(true_block));

    advance(); // eat 'else'

    std::unique_ptr<Block> false_block;
    if (next_token.kind == TokenKind::KeywordIf) {
        store_result(else_if, parse_if_stmt());

        SourceLocation location = else_if->location;
        std::vector<std::unique_ptr<Stmt>> stmts;
        stmts.emplace_back(std::move(else_if));

        false_block = std::make_unique<Block>(location, std::move(stmts));
    } else {
        match(TokenKind::LBrace, "expected block after else statement");
        false_block = parse_block();
    }

    if (!false_block)
        return nullptr;

    return std::make_unique<IfStmt>(location, std::move(condition),
                                    std::move(true_block), std::move(false_block));
}

// <while_stmt>
//   ::= 'while' <expr> <block>
std::unique_ptr<WhileStmt> Parser::parse_while_stmt() {
    SourceLocation location = next_token.location;
    advance(); // eat 'while'

    store_result(condition, parse_expr());

    match(TokenKind::LBrace, "expected block after while statement");
    store_result(body, parse_block());

    return std::make_unique<WhileStmt>(location, std::move(condition),
                                       std::move(body));
}



// <statement>
//   ::= <return_stmt>
//   |   <expr> ';'
//   |   <if_stmt>
std::unique_ptr<Stmt> Parser::parse_stmt() {
    if (next_token.kind == TokenKind::KeywordWhile)
        return parse_while_stmt();
    if (next_token.kind == TokenKind::KeywordIf)
        return parse_if_stmt();
    if (next_token.kind == TokenKind::KeywordReturn)
        return parse_return_stmt();
    if (next_token.kind == TokenKind::KeywordLet || next_token.kind == TokenKind::KeywordConst)
        return parse_decl_stmt();


    return parse_assignment_or_expr();
}

std::unique_ptr<Stmt> Parser::parse_assignment_or_expr() {
    store_result(lhs, parse_prefix_expr());

    if (next_token.kind != TokenKind::Equal) {
        // When a '=' is not found treats the prefix as part of a more complex expression.
        store_result(expr, parse_expr_RHS(std::move(lhs), 0));

        match(TokenKind::SemiColon, "expected ';' at the end of expression");
        advance(); // eat ';'

        return expr;
    }

    // If there is a '=' lhs should be the lhs of an assignment
    auto *dre = dynamic_cast<DeclRefExpr *>(lhs.get());
    if (!dre)
        return log_error(lhs->location,
                         "expected variable on the LHS of an assignment");

    std::ignore = lhs.release();

    store_result(assignment,
                 parse_assignment_rhs(std::unique_ptr<DeclRefExpr>(dre)));

    match(TokenKind::SemiColon, "expected ';' at the end of assignment");
    advance(); // eat ';'
    
    return assignment;
}

std::unique_ptr<Assignment>
Parser::parse_assignment_rhs(std::unique_ptr<DeclRefExpr> lhs) {
    SourceLocation location = next_token.location;
    advance(); // eat '='

    store_result(rhs, parse_expr());

    return std::make_unique<Assignment>(location, std::move(lhs), std::move(rhs));
}

std::unique_ptr<Expr> Parser::parse_expr_RHS(std::unique_ptr<Expr> lhs,
                                             int precedence) {
    while (true) {
        TokenKind op = next_token.kind;
        int current_precedence = get_tok_precedence(op);

        if (current_precedence < precedence)
            return lhs;
        advance(); // eat operator

        store_result(rhs, parse_prefix_expr());

        if (current_precedence < get_tok_precedence(next_token.kind)) {
            // We add 1 to the precedence to indicate left-associativity
            rhs = parse_expr_RHS(std::move(rhs), precedence + 1);
            if (!rhs) {
                return nullptr;
            }
        }

        lhs = std::make_unique<BinaryOP>(lhs->location, std::move(lhs),
                                          std::move(rhs), op);
    }
}

// <prefixExpression>
//  ::= ('!' | '-')* <primaryExpr>
std::unique_ptr<Expr> Parser::parse_prefix_expr() {
    Token tok = next_token;

    if (tok.kind != TokenKind::Minus && tok.kind != TokenKind::NEQ)
        return parse_primary();
    advance(); // eat '-' or '!'

    store_result(rhs, parse_prefix_expr());

    return std::make_unique<UnaryOP>(tok.location, std::move(rhs),
                                           tok.kind);
}

std::unique_ptr<Expr> Parser::parse_expr() {
    store_result(lhs, parse_prefix_expr());
    return parse_expr_RHS(std::move(lhs), 0);
}

// <primary_expression>
//   ::= <number_literal>
//   |   <decl_ref_expr>
//   |   <call_expr>
//   |   '(' <expr> ')'
// <number_literal>
//   ::= <number>
//
// <decl_ref_expr>
//   ::= <identifier>
//
// <call_expr>
//   ::= <decl_ref_expr> <args_list>
std::unique_ptr<Expr> Parser::parse_primary() {
    SourceLocation location = next_token.location;

    if (next_token.kind == TokenKind::LParen) {
        advance(); // eat '('

        store_result(expr, parse_expr());

        match(TokenKind::RParen, "expected ')'");
        advance(); // eat ')'

        return std::make_unique<GroupingExpr>(location, std::move(expr));
    }

    if (next_token.kind == TokenKind::Number) {
        auto num = std::make_unique<NumberLiteral>(location, *next_token.value);
        advance(); // eat number
        return num;
    }

    if (next_token.kind == TokenKind::Identifier) {
        auto decl_ref = std::make_unique<DeclRefExpr>(location, *next_token.value);
        advance(); // eat identifier

        // The declaration references a variable
        if (next_token.kind != TokenKind::LParen)
            return decl_ref;

        location = next_token.location;

        store_result(argument_list, parse_argument_list());

        return std::make_unique<CallExpr>(location, std::move(decl_ref),
                std::move(*argument_list));
    }

    return log_error(location, "expected expression");
}

// <args_list>
//   ::= '(' (<expr> (',' <expr>)* ','?)? ')'
std::optional<Parser::ArgumentList> Parser::parse_argument_list() {
    if (next_token.kind != TokenKind::LParen) {
        log_error(next_token.location, "expected '('");
        return std::nullopt;
    }
    advance(); // eat '('
    
    std::vector<std::unique_ptr<Expr>> argument_list;

    if (next_token.kind == TokenKind::RParen) {
        advance(); // eat ')'
        return argument_list;
    }

    for (;;) {
        auto expr = parse_expr();
        if (!expr) 
            return std::nullopt;
        argument_list.emplace_back(std::move(expr));

        // End of argument list
        if (next_token.kind != TokenKind::Comma)
            break;
        advance(); // eat ','
    }

    if (next_token.kind != TokenKind::RParen) {
        log_error(next_token.location, "expected ')'");
        return std::nullopt;
    }
    advance(); // eat ')'

    return argument_list;
}

// <param_list>
//   ::= '(' (<param_decl> (, <param_decl>)* ','?)? ')'
std::optional<Parser::ParameterList> Parser::parse_parameter_list() {
    if (next_token.kind != TokenKind::LParen) {
        log_error(next_token.location, "expected '('");
        return std::nullopt;
    }
    advance(); // eat '('
    
    std::vector<std::unique_ptr<ParamDecl>> argument_list;

    if (next_token.kind == TokenKind::RParen) {
        advance(); // eat ')'
        return argument_list;
    }

    for (;;) {
        if (next_token.kind != TokenKind::Identifier) {
            log_error(next_token.location, "expected paramater declaration");
            return std::nullopt;
        }

        auto param_decl = parse_param_decl();
        if (!param_decl)
            return std::nullopt;
        argument_list.emplace_back(std::move(param_decl));

        // End of argument list
        if (next_token.kind != TokenKind::Comma)
            break;
        advance(); // eat ','
    }

    if (next_token.kind != TokenKind::RParen) {
        log_error(next_token.location, "expected ')'");
        return std::nullopt;
    }
    advance(); // eat ')'

    return argument_list;
}

// <nip_program> ::= <function_decl>* EOF
std::pair<std::vector<std::unique_ptr<FunctionDecl>>, bool>
Parser::parse_file() {
    std::vector<std::unique_ptr<FunctionDecl>> functions;
    
    while (next_token.kind != TokenKind::_EOF) {
        if (next_token.kind != TokenKind::KeywordFn) {
            log_error(next_token.location,
                    "only functions declaration are allowed on the top level");
            // Continue parsing on invalid token
            synchronize_on(TokenKind::KeywordFn);
            continue;
        }

        auto fn = parse_function_decl();
        if (!fn) {
            // Ignore invalid function
            synchronize_on(TokenKind::KeywordFn);
            continue;
        }

        functions.emplace_back(std::move(fn));
    }

    bool has_main = false;
    for (auto &&fn : functions)
        has_main |= fn->identifier == "main"; 

    // Only report missing entry point if the ast is complete.
    if (!has_main && !is_ast_incomplete)
        log_error(next_token.location, "main function not found");

    return {std::move(functions), !is_ast_incomplete && has_main};
}
