#ifndef _NIP_AST_H
#define _NIP_AST_H
#include <cassert>
#include <cstdio>
#include <iostream>
#include <memory>
#include <vector>

#include "lexer.h"
#include "utility.h"

static std::string_view dump_op(const TokenKind op) {
    switch (op) {
        case TokenKind::Plus:
            return "+";
        case TokenKind::Minus:
            return "-";
        case TokenKind::Star:
            return "*";
        case TokenKind::Slash:
            return "/";
        case TokenKind::EqualEqual:
            return "==";
        case TokenKind::AmpAmp:
            return "&&";
        case TokenKind::PipePipe:
            return "||";
        case TokenKind::LT:
            return "<";
        case TokenKind::GT:
            return ">";
        case TokenKind::NEQ:
            return "!";
        default:
            assert(false && "unexpected operator");
    }
}

struct Type {
    enum class Kind { Void, Number, Custom };

    Kind kind;
    std::string name;

    static Type Void() { return {Kind::Void, "void"}; }
    static Type Number() { return {Kind::Number, "number"}; }
    static Type Custom(const std::string &name) { return {Kind::Custom, name}; }

private:
    Type(Kind kind, std::string name) : kind(kind), name(std::move(name)) {}
};

// Base class for all declarations
struct Decl : public Printable {
    SourceLocation location;
    std::string identifier;

    Decl(SourceLocation location, std::string identifier) 
        : location(location),
          identifier(std::move(identifier)) {}
    virtual ~Decl() = default;
};

struct Stmt : public Printable {
    SourceLocation location;
    Stmt(SourceLocation location) : location(location) {}

    virtual ~Stmt() = default;
};
    

struct Expr : public Stmt {
    Expr(SourceLocation location) : Stmt(location) {} 
};

// Represents a block of code associated with a function.
struct Block : public Printable {
    SourceLocation location;
    std::vector<std::unique_ptr<Stmt>> statements;

    Block(SourceLocation location, std::vector<std::unique_ptr<Stmt>> statements) 
        : location(location),
          statements(std::move(statements)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "Block\n";

        for (auto &&stmt : statements) 
            stmt->print(level + 1);
    } 
};

struct ReturnStmt : public Stmt {
    std::unique_ptr<Expr> expr;

    ReturnStmt(SourceLocation location, std::unique_ptr<Expr> expr = nullptr)
        : Stmt(location),
          expr(std::move(expr)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "ReturnStmt\n";

        if (expr)
            expr->print(level + 1);
    }
};

struct IfStmt : public Stmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Block> true_block;
    std::unique_ptr<Block> false_block;

    IfStmt(SourceLocation location,
           std::unique_ptr<Expr> condition,
           std::unique_ptr<Block> true_block,
           std::unique_ptr<Block> false_block = nullptr)
        : Stmt(location),
          condition(std::move(condition)),
          true_block(std::move(true_block)),
          false_block(std::move(false_block)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "IfStmt\n";

        condition->print(level + 1);
        true_block->print(level + 1);

        if (false_block)
            false_block->print(level + 1);
    }
};

struct WhileStmt : public Stmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Block> body;

    WhileStmt(SourceLocation location,
              std::unique_ptr<Expr> condition,
              std::unique_ptr<Block> body)
        : Stmt(location),
          condition(std::move(condition)),
          body(std::move(body)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "WhileStmt\n";

        condition->print(level + 1);
        body->print(level + 1);
    }
};

struct NumberLiteral : public Expr {
    std::string value;

    NumberLiteral(SourceLocation location, std::string value)
        : Expr(location),
          value(value) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "NumberLiteral: '" << value << "'\n";
    }
};

// Contains the reference to a variable or function name.
struct DeclRefExpr : public Expr {
    std::string identifier;

    DeclRefExpr(SourceLocation location, std::string identifier)
        : Expr(location),
          identifier(identifier) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DeclRefExpr: " << identifier << '\n';
    }
};

struct Assignment : public Stmt {
    std::unique_ptr<DeclRefExpr> variable;
    std::unique_ptr<Expr> expr;

    Assignment(SourceLocation location,
               std::unique_ptr<DeclRefExpr> variable,
               std::unique_ptr<Expr> expr)
        : Stmt(location),
        variable(std::move(variable)),
        expr(std::move(expr)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "Assignment\n";
        variable->print(level + 1);
        expr->print(level + 1);
    }
};

struct CallExpr : public Expr {
    std::unique_ptr<DeclRefExpr> identifier;
    std::vector<std::unique_ptr<Expr>> arguments;

    CallExpr(SourceLocation location,
             std::unique_ptr<DeclRefExpr> identifier,
             std::vector<std::unique_ptr<Expr>> arguments)
        : Expr(location),
        identifier(std::move(identifier)),
        arguments(std::move(arguments)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "CallExpr:\n";

        identifier->print(level + 1);

        for (auto &&arg : arguments)
        arg->print(level + 1);
    }
};

struct BinaryOP : public Expr {
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
    TokenKind op;

    BinaryOP(SourceLocation location,
             std::unique_ptr<Expr> lhs,
             std::unique_ptr<Expr> rhs,
             TokenKind op)
        : Expr(location),
        lhs(std::move(lhs)),
        rhs(std::move(rhs)),
        op(op) {}

    void print(size_t level = 0) const override {
        std::cerr << indent (level) <<"BinaryOP: '" << dump_op(op) << '\''
            << '\n';

        lhs->print(level + 1);
        rhs->print(level + 1);
    }
};

struct UnaryOP : public Expr {
    std::unique_ptr<Expr> operand;
    TokenKind op;

    UnaryOP(SourceLocation location,
            std::unique_ptr<Expr> operand,
            TokenKind op)
        : Expr(location),
        operand(std::move(operand)),
        op(op) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "UnaryOP: '" << dump_op(op) << '\''
            << '\n';

        operand->print(level + 1);
    }
};

struct GroupingExpr : public Expr {
    std::unique_ptr<Expr> expr;

    GroupingExpr(SourceLocation location, std::unique_ptr<Expr> expr)
    : Expr(location), expr(std::move(expr)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "GroupingExpr:\n";

        expr->print(level + 1);
    }
};

struct VarDecl : public Decl {
    std::optional<Type> type;
    std::unique_ptr<Expr> init;
    bool is_mutable;

    VarDecl(SourceLocation location,
            std::string identifier,
            std::optional<Type> type,
            bool is_mutable,
            std::unique_ptr<Expr> init = nullptr)
        : Decl(location, std::move(identifier)),
        type(std::move(type)),
        init(std::move(init)),
        is_mutable(is_mutable){}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "VarDecl: " << identifier;
        if (type) 
            std::cerr << ':' << type->name;
        std::cerr << '\n';

        if (init)
            init->print(level + 1);
    }
};


struct DeclStmt : public Stmt {
    std::unique_ptr<VarDecl> var_decl;

    DeclStmt(SourceLocation location, std::unique_ptr<VarDecl> var_decl)
    : Stmt(location), var_decl(std::move(var_decl)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DeclStmt:\n";
        var_decl->print(level + 1);
    }
};

struct ParamDecl : public Decl {
    Type type;

    ParamDecl(SourceLocation location, std::string identifier, Type type)
        : Decl(location, std::move(identifier)),
        type(std::move(type)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "ParamDecl: " << identifier << ":" << type.name
            << '\n';
    }
};

struct FunctionDecl : public Decl {
    Type type;
    std::vector<std::unique_ptr<ParamDecl>> params;
    std::unique_ptr<Block> body;

    FunctionDecl(SourceLocation location,
                 std::string identifier,
                 Type type,
                 std::vector<std::unique_ptr<ParamDecl>> params,
                 std::unique_ptr<Block> body)
        : Decl(location, std::move(identifier)),
        type(std::move(type)),
        params(std::move(params)),
        body(std::move(body)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "Function Declaration: " << identifier << ":"
            << type.name << '\n';

        for (auto &&p : params)
        p->print(level + 1);

        body->print(level + 1);
    }
};

struct DecoratedStmt : public Printable {
    SourceLocation location;

    DecoratedStmt(SourceLocation location)
    : location(location) {}

    virtual ~DecoratedStmt() = default;
};

struct DecoratedExpr : public ConstValueContainer<DecoratedExpr, double>,
                       public DecoratedStmt {
    Type type;

    DecoratedExpr(SourceLocation location, Type type)
        : DecoratedStmt(location),
        type(type) {}
};

struct DecoratedNumberLiteral : public DecoratedExpr {
    // All numbers are double precision floating point for now.
    double value;

    DecoratedNumberLiteral(SourceLocation location, double value)
        : DecoratedExpr(location, Type::Number()),
        value(value) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedNumberLiteral: '" << value << "'\n";

        if (auto val = get_value())
            std::cerr << indent(level) << "| value: " << *val << '\n';
    }
};

// The decorated version of the Block is identical since it does not contain
// any references to other nodes or type annotations
struct DecoratedBlock : public Printable {
    SourceLocation location;
    std::vector<std::unique_ptr<DecoratedStmt>> statements;

    DecoratedBlock(SourceLocation location,
                   std::vector<std::unique_ptr<DecoratedStmt>> statements)
        : location(location),
        statements(std::move(statements)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedBlock\n";

        for (auto &&stmt : statements)
        stmt->print(level + 1);
    }
};


struct DecoratedDecl : public Printable {
    SourceLocation location;
    std::string identifier;
    Type type;

    DecoratedDecl(SourceLocation location, std::string identifier, Type type)
        : location(location),
        identifier(std::move(identifier)),
        type(type) {}

    virtual ~DecoratedDecl() = default;
};

struct DecoratedVarDecl : public DecoratedDecl {
    std::unique_ptr<DecoratedExpr> init;
    bool is_mutable;

    DecoratedVarDecl(SourceLocation location,
                     std::string identifier,
                     Type type,
                     bool is_mutable,
                     std::unique_ptr<DecoratedExpr> init = nullptr)
        : DecoratedDecl(location, std::move(identifier), type),
        init(std::move(init)),
        is_mutable(is_mutable) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedVarDecl: @(" << this << ") "
            << identifier << ':' << '\n';

        if (init)
            init->print(level + 1);
    }
};

struct DecoratedDeclStmt : public DecoratedStmt {
    std::unique_ptr<DecoratedVarDecl> var_decl;

    DecoratedDeclStmt(SourceLocation location,
                      std::unique_ptr<DecoratedVarDecl> var_decl)
        : DecoratedStmt(location),
        var_decl(std::move(var_decl)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedDeclStmt:\n";
        var_decl->print(level + 1);
    }
};

struct DecoratedParamDecl : public DecoratedDecl {
    DecoratedParamDecl(SourceLocation location, std::string identifier, Type type)
    : DecoratedDecl{location, std::move(identifier), type} {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedParamDecl: @(" << this << ") "
            << identifier << ':' << '\n';
    }
};

struct DecoratedFunctionDecl : public DecoratedDecl {
    std::vector<std::unique_ptr<DecoratedParamDecl>> params;
    std::unique_ptr<DecoratedBlock> body;

    DecoratedFunctionDecl(SourceLocation location,
                          std::string identifier,
                          Type type,
                          std::vector<std::unique_ptr<DecoratedParamDecl>> params,
                          std::unique_ptr<DecoratedBlock> body)
        : DecoratedDecl(location, std::move(identifier), type),
        params(std::move(params)),
        body(std::move(body)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedFunctionDecl: @(" << this << ") "
            << identifier << ':' << '\n';

        for (auto &&param : params)
        param->print(level + 1);

        body->print(level + 1);
    }
};

struct DecoratedDeclRefExpr : public DecoratedExpr {
    const DecoratedDecl *decl;

    DecoratedDeclRefExpr(SourceLocation location, DecoratedDecl &decl)
        : DecoratedExpr(location, decl.type),
          decl(&decl) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecorateRefExpr: @(" << decl << ") "
            << decl->identifier << '\n';

        if (auto val = get_value())
            std::cerr << indent(level) << "| value: " << *val << '\n';
    }
};

struct DecoratedCallExpr : public DecoratedExpr {
    const DecoratedFunctionDecl *callee;
    std::vector<std::unique_ptr<DecoratedExpr>> arguments;

    DecoratedCallExpr(SourceLocation location,
                      const DecoratedFunctionDecl &callee,
                      std::vector<std::unique_ptr<DecoratedExpr>> arguments)
        : DecoratedExpr(location, callee.type),
        callee(&callee),
        arguments(std::move(arguments)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedCallExpr: @(" << callee << ") "
            << callee->identifier << '\n';

        if (auto val = get_value())
            std::cerr << indent(level) << "| value: " << *val << '\n';

        for (auto &&arg : arguments)
        arg->print(level + 1);
    }
};

struct DecoratedReturnStmt : public DecoratedStmt {
    std::unique_ptr<DecoratedExpr> expr;

    DecoratedReturnStmt(SourceLocation location,
                        std::unique_ptr<DecoratedExpr> expr = nullptr)
        : DecoratedStmt(location),
        expr(std::move(expr)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedReturnStmt\n";

        if (expr)
            expr->print(level + 1);
    }
};

struct DecoratedBinaryOP : public DecoratedExpr {
    std::unique_ptr<DecoratedExpr> lhs;
    std::unique_ptr<DecoratedExpr> rhs;
    TokenKind op;

    DecoratedBinaryOP(SourceLocation location,
                      std::unique_ptr<DecoratedExpr> lhs,
                      std::unique_ptr<DecoratedExpr> rhs,
                      TokenKind op)
        : DecoratedExpr(location, lhs->type),
        lhs(std::move(lhs)),
        rhs(std::move(rhs)),
        op(op) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedBinaryOP: '" << dump_op(op) << '\''
            << '\n';

        if (auto val = get_value())
            std::cerr << indent(level) << "| value: " << *val << '\n';

        lhs->print(level + 1);
        rhs->print(level + 1);
    }
};

struct DecoratedUnaryOP : public DecoratedExpr {
    std::unique_ptr<DecoratedExpr> operand;
    TokenKind op;

    DecoratedUnaryOP(SourceLocation location,
                     std::unique_ptr<DecoratedExpr> operand,
                     TokenKind op)
        : DecoratedExpr(location, operand->type),
        operand(std::move(operand)),
        op(op) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedUnaryOP: '" << dump_op(op) << '\''
            << '\n';

        if (auto val = get_value())
            std::cerr << indent(level) << "| value: " << *val << '\n';

        operand->print(level + 1);
    }
};

struct DecoratedGroupingExpr : public DecoratedExpr {
    std::unique_ptr<DecoratedExpr> expr;

    DecoratedGroupingExpr(SourceLocation location, std::unique_ptr<DecoratedExpr> expr)
        : DecoratedExpr(location, expr->type),
        expr(std::move(expr)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedGroupingExpr:\n";

        if (auto val = get_value())
            std::cerr << indent(level) << "| value: " << *val << '\n';

        expr->print(level + 1);
    }
};

struct DecoratedIfStmt : public DecoratedStmt {
    std::unique_ptr<DecoratedExpr> condition;
    std::unique_ptr<DecoratedBlock> true_block;
    std::unique_ptr<DecoratedBlock> false_block;

    DecoratedIfStmt(SourceLocation location,
                    std::unique_ptr<DecoratedExpr> condition,
                    std::unique_ptr<DecoratedBlock> true_block,
                    std::unique_ptr<DecoratedBlock> false_block = nullptr)
        : DecoratedStmt(location),
        condition(std::move(condition)),
        true_block(std::move(true_block)),
        false_block(std::move(false_block)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedIfStmt\n";

        condition->print(level + 1);
        true_block->print(level + 1);
        if (false_block)
            false_block->print(level + 1);
    }
};

struct DecoratedWhileStmt : public DecoratedStmt {
    std::unique_ptr<DecoratedExpr> condition;
    std::unique_ptr<DecoratedBlock> body;

    DecoratedWhileStmt(SourceLocation location,
                       std::unique_ptr<DecoratedExpr> condition,
                       std::unique_ptr<DecoratedBlock> body)
        : DecoratedStmt(location),
        condition(std::move(condition)),
        body(std::move(body)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedWhileStmt\n";

        condition->print(level + 1);
        body->print(level + 1);
    }
};

struct DecoratedAssignment : public DecoratedStmt {
    std::unique_ptr<DecoratedDeclRefExpr> variable;
    std::unique_ptr<DecoratedExpr> expr;

    DecoratedAssignment(SourceLocation location,
               std::unique_ptr<DecoratedDeclRefExpr> variable,
               std::unique_ptr<DecoratedExpr> expr)
        : DecoratedStmt(location),
        variable(std::move(variable)),
        expr(std::move(expr)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "DecoratedAssignment\n";
        variable->print(level + 1);
        expr->print(level + 1);
    }
};


#endif
