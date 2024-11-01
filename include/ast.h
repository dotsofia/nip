#ifndef _NIP_AST_H
#define _NIP_AST_H

#include <cstdio>
#include <iostream>
#include <memory>

#include "utility.h"

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
            expr->print();
    }
};

struct NumberLiteral : public Expr {
    std::string value;

    NumberLiteral(SourceLocation location, std::string value)
        : Expr(location),
          value(value) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "NumberLiteral: '" << value << "\n'";
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

struct CallExpr : public Expr {
    std::unique_ptr<Expr> callee;
    std::vector<std::unique_ptr<Expr>> arguments;

    CallExpr(SourceLocation location,
             std::unique_ptr<Expr> callee,
             std::vector<std::unique_ptr<Expr>> arguments)
        : Expr(location),
          callee(std::move(callee)),
          arguments(std::move(arguments)) {}

    void print(size_t level = 0) const override {
        std::cerr << indent(level) << "CallExpr:\n";
        
        callee->print(level + 1);

        for (auto &&arg : arguments)
            arg->print(level + 1);
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

#endif 
