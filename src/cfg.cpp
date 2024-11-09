#include <cassert>

#include "../include/ast.h"
#include "../include/cfg.h"

bool is_terminator(const DecoratedStmt &stmt) {
    return dynamic_cast<const DecoratedIfStmt *>(&stmt) ||
    dynamic_cast<const DecoratedWhileStmt *>(&stmt) ||
    dynamic_cast<const DecoratedReturnStmt *>(&stmt);
}

void CFG::print(size_t level) const {
    for (int i = basic_blocks.size() - 1; i >= 0; --i) {
        std::cerr << '[' << i;
        if (i == entry)
            std::cerr << " (entry)";
        else if (i == exit)
            std::cerr << " (exit)";
        std::cerr << ']' << '\n';

        std::cerr << "  preds: ";
        for (auto &&[id, reachable] : basic_blocks[i].predecessors)
        std::cerr << id << ((reachable) ? " " : "(U) ");
        std::cerr << '\n';

        std::cerr << "  succs: ";
        for (auto &&[id, reachable] : basic_blocks[i].successors)
        std::cerr << id << ((reachable) ? " " : "(U) ");
        std::cerr << '\n';

        const auto &statements = basic_blocks[i].statements;
        for (auto it = statements.rbegin(); it != statements.rend(); ++it)
            (*it)->print(1);
        std::cerr << '\n';
    }
}

// Insert an if statement with true and false blocks into the CFG.
int CFGBuilder::insert_if_stmt(const DecoratedIfStmt &stmt, int exit) {
    int false_block = exit;
    if (stmt.false_block)
        false_block = insert_block(*stmt.false_block, exit);

    int true_block = insert_block(*stmt.true_block, exit);
    int entry = cfg.insert_new_block();

    std::optional<double> val = cee.evaluate(*stmt.condition, true);
    cfg.insert_edge(entry, true_block, val != 0);
    cfg.insert_edge(entry, false_block, val.value_or(0) == 0);

    cfg.insert_stmt(&stmt, entry);
    return insert_expr(*stmt.condition, entry);
}

// Insert a while statement with loop latch and body into the CFG.
int CFGBuilder::insert_while_stmt(const DecoratedWhileStmt &stmt, int exit) {
    int latch = cfg.insert_new_block();
    int body = insert_block(*stmt.body, latch);

    int header = cfg.insert_new_block();
    cfg.insert_edge(latch, header, true);

    std::optional<double> val = cee.evaluate(*stmt.condition, true);
    cfg.insert_edge(header, body, val != 0);
    cfg.insert_edge(header, exit, val.value_or(0) == 0);

    cfg.insert_stmt(&stmt, header);
    insert_expr(*stmt.condition, header);

    return header;
}

// Insert a return statement into the CFG.
int CFGBuilder::insert_return_stmt(const DecoratedReturnStmt &stmt, int block) {
    block = cfg.insert_new_block_before(cfg.exit, true);

    cfg.insert_stmt(&stmt, block);
    if (stmt.expr)
        return insert_expr(*stmt.expr, block);

    return block;
}

int CFGBuilder::insert_decl_stmt(const DecoratedDeclStmt &stmt, int block) {
    cfg.insert_stmt(&stmt, block);

    if (const auto &init = stmt.var_decl->init)
        return insert_expr(*init, block);

    return block;
}

// Insert an expression into a specific block and handle nested expressions.
int CFGBuilder::insert_expr(const DecoratedExpr &expr, int block) {
    cfg.insert_stmt(&expr, block);

    if (const auto *call = dynamic_cast<const DecoratedCallExpr *>(&expr)) {
        for (auto it = call->arguments.rbegin(); it != call->arguments.rend(); ++it)
            insert_expr(**it, block);
        return block;
    }

    if (const auto *grouping = dynamic_cast<const DecoratedGroupingExpr *>(&expr))
        return insert_expr(*grouping->expr, block);

    if (const auto *binop = dynamic_cast<const DecoratedBinaryOP *>(&expr))
        return insert_expr(*binop->rhs, block), insert_expr(*binop->lhs, block);

    if (const auto *unop = dynamic_cast<const DecoratedUnaryOP *>(&expr))
        return insert_expr(*unop->operand, block);

    return block;
}

// Insert a generic statement, handling if, while, expression, and return cases.
int CFGBuilder::insert_stmt(const DecoratedStmt &stmt, int block) {
    if (auto *if_stmt = dynamic_cast<const DecoratedIfStmt *>(&stmt))
        return insert_if_stmt(*if_stmt, block);

    if (auto *while_stmt = dynamic_cast<const DecoratedWhileStmt *>(&stmt))
        return insert_while_stmt(*while_stmt, block);

    if (auto *expr = dynamic_cast<const DecoratedExpr *>(&stmt))
        return insert_expr(*expr, block);

    if (auto *decl_stmt = dynamic_cast<const DecoratedDeclStmt *>(&stmt))
        return insert_decl_stmt(*decl_stmt, block);

    if (auto *assignment = dynamic_cast<const DecoratedAssignment *>(&stmt))
        return insert_assignment(*assignment, block);

    auto *return_stmt = dynamic_cast<const DecoratedReturnStmt *>(&stmt);
    assert(return_stmt && "unexpected statement");

    return insert_return_stmt(*return_stmt, block);
}

int CFGBuilder::insert_assignment(const DecoratedAssignment &stmt, int block) {
    cfg.insert_stmt(&stmt, block);
    return insert_expr(*stmt.expr, block);
}

// Insert a block and handle terminator logic, creating new blocks as needed.
int CFGBuilder::insert_block(const DecoratedBlock &block, int succ) {
    const auto &stmts = block.statements;

    bool insert_new_block = true;
    for (auto it = stmts.rbegin(); it != stmts.rend(); ++it) {
        if (insert_new_block && !is_terminator(**it))
            succ = cfg.insert_new_block_before(succ, true);

        insert_new_block = dynamic_cast<const DecoratedWhileStmt *>(it->get());
        succ = insert_stmt(**it, succ);
    }

    return succ;
}

// Build function to create the CFG for a function declaration.
CFG CFGBuilder::build(const DecoratedFunctionDecl &fn) {
    cfg = {};
    cfg.exit = cfg.insert_new_block();

    int body = insert_block(*fn.body, cfg.exit);

    cfg.entry = cfg.insert_new_block_before(body, true);
    return cfg;
}
