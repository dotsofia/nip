#ifndef _NIP_CFG_H 
#define _NIP_CFG_H

#include <set>
#include <vector>

#include "ast.h"
#include "constexpr.h"

// Structure representing a basic block in the CFG, with predecessors, successors, and statements.
struct BasicBlock {
    std::set<std::pair<int, bool>> predecessors;
    std::set<std::pair<int, bool>> successors;
    std::vector<const DecoratedStmt *> statements;
};

// Control Flow Graph (CFG) structure, inheriting from Printable..
struct CFG : public Printable {
    std::vector<BasicBlock> basic_blocks;
    int entry = -1;
    int exit = -1;

    // Insert a new block at the end of basic_blocks and return its index.
    int insert_new_block() {
        basic_blocks.emplace_back();
        return basic_blocks.size() - 1;
    }

    // Insert a new block before a specified block and add an edge between them.
    int insert_new_block_before(int before, bool reachable) {
        int b = insert_new_block();
        insert_edge(b, before, reachable);
        return b;
    }

    // Insert an edge between two blocks with specified reachability.
    void insert_edge(int from, int to, bool reachable) {
        basic_blocks[from].successors.emplace(std::make_pair(to, reachable));
        basic_blocks[to].predecessors.emplace(std::make_pair(from, reachable));
    }

    // Insert a statement into a specified block.
    void insert_stmt(const DecoratedStmt *stmt, int block) {
        basic_blocks[block].statements.emplace_back(stmt);
    }

    void print(size_t level = 0) const override;
};

// CFGBuilder class responsible for constructing the CFG.
class CFGBuilder {
    ConstExprEval cee;
    CFG cfg;

    int insert_block(const DecoratedBlock &block, int successor);
    int insert_if_stmt(const DecoratedIfStmt &stmt, int exit);
    int insert_while_stmt(const DecoratedWhileStmt &stmt, int exit);
    int insert_assignment(const DecoratedAssignment &stmt, int block);

    int insert_stmt(const DecoratedStmt &stmt, int block);
    int insert_return_stmt(const DecoratedReturnStmt &stmt, int block);
    int insert_decl_stmt(const DecoratedDeclStmt &stmt, int block);
    int insert_expr(const DecoratedExpr &expr, int block);

public:
    // Main function to build the CFG for a function declaration.
    CFG build(const DecoratedFunctionDecl &fn);
};

#endif 
