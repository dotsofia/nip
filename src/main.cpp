#include <llvm/Support/raw_ostream.h>

#include <fstream>
#include <iostream>
#include <sstream>

#include "../include/irgen.h"
#include "../include/lexer.h"
#include "../include/parser.h"
#include "../include/semantic.h"

int main(int argc, const char **argv) {
    std::ifstream file(argv[1]);

    std::stringstream buffer;
    buffer << file.rdbuf();
    SourceFile source = {argv[1], buffer.str()};

    Lexer lexer(source);
    Parser parser(lexer);

    auto [ast, success] = parser.parse_file();

    for (auto &&fn : ast)
        fn->print();

    Semantic sema(std::move(ast));
    auto res = sema.decorate_ast();

    for (auto &&fn : res)
        fn->print();

    IRGen irgen(std::move(res), argv[1]);
    irgen.generate_ir()->print(llvm::errs(), nullptr);
    
    return 0;
}
