#include <fstream>
#include <iostream>
#include <sstream>

#include "../include/lexer.h"
#include "../include/parser.h"

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

    return !success;
}
