#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "../include/irgen.h"
#include "../include/lexer.h"
#include "../include/parser.h"
#include "../include/semantic.h"

struct CommandLineArgs {
    std::filesystem::path source;
    std::filesystem::path output;
    bool display_help = false;
    bool dump_ast = false;
    bool dump_dast = false;
    bool dump_llvm = false;
};

void help() {
    std::cout << "Usage:\n"
              << " nip [options] <source_file>\n\n"
              << "Options:\n"
              << "  -h            display this message\n"
              << "  -o <file>     write the output to <file>\n"
              << "  -dump-ast     print the abstract syntax tree\n"
              << "  -dump-dast    print the decorated syntax tree\n"
              << "  -dump-llvm    print the llvm module\n";
}

[[noreturn]]
void error(std::string_view msg) {
    std::cerr << "error: " << msg << '\n';
    std::exit(1);
}

CommandLineArgs parse_args(int argc, char **argv) {
    CommandLineArgs options;

    int i = 1;
    while (i < argc) {
        std::string_view arg = argv[i];

        // If the argument doesn't start with '-' it should be the file name.
        if (arg[0] != '-') {
            if (!options.source.empty())
                error("unexpected argument '" + std::string(arg) + '\'');

            options.source = arg;
        } else {
            if (arg == "-h")
                options.display_help = true;
            else if (arg == "-o")
                options.output = ++i < argc ? argv[i] : "";
            else if (arg == "-dump-ast")
                options.dump_ast = true;
            else if (arg == "-dump-dast")
                options.dump_dast = true;
            else if (arg == "-dump-llvm")
                options.dump_llvm = true;
            else
                error("unexpected option '" + std::string(arg) + '\'');
        }

        ++i;
    }
    return options;
}

int main(int argc, char **argv) {
    CommandLineArgs options = parse_args(argc, argv);

    if (options.display_help) {
        help();
        return 0;
    }

    if (options.source.empty())
        error("source file not specified");

    if (options.source.extension() != ".nip")
        error("unexpected source file extension");

    // Checks if file exists
    std::ifstream file(options.source);
    if (!file)
        error("failed to open '" + options.source.string() + '\'');

    std::stringstream buffer;
    buffer << file.rdbuf();
    SourceFile source = {options.source.c_str(), buffer.str()};

    Lexer lexer(source);
    Parser parser(lexer);

    auto [ast, success] = parser.parse_file();

    if (options.dump_ast) {
        for (auto &&fn : ast)
            fn->print();
        return 0;
    }

    if (!success)
        return 1;

    Semantic sema(std::move(ast));
    auto dast = sema.decorate_ast();

    if (options.dump_dast) {
        for (auto &&fn : dast)
            fn->print();
        return 0;
    }

    if (dast.empty())
        return 1;

    IRGen irgen(std::move(dast), options.source.c_str());
    std::unique_ptr<llvm::Module> llvm_ir = irgen.generate_ir();

    if(options.dump_llvm) {
        llvm_ir->print(llvm::errs(), nullptr);
        return 0;
    }

    // Store the llvm module in a temporary file
    std::stringstream path;
    path << "tmp-" << std::filesystem::hash_value(options.source) << ".ll";
    const std::string &llvm_ir_path = path.str();

    std::error_code err;
    llvm::raw_fd_ostream f(llvm_ir_path, err);
    llvm_ir->print(f, nullptr);

    // Compile the llvm ir using clang
    std::stringstream command;
    command << "clang-20 " << llvm_ir_path;
    if (!options.output.empty())
        command << " -o " << options.output;

    int ret = std::system(command.str().c_str());

    // Remove llvm temporary file
    std::filesystem::remove(llvm_ir_path);

    return ret;
}
