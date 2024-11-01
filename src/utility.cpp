#include <iostream>

#include "../include/utility.h"

std::nullptr_t log_error(SourceLocation location, std::string_view message, bool is_warning) {
    const auto &[file, line, col] = location;

    std::cerr << file << ':' << line << ':' << col << ':'
        << (is_warning ? " warning: " : " error: ") << message << '\n';

    return nullptr;
}
