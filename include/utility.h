#ifndef _NIP_UTILITY_H
#define _NIP_UTILITY_H

#include <string>
#include <string_view>

// Declare variable with the return if error does not occur on parsing.
#define store_result(var, init)    \
    auto var = (init);             \
    if (!var)                      \
        return nullptr;

// If next_token doesn't match return and report the message.
#define match(tok, msg)                              \
    if (next_token.kind != tok)                      \
      return log_error(next_token.location, msg);

struct Printable {
    std::string indent(size_t level) const {
        return std::string(level * 2, ' ');
    }
    virtual ~Printable() = default;
    virtual void print(size_t level = 0) const = 0;
};

// Indicates the location within the file.
struct SourceLocation {
    std::string_view file_path;
    int line;
    int col;
};

// The internal representation of a source file.
struct SourceFile {
    std::string_view path;
    std::string buffer;
};

std::nullptr_t log_error(SourceLocation location, std::string_view message,
                      bool is_warning = false);
#endif
