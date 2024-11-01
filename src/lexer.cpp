#include <iostream>

#include "../include/lexer.h"

static bool is_space(char c) {
    return c == ' ' || c == '\n' || c == '\r';
}

static bool is_alpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool is_digit(char c) { return c >= '0' && c <= '9'; }

Token Lexer::get_next_token() {
    char current = advance();

    // Skips whitespace.
    while (is_space(current)) current = advance();

    SourceLocation location{src->path, line, col};

    // Handle single character tokens.
    for (auto &&c : single_char_tokens) {
        if (c == current) 
           return Token{location, static_cast<TokenKind>(c)};
    }

    // Skips comments.
    if (current == '/' && peek() == '/') {
        char c = advance();
        // A comments goes until the end of the line.
        while (c != '\n' && c != '\0') c = advance();

        return get_next_token();
    }

    // Identifiers: [a-zA-Z][a-zA-Z0-9]*
    if (is_alpha(current)) {
        std::string name{current};

        while (is_alpha(peek()) || is_digit(peek())) 
            name += advance();
        
        // Check if identifier is a keyword
        if (keywords.count(name))
            return Token{location, keywords.at(name), std::move(name)};

        return Token{location, TokenKind::Identifier, std::move(name)};
    }

    // Number: [0-9]+(.[0-9]+)?
    if (is_digit(current)) {
        std::string value{current};

        while (is_digit(peek()))
            value += advance();

        // A simple integer number.
        if (peek() != '.')
            return Token{location, TokenKind::Number, value};

        value += advance();

        // Invalid fractional part.
        if (!is_digit(peek()))
            return Token{location, TokenKind::Invalid};

        while (is_digit(peek()))
            value += advance();

        return Token{location, TokenKind::Number, value};
    }
    
    // Could not identify token.
    return Token{location, TokenKind::Invalid};
}
