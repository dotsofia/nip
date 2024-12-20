#ifndef _NIP_LEXER_H_
#define _NIP_LEXER_H_

#include <optional>
#include <string>
#include <unordered_map>

#include "utility.h"

constexpr char single_char_tokens[] = {'\0', '(', ')', '{', '}', ':', ';',
                                       ',',  '+', '-', '*', '<', '>', '!'};

enum class TokenKind : char {
    // Represents any unknown piece of code.
    Invalid = -128,
    Slash,

    Equal,
    EqualEqual,
    AmpAmp,
    PipePipe,

    // Primary types
    Identifier,
    Number,

    // Keywords
    KeywordFn,
    KeywordVoid,
    KeywordReturn,
    KeywordNumber,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordLet,
    KeywordConst,

    // Single character codes are represented by their ascii value.
    _EOF = single_char_tokens[0],
    LParen = single_char_tokens[1],
    RParen = single_char_tokens[2],
    LBrace = single_char_tokens[3],
    RBrace = single_char_tokens[4],
    Colon  = single_char_tokens[5],
    SemiColon = single_char_tokens[6],
    Comma  = single_char_tokens[7],
    Plus = single_char_tokens[8],
    Minus = single_char_tokens[9],
    Star = single_char_tokens[10],
    LT = single_char_tokens[11],
    GT = single_char_tokens[12],
    NEQ = single_char_tokens[13],
};

// Maps the token lexeme with its kind
const std::unordered_map<std::string_view, TokenKind> keywords = {
    {"void", TokenKind::KeywordVoid},
    {"fn", TokenKind::KeywordFn},
    {"return", TokenKind::KeywordReturn},
    {"number", TokenKind::KeywordNumber},
    {"if", TokenKind::KeywordIf},
    {"else", TokenKind::KeywordElse},
    {"while", TokenKind::KeywordWhile},
    {"let", TokenKind::KeywordLet},
    {"const", TokenKind::KeywordConst},
};

struct Token {
    SourceLocation location;
    TokenKind kind;
    std::optional<std::string> value = std::nullopt;
};

class Lexer {
    const SourceFile *src;
    size_t i = 0;

    // Starts in the first line before the first character.
    int line = 1;
    int col = 0;

    char peek() const { return src->buffer[i]; }

    char advance() {
        col++;

        if (src->buffer[i] == '\n') {
            line++;
            col = 0;
        }

        return src->buffer[i++];
    }

    public:
    Lexer(const SourceFile &src) : src(&src) {}
    Token get_next_token();
};

#endif
