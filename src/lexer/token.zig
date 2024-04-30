const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    // Identifiers and literals
    IDENT,
    INT,
    // Operators
    ASSIGN,
    PLUS,
    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    // Keywords
    FUNCTION,
    LET,
};

pub const Token = struct {
    token_type: TokenType,
    literal: []const u8,

    pub fn init(token_type: TokenType, literal: []const u8) @This() {
        return @This(){
            .token_type = token_type,
            .literal = literal,
        };
    }
};
