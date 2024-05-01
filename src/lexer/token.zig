const std = @import("std");
const mem = std.mem;
const ArrayList = std.ArrayList;

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    // Identifiers and literals
    IDENT,
    INT,
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOT_EQ,
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
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
};

pub const Token = struct {
    allocator: mem.Allocator,
    token_type: TokenType,
    literal: []const u8,

    pub fn init(allocator: mem.Allocator, token_type: TokenType, literal: []const u8) !Token {
        return Token{
            .allocator = allocator,
            .token_type = token_type,
            .literal = try allocator.dupe(u8, literal),
        };
    }

    pub fn deinit(self: *Token) void {
        self.allocator.free(self.literal);
    }

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{} {s}\n", .{ self.token_type, self.literal });
    }
};
