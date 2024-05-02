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

pub const Position = struct {
    row: usize,
    start_col: usize,
    end_col: usize,

    pub fn init() Position {
        return Position{
            .row = 0,
            .start_col = 0,
            .end_col = 0,
        };
    }
};

pub const Token = struct {
    allocator: mem.Allocator,
    token_type: TokenType,
    literal: []const u8,
    position: Position,

    pub fn init(allocator: mem.Allocator, token_type: TokenType, literal: []const u8, pos: Position) !Token {
        return Token{
            .allocator = allocator,
            .token_type = token_type,
            .literal = try allocator.dupe(u8, literal),
            .position = pos,
        };
    }

    pub fn deinit(self: Token) void {
        self.allocator.free(self.literal);
    }

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        try writer.print("token_type: {}\nliteral: {s}\nrow: {}\nstart_col: {}\nend_col: {}\n", .{
            self.token_type,
            self.literal,
            self.position.row,
            self.position.start_col,
            self.position.end_col,
        });
    }
};
