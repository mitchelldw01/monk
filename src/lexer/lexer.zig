const std = @import("std");
const ascii = std.ascii;
const mem = std.mem;
const ArrayList = std.ArrayList;

const token = @import("token.zig");
pub const Token = token.Token;
const TokenType = token.TokenType;
pub const Position = token.Position;

const keywords = @import("keywords.zig");

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    input: []const u8,
    position: usize,
    read_position: usize,
    curr_byte: u8,
    token_position: Position,

    pub fn init(allocator: mem.Allocator, input: []const u8) Lexer {
        var lexer = Lexer{
            .allocator = allocator,
            .input = input,
            .position = 0,
            .read_position = 0,
            .curr_byte = 0,
            .token_position = Position.init(),
        };
        lexer.readChar();
        return lexer;
    }

    pub fn nextToken(self: *Lexer) !Token {
        self.skipWhitespace();

        const curr_bytes = &[_]u8{self.curr_byte};
        var tok: Token = undefined;
        self.token_position.start_col = self.token_position.end_col;

        switch (self.curr_byte) {
            '=' => eq: {
                if (self.peekChar() != '=') {
                    tok = try Token.init(self.allocator, TokenType.ASSIGN, curr_bytes, self.token_position);
                    break :eq;
                }
                tok = try self.makeTwoCharToken(TokenType.EQ);
            },
            '!' => bang: {
                if (self.peekChar() != '=') {
                    tok = try Token.init(self.allocator, TokenType.BANG, curr_bytes, self.token_position);
                    break :bang;
                }
                tok = try self.makeTwoCharToken(TokenType.NOT_EQ);
            },
            '+' => tok = try Token.init(self.allocator, TokenType.PLUS, curr_bytes, self.token_position),
            '-' => tok = try Token.init(self.allocator, TokenType.MINUS, curr_bytes, self.token_position),
            '*' => tok = try Token.init(self.allocator, TokenType.ASTERISK, curr_bytes, self.token_position),
            '/' => tok = try Token.init(self.allocator, TokenType.SLASH, curr_bytes, self.token_position),
            '<' => tok = try Token.init(self.allocator, TokenType.LT, curr_bytes, self.token_position),
            '>' => tok = try Token.init(self.allocator, TokenType.GT, curr_bytes, self.token_position),
            ',' => tok = try Token.init(self.allocator, TokenType.COMMA, curr_bytes, self.token_position),
            ';' => tok = try Token.init(self.allocator, TokenType.SEMICOLON, curr_bytes, self.token_position),
            '(' => tok = try Token.init(self.allocator, TokenType.LPAREN, curr_bytes, self.token_position),
            ')' => tok = try Token.init(self.allocator, TokenType.RPAREN, curr_bytes, self.token_position),
            '{' => tok = try Token.init(self.allocator, TokenType.LBRACE, curr_bytes, self.token_position),
            '}' => tok = try Token.init(self.allocator, TokenType.RBRACE, curr_bytes, self.token_position),
            0 => tok = try Token.init(self.allocator, TokenType.EOF, "", self.token_position),
            else => {
                if (isIdentifierByte(self.curr_byte)) {
                    const literal = self.readIdentifier();
                    tok = try Token.init(self.allocator, keywords.lookupIdent(literal), literal, self.token_position);
                    // decrement only the token's column because we had to read ahead
                    tok.position.end_col -= 1;
                    return tok;
                }
                if (ascii.isDigit(self.curr_byte)) {
                    const literal = self.readNumber();
                    tok = try Token.init(self.allocator, TokenType.INT, literal, self.token_position);
                    // decrement only the token's column because we had to read ahead
                    tok.position.end_col -= 1;
                    return tok;
                }
                tok = try Token.init(self.allocator, TokenType.ILLEGAL, curr_bytes, self.token_position);
            },
        }

        self.readChar();
        return tok;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (ascii.isWhitespace(self.curr_byte)) {
            if (self.curr_byte == '\n') {
                self.token_position.row += 1;
                self.token_position.end_col = 0;
            }
            self.readChar();
        }
    }

    fn readChar(self: *Lexer) void {
        self.token_position.end_col += 1;

        if (self.read_position >= self.input.len) {
            self.curr_byte = 0;
        } else {
            self.curr_byte = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const pos = self.position;
        while (isIdentifierByte(self.curr_byte)) {
            self.readChar();
        }

        return self.input[pos..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        const pos = self.position;
        while (ascii.isDigit(self.curr_byte)) {
            self.readChar();
        }
        return self.input[pos..self.position];
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        }
        return self.input[self.read_position];
    }

    fn makeTwoCharToken(self: *Lexer, token_type: TokenType) !Token {
        const curr_byte = self.curr_byte;
        self.readChar();
        const tok_bytes = &[_]u8{ curr_byte, self.curr_byte };
        return try Token.init(self.allocator, token_type, tok_bytes, self.token_position);
    }
};

fn isIdentifierByte(byte: u8) bool {
    return byte == '_' or ascii.isAlphabetic(byte);
}

test "lexer" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const TestCase = struct {
        expected_type: TokenType,
        expected_literal: []const u8,
        expected_position: Position,
    };

    const test_cases = [_]TestCase{
        .{
            .expected_type = TokenType.LET,
            .expected_literal = "let",
            .expected_position = Position{ .row = 0, .start_col = 1, .end_col = 3 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "five",
            .expected_position = Position{ .row = 0, .start_col = 5, .end_col = 8 },
        },
        .{
            .expected_type = TokenType.ASSIGN,
            .expected_literal = "=",
            .expected_position = Position{ .row = 0, .start_col = 10, .end_col = 10 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "5",
            .expected_position = Position{ .row = 0, .start_col = 12, .end_col = 12 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 0, .start_col = 13, .end_col = 13 },
        },
        .{
            .expected_type = TokenType.LET,
            .expected_literal = "let",
            .expected_position = Position{ .row = 1, .start_col = 1, .end_col = 3 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "ten",
            .expected_position = Position{ .row = 1, .start_col = 5, .end_col = 7 },
        },
        .{
            .expected_type = TokenType.ASSIGN,
            .expected_literal = "=",
            .expected_position = Position{ .row = 1, .start_col = 9, .end_col = 9 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "10",
            .expected_position = Position{ .row = 1, .start_col = 11, .end_col = 12 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 1, .start_col = 13, .end_col = 13 },
        },
        .{
            .expected_type = TokenType.LET,
            .expected_literal = "let",
            .expected_position = Position{ .row = 3, .start_col = 1, .end_col = 3 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "add",
            .expected_position = Position{ .row = 3, .start_col = 5, .end_col = 7 },
        },
        .{
            .expected_type = TokenType.ASSIGN,
            .expected_literal = "=",
            .expected_position = Position{ .row = 3, .start_col = 9, .end_col = 9 },
        },
        .{
            .expected_type = TokenType.FUNCTION,
            .expected_literal = "fn",
            .expected_position = Position{ .row = 3, .start_col = 11, .end_col = 12 },
        },
        .{
            .expected_type = TokenType.LPAREN,
            .expected_literal = "(",
            .expected_position = Position{ .row = 3, .start_col = 13, .end_col = 13 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "x",
            .expected_position = Position{ .row = 3, .start_col = 14, .end_col = 14 },
        },
        .{
            .expected_type = TokenType.COMMA,
            .expected_literal = ",",
            .expected_position = Position{ .row = 3, .start_col = 15, .end_col = 15 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "y",
            .expected_position = Position{ .row = 3, .start_col = 17, .end_col = 17 },
        },
        .{
            .expected_type = TokenType.RPAREN,
            .expected_literal = ")",
            .expected_position = Position{ .row = 3, .start_col = 18, .end_col = 18 },
        },
        .{
            .expected_type = TokenType.LBRACE,
            .expected_literal = "{",
            .expected_position = Position{ .row = 3, .start_col = 20, .end_col = 20 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "x",
            .expected_position = Position{ .row = 4, .start_col = 5, .end_col = 5 },
        },
        .{
            .expected_type = TokenType.PLUS,
            .expected_literal = "+",
            .expected_position = Position{ .row = 4, .start_col = 7, .end_col = 7 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "y",
            .expected_position = Position{ .row = 4, .start_col = 9, .end_col = 9 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 4, .start_col = 10, .end_col = 10 },
        },
        .{
            .expected_type = TokenType.RBRACE,
            .expected_literal = "}",
            .expected_position = Position{ .row = 5, .start_col = 1, .end_col = 1 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 5, .start_col = 2, .end_col = 2 },
        },
        .{
            .expected_type = TokenType.LET,
            .expected_literal = "let",
            .expected_position = Position{ .row = 7, .start_col = 1, .end_col = 3 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "result",
            .expected_position = Position{ .row = 7, .start_col = 5, .end_col = 10 },
        },
        .{
            .expected_type = TokenType.ASSIGN,
            .expected_literal = "=",
            .expected_position = Position{ .row = 7, .start_col = 12, .end_col = 12 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "add",
            .expected_position = Position{ .row = 7, .start_col = 14, .end_col = 16 },
        },
        .{
            .expected_type = TokenType.LPAREN,
            .expected_literal = "(",
            .expected_position = Position{ .row = 7, .start_col = 17, .end_col = 17 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "five",
            .expected_position = Position{ .row = 7, .start_col = 18, .end_col = 21 },
        },
        .{
            .expected_type = TokenType.COMMA,
            .expected_literal = ",",
            .expected_position = Position{ .row = 7, .start_col = 22, .end_col = 22 },
        },
        .{
            .expected_type = TokenType.IDENT,
            .expected_literal = "ten",
            .expected_position = Position{ .row = 7, .start_col = 24, .end_col = 26 },
        },
        .{
            .expected_type = TokenType.RPAREN,
            .expected_literal = ")",
            .expected_position = Position{ .row = 7, .start_col = 27, .end_col = 27 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 7, .start_col = 28, .end_col = 28 },
        },
        .{
            .expected_type = TokenType.BANG,
            .expected_literal = "!",
            .expected_position = Position{ .row = 8, .start_col = 1, .end_col = 1 },
        },
        .{
            .expected_type = TokenType.MINUS,
            .expected_literal = "-",
            .expected_position = Position{ .row = 8, .start_col = 2, .end_col = 2 },
        },
        .{
            .expected_type = TokenType.SLASH,
            .expected_literal = "/",
            .expected_position = Position{ .row = 8, .start_col = 3, .end_col = 3 },
        },
        .{
            .expected_type = TokenType.ASTERISK,
            .expected_literal = "*",
            .expected_position = Position{ .row = 8, .start_col = 4, .end_col = 4 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "5",
            .expected_position = Position{ .row = 8, .start_col = 5, .end_col = 5 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 8, .start_col = 6, .end_col = 6 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "5",
            .expected_position = Position{ .row = 9, .start_col = 1, .end_col = 1 },
        },
        .{
            .expected_type = TokenType.LT,
            .expected_literal = "<",
            .expected_position = Position{ .row = 9, .start_col = 3, .end_col = 3 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "10",
            .expected_position = Position{ .row = 9, .start_col = 5, .end_col = 6 },
        },
        .{
            .expected_type = TokenType.GT,
            .expected_literal = ">",
            .expected_position = Position{ .row = 9, .start_col = 8, .end_col = 8 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "5",
            .expected_position = Position{ .row = 9, .start_col = 10, .end_col = 10 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 9, .start_col = 11, .end_col = 11 },
        },
        .{
            .expected_type = TokenType.IF,
            .expected_literal = "if",
            .expected_position = Position{ .row = 11, .start_col = 1, .end_col = 2 },
        },
        .{
            .expected_type = TokenType.LPAREN,
            .expected_literal = "(",
            .expected_position = Position{ .row = 11, .start_col = 4, .end_col = 4 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "5",
            .expected_position = Position{ .row = 11, .start_col = 5, .end_col = 5 },
        },
        .{
            .expected_type = TokenType.LT,
            .expected_literal = "<",
            .expected_position = Position{ .row = 11, .start_col = 7, .end_col = 7 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "10",
            .expected_position = Position{ .row = 11, .start_col = 9, .end_col = 10 },
        },
        .{
            .expected_type = TokenType.RPAREN,
            .expected_literal = ")",
            .expected_position = Position{ .row = 11, .start_col = 11, .end_col = 11 },
        },
        .{
            .expected_type = TokenType.LBRACE,
            .expected_literal = "{",
            .expected_position = Position{ .row = 11, .start_col = 13, .end_col = 13 },
        },
        .{
            .expected_type = TokenType.RETURN,
            .expected_literal = "return",
            .expected_position = Position{ .row = 12, .start_col = 5, .end_col = 10 },
        },
        .{
            .expected_type = TokenType.TRUE,
            .expected_literal = "true",
            .expected_position = Position{ .row = 12, .start_col = 12, .end_col = 15 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 12, .start_col = 16, .end_col = 16 },
        },
        .{
            .expected_type = TokenType.RBRACE,
            .expected_literal = "}",
            .expected_position = Position{ .row = 13, .start_col = 1, .end_col = 1 },
        },
        .{
            .expected_type = TokenType.ELSE,
            .expected_literal = "else",
            .expected_position = Position{ .row = 13, .start_col = 3, .end_col = 6 },
        },
        .{
            .expected_type = TokenType.LBRACE,
            .expected_literal = "{",
            .expected_position = Position{ .row = 13, .start_col = 8, .end_col = 8 },
        },
        .{
            .expected_type = TokenType.RETURN,
            .expected_literal = "return",
            .expected_position = Position{ .row = 14, .start_col = 5, .end_col = 10 },
        },
        .{
            .expected_type = TokenType.FALSE,
            .expected_literal = "false",
            .expected_position = Position{ .row = 14, .start_col = 12, .end_col = 16 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 14, .start_col = 17, .end_col = 17 },
        },
        .{
            .expected_type = TokenType.RBRACE,
            .expected_literal = "}",
            .expected_position = Position{ .row = 15, .start_col = 1, .end_col = 1 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "10",
            .expected_position = Position{ .row = 17, .start_col = 1, .end_col = 2 },
        },
        .{
            .expected_type = TokenType.EQ,
            .expected_literal = "==",
            .expected_position = Position{ .row = 17, .start_col = 4, .end_col = 5 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "10",
            .expected_position = Position{ .row = 17, .start_col = 7, .end_col = 8 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 17, .start_col = 9, .end_col = 9 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "10",
            .expected_position = Position{ .row = 18, .start_col = 1, .end_col = 2 },
        },
        .{
            .expected_type = TokenType.NOT_EQ,
            .expected_literal = "!=",
            .expected_position = Position{ .row = 18, .start_col = 4, .end_col = 5 },
        },
        .{
            .expected_type = TokenType.INT,
            .expected_literal = "9",
            .expected_position = Position{ .row = 18, .start_col = 7, .end_col = 7 },
        },
        .{
            .expected_type = TokenType.SEMICOLON,
            .expected_literal = ";",
            .expected_position = Position{ .row = 18, .start_col = 8, .end_col = 8 },
        },
        .{
            .expected_type = TokenType.EOF,
            .expected_literal = "",
            .expected_position = Position{ .row = 18, .start_col = 9, .end_col = 9 },
        },
    };

    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
        \\
        \\10 == 10;
        \\10 != 9;
    ;

    var lexer = Lexer.init(allocator, input);

    for (test_cases) |test_case| {
        var tok = try lexer.nextToken();
        defer tok.deinit();
        try testing.expectEqual(test_case.expected_type, tok.token_type);
        try testing.expectEqualSlices(u8, test_case.expected_literal, tok.literal);
        try testing.expectEqualDeep(test_case.expected_position, tok.position);
    }
}
