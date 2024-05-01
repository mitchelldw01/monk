const std = @import("std");
const ascii = std.ascii;
const mem = std.mem;
const ArrayList = std.ArrayList;

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

const keywords = @import("keywords.zig");

pub const Lexer = struct {
    allocator: std.mem.Allocator,
    input: []const u8,
    position: usize,
    read_position: usize,
    curr_byte: u8,
    row: usize,
    col: usize,

    pub fn init(allocator: mem.Allocator, input: []const u8) Lexer {
        var lexer = Lexer{
            .allocator = allocator,
            .input = input,
            .position = 0,
            .read_position = 0,
            .curr_byte = 0,
            .row = 1,
            .col = 1,
        };
        lexer.readChar();
        return lexer;
    }

    pub fn nextToken(self: *Lexer) !Token {
        self.skipWhitespace();

        const curr_bytes = &[_]u8{self.curr_byte};
        var tok: Token = undefined;
        switch (self.curr_byte) {
            '=' => eq: {
                if (self.peekChar() != '=') {
                    tok = try Token.init(self.allocator, TokenType.ASSIGN, curr_bytes);
                    break :eq;
                }
                tok = try self.makeTwoCharToken(TokenType.EQ);
            },
            '!' => bang: {
                if (self.peekChar() != '=') {
                    tok = try Token.init(self.allocator, TokenType.BANG, curr_bytes);
                    break :bang;
                }
                tok = try self.makeTwoCharToken(TokenType.NOT_EQ);
            },
            '+' => tok = try Token.init(self.allocator, TokenType.PLUS, curr_bytes),
            '-' => tok = try Token.init(self.allocator, TokenType.MINUS, curr_bytes),
            '*' => tok = try Token.init(self.allocator, TokenType.ASTERISK, curr_bytes),
            '/' => tok = try Token.init(self.allocator, TokenType.SLASH, curr_bytes),
            '<' => tok = try Token.init(self.allocator, TokenType.LT, curr_bytes),
            '>' => tok = try Token.init(self.allocator, TokenType.GT, curr_bytes),
            ',' => tok = try Token.init(self.allocator, TokenType.COMMA, curr_bytes),
            ';' => tok = try Token.init(self.allocator, TokenType.SEMICOLON, curr_bytes),
            '(' => tok = try Token.init(self.allocator, TokenType.LPAREN, curr_bytes),
            ')' => tok = try Token.init(self.allocator, TokenType.RPAREN, curr_bytes),
            '{' => tok = try Token.init(self.allocator, TokenType.LBRACE, curr_bytes),
            '}' => tok = try Token.init(self.allocator, TokenType.RBRACE, curr_bytes),
            0 => tok = try Token.init(self.allocator, TokenType.EOF, ""),
            else => {
                if (isIdentifierByte(self.curr_byte)) {
                    const literal = self.readIdentifier();
                    return try Token.init(self.allocator, keywords.lookupIdent(literal), literal);
                }
                if (ascii.isDigit(self.curr_byte)) {
                    const literal = self.readNumber();
                    return try Token.init(self.allocator, TokenType.INT, literal);
                }
                tok = try Token.init(self.allocator, TokenType.ILLEGAL, "");
            },
        }

        self.readChar();
        return tok;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (ascii.isWhitespace(self.curr_byte)) {
            if (self.curr_byte == '\n') {
                self.row += 1;
                self.col = 0;
            }

            self.readChar();
        }
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.curr_byte = 0;
        } else {
            self.curr_byte = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (isIdentifierByte(self.curr_byte)) {
            self.readChar();
        }
        return self.input[position..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;
        while (ascii.isDigit(self.curr_byte)) {
            self.readChar();
        }
        return self.input[position..self.position];
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
        return try Token.init(self.allocator, token_type, tok_bytes);
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
    };

    const test_cases = [_]TestCase{
        .{ .expected_type = TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "five" },
        .{ .expected_type = TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "ten" },
        .{ .expected_type = TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "add" },
        .{ .expected_type = TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = TokenType.FUNCTION, .expected_literal = "fn" },
        .{ .expected_type = TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "x" },
        .{ .expected_type = TokenType.COMMA, .expected_literal = "," },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "y" },
        .{ .expected_type = TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = TokenType.LBRACE, .expected_literal = "{" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "x" },
        .{ .expected_type = TokenType.PLUS, .expected_literal = "+" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "y" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.RBRACE, .expected_literal = "}" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.LET, .expected_literal = "let" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "result" },
        .{ .expected_type = TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "add" },
        .{ .expected_type = TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "five" },
        .{ .expected_type = TokenType.COMMA, .expected_literal = "," },
        .{ .expected_type = TokenType.IDENT, .expected_literal = "ten" },
        .{ .expected_type = TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.BANG, .expected_literal = "!" },
        .{ .expected_type = TokenType.MINUS, .expected_literal = "-" },
        .{ .expected_type = TokenType.SLASH, .expected_literal = "/" },
        .{ .expected_type = TokenType.ASTERISK, .expected_literal = "*" },
        .{ .expected_type = TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = TokenType.LT, .expected_literal = "<" },
        .{ .expected_type = TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = TokenType.GT, .expected_literal = ">" },
        .{ .expected_type = TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.IF, .expected_literal = "if" },
        .{ .expected_type = TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = TokenType.INT, .expected_literal = "5" },
        .{ .expected_type = TokenType.LT, .expected_literal = "<" },
        .{ .expected_type = TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = TokenType.LBRACE, .expected_literal = "{" },
        .{ .expected_type = TokenType.RETURN, .expected_literal = "return" },
        .{ .expected_type = TokenType.TRUE, .expected_literal = "true" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.RBRACE, .expected_literal = "}" },
        .{ .expected_type = TokenType.ELSE, .expected_literal = "else" },
        .{ .expected_type = TokenType.LBRACE, .expected_literal = "{" },
        .{ .expected_type = TokenType.RETURN, .expected_literal = "return" },
        .{ .expected_type = TokenType.FALSE, .expected_literal = "false" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.RBRACE, .expected_literal = "}" },
        .{ .expected_type = TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = TokenType.EQ, .expected_literal = "==" },
        .{ .expected_type = TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.INT, .expected_literal = "10" },
        .{ .expected_type = TokenType.NOT_EQ, .expected_literal = "!=" },
        .{ .expected_type = TokenType.INT, .expected_literal = "9" },
        .{ .expected_type = TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = TokenType.EOF, .expected_literal = "" },
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
    }
}
