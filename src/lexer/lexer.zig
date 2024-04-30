const std = @import("std");
const token = @import("token.zig");

const mem = std.mem;

const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    curr_byte: u8,

    pub fn init(input: []const u8) @This() {
        var lexer = @This(){
            .input = input,
            .position = 0,
            .read_position = 0,
            .curr_byte = 0,
        };
        lexer.readByte();
        return lexer;
    }

    pub fn nextToken(self: *@This()) token.Token {
        const curr_bytes = &[_]u8{self.curr_byte};
        const tok = switch (self.curr_byte) {
            '=' => token.Token.init(token.TokenType.ASSIGN, curr_bytes),
            ';' => token.Token.init(token.TokenType.SEMICOLON, curr_bytes),
            '(' => token.Token.init(token.TokenType.LPAREN, curr_bytes),
            ')' => token.Token.init(token.TokenType.RPAREN, curr_bytes),
            ',' => token.Token.init(token.TokenType.COMMA, curr_bytes),
            '+' => token.Token.init(token.TokenType.PLUS, curr_bytes),
            '{' => token.Token.init(token.TokenType.LBRACE, curr_bytes),
            '}' => token.Token.init(token.TokenType.RBRACE, curr_bytes),
            0 => token.Token.init(token.TokenType.EOF, ""),
            else => token.Token.init(token.TokenType.ILLEGAL, ""),
        };

        self.readByte();
        return tok;
    }

    fn readByte(self: *@This()) void {
        if (self.read_position >= self.input.len) {
            self.curr_byte = 0;
        } else {
            self.curr_byte = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }
};

test "lexer" {
    const testing = std.testing;

    const TestCase = struct {
        expected_type: token.TokenType,
        expected_literal: []const u8,
    };

    const test_cases = [_]TestCase{
        .{ .expected_type = token.TokenType.ASSIGN, .expected_literal = "=" },
        .{ .expected_type = token.TokenType.PLUS, .expected_literal = "+" },
        .{ .expected_type = token.TokenType.LPAREN, .expected_literal = "(" },
        .{ .expected_type = token.TokenType.RPAREN, .expected_literal = ")" },
        .{ .expected_type = token.TokenType.LBRACE, .expected_literal = "{" },
        .{ .expected_type = token.TokenType.RBRACE, .expected_literal = "}" },
        .{ .expected_type = token.TokenType.COMMA, .expected_literal = "," },
        .{ .expected_type = token.TokenType.SEMICOLON, .expected_literal = ";" },
        .{ .expected_type = token.TokenType.EOF, .expected_literal = "" },
    };

    var lexer = Lexer.init("=+(){},;");

    for (test_cases) |test_case| {
        const tok = lexer.nextToken();
        try testing.expectEqual(tok.token_type, test_case.expected_type);
        try testing.expectEqualSlices(u8, tok.literal, test_case.expected_literal);
    }
}
