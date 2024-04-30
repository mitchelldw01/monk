const std = @import("std");
const token = @import("token.zig");
const TokenType = token.TokenType;

const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "fn", TokenType.FUNCTION },
    .{ "let", TokenType.LET },
    .{ "true", TokenType.TRUE },
    .{ "false", TokenType.FALSE },
    .{ "if", TokenType.IF },
    .{ "else", TokenType.ELSE },
    .{ "return", TokenType.RETURN },
});

pub fn lookupIdent(ident: []const u8) TokenType {
    if (keywords.get(ident)) |tok| {
        return tok;
    }
    return TokenType.IDENT;
}
