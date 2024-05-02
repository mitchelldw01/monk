const std = @import("std");
const io = std.io;
const mem = std.mem;
const ArrayList = std.ArrayList;

const lexer = @import("lexer");

const PROMPT = "$";

pub fn start(allocator: mem.Allocator) !void {
    const stdin = io.getStdIn().reader();
    const stdout = io.getStdOut().writer();
    try stdout.print("MONK REPL v0.0.0\n", .{});

    while (true) {
        try processStdin(allocator, stdin, stdout);
    }
}

fn processStdin(allocator: mem.Allocator, stdin: anytype, stdout: anytype) !void {
    try stdout.print("{s} ", .{PROMPT});
    var input: [1024]u8 = undefined;
    const line = try stdin.readUntilDelimiter(&input, '\n');
    const tokens = try createTokens(allocator, line);
    defer {
        for (tokens.items) |tok| {
            tok.deinit();
        }
        tokens.deinit();
    }

    const contains_illegal_tok = containsIllegalToken(tokens);
    if (contains_illegal_tok) {
        try diagnoseError(tokens, &input);
        return;
    }

    for (tokens.items) |tok| {
        try stdout.print("TOKEN:\n{s}", .{tok});
    }
}

fn containsIllegalToken(tokens: ArrayList(lexer.Token)) bool {
    for (tokens.items) |tok| {
        switch (tok.token_type) {
            .ILLEGAL => return true,
            else => {},
        }
    }
    return false;
}

fn createTokens(allocator: mem.Allocator, line: []const u8) !ArrayList(lexer.Token) {
    var lex = lexer.Lexer.init(allocator, line);
    var tokens = ArrayList(lexer.Token).init(allocator);

    while (true) {
        const tok = try lex.nextToken();
        switch (tok.token_type) {
            .EOF => break,
            else => try tokens.append(tok),
        }
    }

    return tokens;
}

fn diagnoseError(tokens: ArrayList(lexer.Token), input: []const u8) !void {
    const stderr = io.getStdErr().writer();
    const err_meta = getErrorMetadata(tokens, input);

    try stderr.print("SyntaxError: invalid token '{s}' on line {d} column {d}\n\n", .{
        err_meta.token.literal,
        err_meta.token.position.row + 1,
        err_meta.token.position.start_col,
    });

    try stderr.print("> {s}\n", .{err_meta.line});
    for (0..err_meta.token.position.start_col + 1) |_| {
        try stderr.print(" ", .{});
    }
    try stderr.print("⎺\n", .{});
}

const ErrorMetadata = struct {
    token: lexer.Token,
    line: []const u8,

    fn init(token: lexer.Token, line: []const u8) ErrorMetadata {
        return ErrorMetadata{
            .token = token,
            .line = line,
        };
    }
};

fn getErrorMetadata(tokens: ArrayList(lexer.Token), input: []const u8) ErrorMetadata {
    var illegal_tok: lexer.Token = undefined;
    for (tokens.items) |tok| {
        switch (tok.token_type) {
            .ILLEGAL => {
                illegal_tok = tok;
                break;
            },
            else => {},
        }
    }

    var i: usize = 0;
    var illegal_line: []const u8 = undefined;
    var lines = mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        i += 1;
        if (i == illegal_tok.position.row + 1) {
            illegal_line = line;
        }
    }

    return ErrorMetadata.init(illegal_tok, illegal_line);
}
