const std = @import("std");
const io = std.io;
const mem = std.mem;

const lexer = @import("lexer");

const PROMPT = ">>";

pub fn start(allocator: mem.Allocator) !void {
    const stdin = io.getStdIn().reader();
    const stdout = io.getStdOut().writer();

    try stdout.print("MONK REPL v0.0.0\n", .{});

    while (true) {
        try stdout.print("{s} ", .{PROMPT});
        var input: [1024]u8 = undefined;
        const line = try stdin.readUntilDelimiter(&input, '\n');
        var lex = lexer.Lexer.init(allocator, line);

        while (true) {
            var tok = try lex.nextToken();
            defer tok.deinit();

            switch (tok.token_type) {
                .EOF => break,
                else => try stdout.print("{s}", .{tok}),
            }
        }
    }
}
