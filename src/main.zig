const std = @import("std");
const heap = std.heap;

const repl = @import("repl");

pub fn main() !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    try repl.start(allocator);
}
