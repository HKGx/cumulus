const std = @import("std");
const lex = @import("./lex.zig");

fn nextLineAlloc(allocator: std.mem.Allocator, reader: anytype) ![]const u8 {
    const line = try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024) orelse return error.Empty;

    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});
    const line = try nextLineAlloc(std.heap.page_allocator, std.io.getStdIn().reader());
    defer std.heap.page_allocator.free(line);
    std.debug.print("{s}", .{line});
    _ = try stdout.writeAll(line);
    _ = try stdout.writeAll(line);
    _ = try stdout.writeAll(line);
    try bw.flush();
}
