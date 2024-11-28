const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Token = union(enum) {
    identifier: []const u8,
    integer: []const u8,
    comma,
    dot,
    plus,
    minus,
    star,
    slash,
    left_paren,
    right_paren,

    pub fn format(token: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Token.{s}(", .{@tagName(token)});

        switch (token) {
            Token.identifier => |value| try writer.print("\"{s}\"", .{value}),
            Token.integer => |value| try writer.print("{s}", .{value}),
            else => {},
        }

        try writer.writeAll(")");
    }

    pub fn equals(a: Token, b: Token) bool {
        const Tag = std.meta.Tag(Token);

        const aTag = @as(Tag, a);
        const bTag = @as(Tag, b);

        if (aTag != bTag) return false;

        return switch (a) {
            Token.identifier => std.mem.eql(u8, a.identifier, b.identifier),
            Token.integer => std.mem.eql(u8, a.integer, b.integer),
            else => true,
        };
    }
};

const token_table = [_]Token{ .left_paren, .right_paren, .star, .plus, .comma, .minus, .dot, .slash };

fn charToSimpleToken(c: u8) ?Token {
    if (c >= '(' and c <= '/') return token_table[c - '('];
    return null;
}

pub const Lexer = struct {
    source: []const u8,
    current_idx: usize,
    allocator: Allocator,

    pub fn init(allocator: Allocator, source: []const u8) Lexer {
        return Lexer{
            .allocator = allocator,
            .source = source,
            .current_idx = 0,
        };
    }

    inline fn ended(self: Lexer) bool {
        return self.current_idx >= self.source.len;
    }

    inline fn current(self: Lexer) ?u8 {
        if (self.ended()) return null;

        return self.source[self.current_idx];
    }

    inline fn peek(self: Lexer) ?u8 {
        if (self.current_idx >= self.source.len - 1) return null;

        return self.source[self.current_idx + 1];
    }

    inline fn advance(self: *Lexer) void {
        self.current_idx += 1;
    }

    fn skipWhitespaces(self: *Lexer) void {
        while (self.current()) |value| : (self.advance()) {
            if (!std.ascii.isWhitespace(value)) return;
        }
    }

    fn parseIdentifier(self: *Lexer) Token {
        const start_idx = self.current_idx;

        while (self.current()) |value| {
            if (!std.ascii.isAlphanumeric(value)) break;
            self.advance();
        }

        const end_idx = self.current_idx;
        return Token{ .identifier = self.source[start_idx..end_idx] };
    }

    fn parseInteger(self: *Lexer) Token {
        const start_idx = self.current_idx;

        while (self.current()) |value| {
            if (!std.ascii.isDigit(value)) break;
            self.advance();
        }

        const end_idx = self.current_idx;
        return Token{ .integer = self.source[start_idx..end_idx] };
    }

    pub fn lex(self: *Lexer) !ArrayList(Token) {
        var tokens = ArrayList(Token).init(self.allocator);
        errdefer tokens.deinit();

        while (self.current()) |value| : (self.skipWhitespaces()) {
            if (charToSimpleToken(value)) |token| {
                try tokens.append(token);
                self.advance();
                continue;
            }

            const token = switch (value) {
                'A'...'Z', 'a'...'z' => self.parseIdentifier(),
                '0'...'9' => self.parseInteger(),
                else => {
                    std.debug.print("found {any} which is unsupported", .{value});
                    return error.Unsupported;
                },
            };

            try tokens.append(token);
        }

        return tokens;
    }
};

fn expectLex(source: []const u8, expected: []const Token) !void {
    var lexer = Lexer.init(std.testing.allocator, source);
    const lexed = try lexer.lex();
    defer lexed.deinit();

    try expectEqualTokens(expected, lexed.items);
}

fn print(comptime fmt: []const u8, args: anytype) void {
    if (@inComptime()) {
        @compileError(std.fmt.comptimePrint(fmt, args));
    } else {
        std.debug.print(fmt, args);
    }
}

fn expectTokenFormat(expected: []const u8, token: Token) !void {
    const str = try std.fmt.allocPrint(std.testing.allocator, "{s}", .{token});
    defer std.testing.allocator.free(str);

    try std.testing.expectEqualStrings(expected, str);
}

fn expectEqualTokens(expected: []const Token, actual: []const Token) !void {
    var i: usize = 0;

    const stderr = std.io.getStdErr();
    const ttyconf = std.io.tty.detectConfig(stderr);

    const equal_len = expected.len == actual.len;
    const equal = switch (equal_len) {
        true => while (i < expected.len) : (i += 1) {
            if (!expected[i].equals(actual[i])) break false;
        } else true,
        false => false,
    };

    if (equal) return;
    const upper = @max(expected.len, actual.len);

    print("=== found not equal tokens ===\n", .{});

    while (i < upper) : (i += 1) {
        if (i < actual.len and i < expected.len) {
            const tokenEqual = expected[i].equals(actual[i]);
            if (tokenEqual) {
                print("[{d}]: {s}\n", .{ i, expected[i] });
            } else {
                try ttyconf.setColor(stderr.writer(), .red);
                print("[{d}]: {s}\n", .{ i, actual[i] });
                try ttyconf.setColor(stderr.writer(), .green);
                print("[{d}]: {s}\n", .{ i, expected[i] });
                try ttyconf.setColor(stderr.writer(), .reset);
            }
            continue;
        }
        if (i < actual.len) {
            try ttyconf.setColor(stderr.writer(), .red);
            print("[{d}]: {s}\n", .{ i, actual[i] });
        } else {
            try ttyconf.setColor(stderr.writer(), .green);
            print("[{d}]: {s}\n", .{ i, expected[i] });
        }
        try ttyconf.setColor(stderr.writer(), .reset);
    }
    print("==============================\n", .{});

    return error.TestExpectedEqual;
}

test "properly formats tokens" {
    try expectTokenFormat("Token.slash()", .slash);
    try expectTokenFormat("Token.star()", .star);
    try expectTokenFormat("Token.minus()", .minus);
    try expectTokenFormat("Token.plus()", .plus);
    try expectTokenFormat("Token.identifier(\"veryIdentifying\")", .{ .identifier = "veryIdentifying" });
}

test "lexing with empty string" {
    const expected = &.{};

    try expectLex("", expected);
}

test "lexing with symbols" {
    const expected = &.{ .star, .plus, .slash, .minus };

    try expectLex("* + / -", expected);
}

test "lexing with idents" {
    const expected = &.{ .{ .identifier = "bardzo" }, .{ .identifier = "mocne" }, .plus, .{ .identifier = "slowa" } };

    try expectLex("bardzo mocne + slowa", expected);
}

test "lexing with idents without whitespaces" {
    const expected = &.{ .{ .identifier = "bardzo" }, .{ .identifier = "mocne" }, .plus, .{ .identifier = "slowa" } };

    try expectLex("bardzo mocne+slowa", expected);
}

test "lexing with short idents" {
    const expected = &.{ .{ .identifier = "a" }, .{ .identifier = "b" }, .{ .identifier = "c" } };

    try expectLex("a b c", expected);
}

test "lexing with integers" {
    const expected = &.{ .{ .integer = "123" }, .{ .integer = "456" }, .{ .integer = "789" }, .{ .integer = "000" } };

    try expectLex("123 456 789 000", expected);
}

test "lexing with short integers" {
    const expected = &.{ .{ .integer = "1" }, .{ .integer = "2" }, .{ .integer = "3" }, .{ .integer = "0" } };

    try expectLex("1 2 3 0", expected);
}

test "lexing with parens" {
    const expected = &.{ .{ .integer = "1" }, .star, .left_paren, .{ .integer = "2" }, .plus, .{ .integer = "3" }, .right_paren, .plus, .{ .integer = "0" } };

    try expectLex("1 * (2 + 3) + 0", expected);
}

test "lexing with unsupported error" {
    const expected = &.{};
    const lexResult = expectLex(":3", expected);

    try std.testing.expectError(error.Unsupported, lexResult);
}
