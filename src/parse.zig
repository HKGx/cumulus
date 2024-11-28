const std = @import("std");
const lex = @import("./lex.zig");
const Allocator = std.mem.Allocator;
const Token = lex.Token;

inline fn logDebug(value: anytype, src: std.builtin.SourceLocation) void {
    std.debug.print("[INFO] Current: {any} at {s}:{d}:{d} - {s}\n", .{ value, src.file, src.line, src.column, src.fn_name });
}

const AdditionSign = enum { plus, minus };
const MultiplicationSign = enum { star, slash };

const Expression = union(enum) {
    identifier: []const u8,
    integer: []const u8,
    addition: struct { left: *Expression, right: *Expression, sign: AdditionSign },
    multiplication: struct { left: *Expression, right: *Expression, sign: MultiplicationSign },
    arithmeticNegation: struct { value: *Expression },

    pub fn format(expression: Expression, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Expression.{s}(", .{@tagName(expression)});

        switch (expression) {
            Expression.identifier => |value| try writer.print("\"{s}\"", .{value}),
            Expression.integer => |value| try writer.print("{s}", .{value}),
            Expression.addition => |value| {
                const sign: u8 = if (value.sign == AdditionSign.plus) '+' else '-';
                try writer.print("{} {c} {}", .{ value.left, sign, value.right });
            },
            Expression.multiplication => |value| {
                const sign: u8 = if (value.sign == MultiplicationSign.star) '*' else '/';
                try writer.print("{} {c} {}", .{ value.left, sign, value.right });
            },
            Expression.arithmeticNegation => |value| {
                try writer.print("-{}", .{value.value});
            },
        }

        try writer.writeAll(")");
    }
    pub fn int(value: []const u8) Expression {
        return Expression{ .integer = value };
    }

    pub fn ident(value: []const u8) Expression {
        return Expression{ .identifier = value };
    }

    pub fn add(left: *Expression, right: *Expression) Expression {
        return Expression{ .addition = .{ .left = left, .right = right, .sign = AdditionSign.plus } };
    }

    pub fn sub(left: *Expression, right: *Expression) Expression {
        return Expression{ .addition = .{ .left = left, .right = right, .sign = AdditionSign.minus } };
    }

    pub fn mul(left: *Expression, right: *Expression) Expression {
        return Expression{ .multiplication = .{ .left = left, .right = right, .sign = MultiplicationSign.star } };
    }

    pub fn div(left: *Expression, right: *Expression) Expression {
        return Expression{ .multiplication = .{ .left = left, .right = right, .sign = MultiplicationSign.slash } };
    }

    pub fn neg(value: *Expression) Expression {
        return Expression{ .arithmeticNegation = .{ .value = value } };
    }
};

const Statement = union(enum) {
    expression: *Expression,

    pub fn format(statement: Statement, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Statement.{s}(", .{@tagName(statement)});

        switch (statement) {
            Statement.expression => |value| try writer.print("{};", .{value}),
        }

        try writer.writeAll(")");
    }

    pub fn expr(value: *Expression) Statement {
        return Statement{ .expression = value };
    }
};

const Program = struct {
    statements: []*Statement,
};

const ParserError = error{
    ExpectedRightParen,
    OutOfMemory,
    UnexpectedEndOfInput,
    UnexpectedInput,
};

const Parser = struct {
    source: []const Token,
    current_idx: usize,
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: Allocator, source: []const Token) Parser {
        return Parser{
            .source = source,
            .current_idx = 0,
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    inline fn ended(self: Parser) bool {
        return self.current_idx >= self.source.len;
    }

    inline fn current(self: Parser) ?Token {
        if (self.ended()) return null;

        return self.source[self.current_idx];
    }

    inline fn peek(self: Parser) ?Token {
        if (self.current_idx >= self.source.len - 1) return null;

        return self.source[self.current_idx + 1];
    }

    inline fn advance(self: *Parser) void {
        self.current_idx += 1;
    }

    inline fn isCurrentToken(self: *Parser, tokens: anytype) ?Token {
        const currentToken = self.current() orelse return null;
        inline for (tokens) |value| {
            if (currentToken.equals(value)) return currentToken;
        }
        return null;
    }

    fn parseAddition(self: *Parser) !*Expression {
        logDebug(self.current(), @src());
        var left = try self.parseMultiplication();
        logDebug(self.current(), @src());

        while (self.isCurrentToken(.{ Token.minus, Token.plus })) |signToken| {
            logDebug(self.current(), @src());
            self.advance();
            const right = try self.parseMultiplication();
            const temp_left = left;
            left = try self.arena.allocator().create(Expression);
            left.* = if (signToken == Token.plus) temp_left.add(right) else temp_left.sub(right);
        }
        return left;
    }

    fn parseMultiplication(self: *Parser) !*Expression {
        logDebug(self.current(), @src());
        var left = try self.parseArithmeticNegation();
        logDebug(self.current(), @src());

        while (self.isCurrentToken(.{ Token.star, Token.slash })) |signToken| {
            logDebug(self.current(), @src());
            self.advance();
            const right = try self.parseArithmeticNegation();
            const temp_left = left;
            left = try self.arena.allocator().create(Expression);
            left.* = if (signToken == Token.star) temp_left.mul(right) else temp_left.div(right);
        }

        return left;
    }

    fn parseArithmeticNegation(self: *Parser) !*Expression {
        logDebug(self.current(), @src());
        if (self.isCurrentToken(.{Token.minus})) |_| {
            self.advance();
            const expr = try self.arena.allocator().create(Expression);
            expr.* = (try self.parseValue()).neg();

            return expr;
        }

        return try self.parseValue();
    }

    fn parseParenthesised(self: *Parser) ParserError!*Expression {
        // We know that current char is `(`
        logDebug(self.current(), @src());
        self.advance();
        logDebug(self.current(), @src());
        const expression = try self.parseExpression();
        logDebug(self.current(), @src());

        if (self.isCurrentToken(.{Token.right_paren})) |_| {
            self.advance();
            return expression;
        }

        return if (self.ended()) return ParserError.UnexpectedEndOfInput else error.ExpectedRightParen;
    }

    fn parseValue(self: *Parser) ParserError!*Expression {
        logDebug(self.current(), @src());
        if (self.current()) |value| {
            const expression = try self.arena.allocator().create(Expression);
            switch (value) {
                Token.identifier => |identifier| {
                    self.advance();
                    expression.* = Expression.ident(identifier);
                    return expression;
                },
                Token.integer => |integer| {
                    self.advance();
                    expression.* = Expression.int(integer);
                    return expression;
                },
                Token.left_paren => {
                    return self.parseParenthesised();
                },
                else => return ParserError.UnexpectedInput,
            }
        }

        return ParserError.UnexpectedEndOfInput;
    }

    fn parseExpression(self: *Parser) ParserError!*Expression {
        return try parseAddition(self);
    }

    fn parseStatement(self: *Parser) ParserError!*Statement {
        const statement = try self.arena.allocator().create(Statement);
        logDebug(self.current(), @src());

        statement.* = .{ .expression = try self.parseExpression() };

        return statement;
    }

    pub fn parse(self: *Parser) !*Program {
        var statements = std.ArrayList(*Statement).init(self.arena.allocator());
        errdefer statements.deinit();

        while (self.parseStatement()) |value| {
            logDebug(self.current(), @src());

            try statements.append(value);
        } else |err| {
            std.debug.print("[ERROR]: {?}\n", .{err});
        }

        const program: *Program = try self.arena.allocator().create(Program);
        program.* = .{ .statements = try statements.toOwnedSlice() };

        return program;
    }
};

fn expectParse(source: []const u8, expected: Program) !void {
    var lexer = lex.Lexer.init(std.testing.allocator, source);
    const tokens = try lexer.lex();
    defer tokens.deinit();
    var parser = Parser.init(std.testing.allocator, tokens.items);
    defer parser.deinit();

    const program = try parser.parse();

    try std.testing.expectEqualDeep(expected, program.*);
}

test "parsing with empty string" {
    try expectParse("", .{ .statements = &.{} });
}

test "parsing numbers in parenthesis" {
    var expression = Expression.int("1234");
    var statement = Statement.expr(&expression);
    var statements = [_]*Statement{&statement};

    try expectParse("(1234)", .{ .statements = statements[0..] });
}

test "parsing addition" {
    var left = Expression.int("12");
    var right = Expression.int("34");
    var addition = left.add(&right);
    var statement = Statement.expr(&addition);
    var statements = [_]*Statement{&statement};

    try expectParse("12 + 34", .{ .statements = statements[0..] });
}

test "parsing additions" {
    var left = Expression.int("12");
    var middle = Expression.int("34");
    var right = Expression.int("56");
    var addition1 = left.add(&middle);
    var addition2 = addition1.add(&right);
    var statement = Statement.expr(&addition2);
    var statements = [_]*Statement{&statement};

    try expectParse("12 + 34 + 56", .{ .statements = statements[0..] });
}

test "parsing subtraction" {
    var left = Expression.int("12");
    var right = Expression.int("34");
    var subtraction = left.sub(&right);
    var statement = Statement.expr(&subtraction);
    const program = Program{ .statements = &[_]*Statement{&statement} };

    try expectParse("12 - 34", program);
}

test "parsing subtractions" {
    var left = Expression.int("12");
    var middle = Expression.int("34");
    var right = Expression.int("56");
    var subtraction1 = left.sub(&middle);
    var subtraction2 = subtraction1.sub(&right);
    var statement = Statement.expr(&subtraction2);
    var statements = [_]*Statement{&statement};

    try expectParse("12 - 34 - 56", .{ .statements = statements[0..] });
}

test "parsing mixed addition and subtraction" {
    var term1 = Expression.int("12");
    var term2 = Expression.int("34");
    var term3 = Expression.int("56");
    var term4 = Expression.int("78");
    var addition1 = term1.add(&term2);
    var subtraction1 = addition1.sub(&term3);
    var addition2 = subtraction1.add(&term4);
    var statement = Statement.expr(&addition2);
    var statements = [_]*Statement{&statement};

    try expectParse("12 + 34 - 56 + 78", .{ .statements = statements[0..] });
}

test "parsing multiplication" {
    var left = Expression.int("12");
    var right = Expression.int("34");
    var multiplication = left.mul(&right);
    var statement = Statement.expr(&multiplication);
    var statements = [_]*Statement{&statement};

    try expectParse("12 * 34", .{ .statements = statements[0..] });
}

test "parsing multiplications" {
    var left = Expression.int("12");
    var middle = Expression.int("34");
    var right = Expression.int("56");
    var multiplication1 = left.mul(&middle);
    var multiplication2 = multiplication1.mul(&right);
    var statement = Statement.expr(&multiplication2);
    var statements = [_]*Statement{&statement};

    try expectParse("12 * 34 * 56", .{ .statements = statements[0..] });
}

test "parsing division" {
    var left = Expression.int("12");
    var right = Expression.int("34");
    var division = left.div(&right);
    var statement = Statement.expr(&division);
    var statements = [_]*Statement{&statement};

    try expectParse("12 / 34", .{ .statements = statements[0..] });
}

test "parsing divisions" {
    var left = Expression.int("12");
    var middle = Expression.int("34");
    var right = Expression.int("56");
    var division1 = left.div(&middle);
    var division2 = division1.div(&right);
    var statement = Statement.expr(&division2);
    var statements = [_]*Statement{&statement};

    try expectParse("12 / 34 / 56", .{ .statements = statements[0..] });
}

test "parsing mixed multiplication and division" {
    var term1 = Expression.int("12");
    var term2 = Expression.int("34");
    var term3 = Expression.int("56");
    var term4 = Expression.int("78");
    var multiplication1 = term1.mul(&term2);
    var division1 = multiplication1.div(&term3);
    var multiplication2 = division1.mul(&term4);
    var statement = Statement.expr(&multiplication2);
    var statements = [_]*Statement{&statement};

    try expectParse("12 * 34 / 56 * 78", .{ .statements = statements[0..] });
}

test "parsing mixed multiplication and addition" {
    var term1 = Expression.int("12");
    var term2 = Expression.int("34");
    var term3 = Expression.int("56");
    var term4 = Expression.int("78");
    var division = term2.div(&term3);
    var addition = term1.add(&division);
    var subtraction = addition.sub(&term4);
    var statement = Statement.expr(&subtraction);
    var statements = [_]*Statement{&statement};

    try expectParse("12 + 34 / 56 - 78", .{ .statements = statements[0..] });
}

test "parsing arithmetic negation" {
    var term1 = Expression.int("12");
    var negation = term1.neg();
    var statement = Statement.expr(&negation);
    var statements = [_]*Statement{&statement};

    try expectParse("-12", .{ .statements = statements[0..] });
}

test "parsing arithmetic negation in harder context" {
    var term1 = Expression.int("12");
    var term2 = Expression.int("34");
    var term3 = Expression.int("56");
    var term4 = Expression.int("78");
    var negation = term3.neg();
    var division = term2.div(&negation);
    var addition = term1.add(&division);
    var subtraction = addition.sub(&term4);
    var statement = Statement.expr(&subtraction);
    var statements = [_]*Statement{&statement};

    try expectParse("12 + 34 / -56 - 78", .{ .statements = statements[0..] });
}

test "parsing negation with parenthesis" {
    var term1 = Expression.int("12");
    var term2 = Expression.int("34");
    var term3 = Expression.int("56");
    var addition = term1.add(&term2);
    var negation = addition.neg();
    var multiplication = term3.mul(&negation);
    var statement = Statement.expr(&multiplication);
    var statements = [_]*Statement{&statement};

    try expectParse("56 * -(12 + 34)", .{ .statements = statements[0..] });
}
