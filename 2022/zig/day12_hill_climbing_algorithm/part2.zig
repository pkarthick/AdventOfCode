const std = @import("std");

const WorryLevelTarget = struct {
    target: usize,
    level: usize,
};

const OperandType = enum(u8) {
    parameter,
    literal
};

const Operand = union(OperandType) {
    parameter,
    literal: usize
};

const OperationType = enum(u8) {
    multiply,
    add
};

const Operation = union(OperationType) {

    const Self = @This();

    multiply: [2] Operand,
    add: [2] Operand,

    fn init(line: []const u8) !Operation {
        var tokens_it = std.mem.tokenize(u8, line, "=");
        _ = tokens_it.next();
        var ops_it = std.mem.tokenize(u8, tokens_it.next().?, " ");

        const op1 = ops_it.next().?;
        const op = ops_it.next().?;
        const op2 = ops_it.next().?;

        var ops: Operation = undefined;
        const param = Operand{.parameter = {}};

        if (std.mem.eql(u8, op1, "old")) {
            if (std.mem.eql(u8, op2, "old")) {
                if (std.mem.eql(u8, op, "*")) {
                    ops = Operation {.multiply = [2] Operand{ param, param } };
                } else {
                    ops = Operation {.add = [2] Operand{ param, param } };
                }
            } else {
                const operand = try std.fmt.parseInt(usize, op2, 10);
                // std.debug.print("{} {}\n", .{operand, old});
                if (std.mem.eql(u8, op, "*")) {
                    ops = Operation {.multiply = [2] Operand{ param, Operand{.literal = operand} } };
                } else {
                    ops = Operation {.add = [2] Operand{ param, Operand{.literal = operand} } };
                }
            }
        }
        return ops;
    }
    
    fn execute(self: Self, param_value: usize) usize {
        switch(self) {
            .multiply => |operands| {
                const op1 = switch (operands[0]) {
                    .parameter => param_value,
                    .literal => |val| val, 
                };
                const op2 = switch (operands[1]) {
                    .parameter => param_value,
                    .literal => |val| val, 
                };
                return op1 * op2;
            },
            .add => |operands| {
                const op1 = switch (operands[0]) {
                    .parameter => param_value,
                    .literal => |val| val, 
                };
                const op2 = switch (operands[1]) {
                    .parameter => param_value,
                    .literal => |val| val, 
                };
                return op1 + op2;
            }
        }
    }

};

const Things = struct {

    const Self = @This();

    items: std.ArrayList(usize),

    fn init(allocator: std.mem.Allocator, line: []const u8) !Self {
        var tokens_it = std.mem.tokenize(u8, line, ":");
        
        _ = tokens_it.next();
        var items_it = std.mem.tokenize(u8, tokens_it.next().?, ", ");
        
        var things = std.ArrayList(usize).init(allocator);
        
        while (items_it.next()) |item| {
            // std.debug.print("item {s}\n", .{item});
            const num = try std.fmt.parseInt(usize, item, 10);
            try things.append(num);
        }

        return Self {
            .items = things
        };
    }


};

const Monkey = struct {
    
    const Self = @This();

    things: Things, 
    op: Operation, 
    divisor: usize, 
    success: usize, 
    failure: usize,
    inspected: usize = 0,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, line_it: *std.mem.TokenIterator(u8)) !Monkey {
        
        const things = try Things.init(allocator, line_it.next().?);
        const op = try Operation.init(line_it.next().?);
        const divisor = try parseLine(line_it.next().?, 3);
        const success = try parseLine(line_it.next().?, 5);
        const failure = try parseLine(line_it.next().?, 5);
        return Monkey {.things = things, .op = op, .divisor = divisor, .success = success, .failure=failure, .allocator = allocator};
        
    }

    fn parseThings(allocator: std.mem.Allocator, line: []const u8) !std.ArrayList(usize) {
        var tokens_it = std.mem.tokenize(u8, line, ":");
        
        _ = tokens_it.next();
        var items_it = std.mem.tokenize(u8, tokens_it.next().?, ", ");
        
        var things = std.ArrayList(usize).init(allocator);
        
        while (items_it.next()) |item| {
            // std.debug.print("item {s}\n", .{item});
            const num = try std.fmt.parseInt(usize, item, 10);
            try things.append(num);
        }
        return things;
    }

    fn parseOperation(line: []const u8) !Operation {
        var tokens_it = std.mem.tokenize(u8, line, "=");
        _ = tokens_it.next();
        var ops_it = std.mem.tokenize(u8, tokens_it.next().?, " ");

        const op1 = ops_it.next().?;
        const op = ops_it.next().?;
        const op2 = ops_it.next().?;

        var ops: Operation = undefined;
        const param = Operand{.parameter = {}};

        if (std.mem.eql(u8, op1, "old")) {
            if (std.mem.eql(u8, op2, "old")) {
                if (std.mem.eql(u8, op, "*")) {
                    ops = Operation {.multiply = [2] Operand{ param, param } };
                } else {
                    ops = Operation {.add = [2] Operand{ param, param } };
                }
            } else {
                const operand = try std.fmt.parseInt(usize, op2, 10);
                // std.debug.print("{} {}\n", .{operand, old});
                if (std.mem.eql(u8, op, "*")) {
                    ops = Operation {.multiply = [2] Operand{ param, Operand{.literal = operand} } };
                } else {
                    ops = Operation {.add = [2] Operand{ param, Operand{.literal = operand} } };
                }
            }
        }
        return ops;
    }

    fn parseLine(line: [] const u8, skip: usize) !usize {
        var it = std.mem.tokenize(u8, line, " ");

        var i: u8 = 0;
        while (i < skip) : (i+=1) {
            _ = it.next();
        }

        return try std.fmt.parseInt(u8, it.next().?, 10);
    }

    fn inspectThings(self: *Self, modValue: usize) !std.ArrayList(WorryLevelTarget) {

        var result = std.ArrayList(WorryLevelTarget).init(self.allocator);

        while (self.things.items.items.len > 0) {
            self.inspected += 1;
            const thing = self.things.items.orderedRemove(0);
            const level1 = self.op.execute(thing);
            const level = level1 % modValue;

            if (level % self.divisor == 0) {
                try result.append(WorryLevelTarget{.level=level, .target=self.success});
            } else {
                try result.append(WorryLevelTarget{.level=level, .target=self.failure});
            }
        }

        return result;

    }

};

const Inspector = struct {

    const Self = @This();

    monkeys: std.ArrayList(Monkey),
    modValue: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, data: [] const u8) !Self {

        var monkeys = std.ArrayList(Monkey).init(allocator);
        var modValue: usize = 1;

        var line_it = std.mem.tokenize(u8, data, "\n");

        while (line_it.next()) |_| {
            var monkey = try Monkey.init(allocator, &line_it);
            modValue *= monkey.divisor;
            try monkeys.append(monkey);
        }

        // std.debug.print("{}\n", .{modValue});

        return Self {
            .monkeys = monkeys,
            .modValue = modValue,
            .allocator = allocator
        };

    }


    fn findLevelOfMonkeyBusiness(self: Self) !usize {
        
        var i: usize = 0;

        while (i < 10000) : (i += 1) {
            for (self.monkeys.items) |*monkey| {
                var toMove = try monkey.inspectThings(self.modValue);
                for (toMove.items) |worryLevel| {
                    try self.monkeys.items[worryLevel.target].things.items.append(worryLevel.level);
                }
            }
        }

        var inspected = std.ArrayList(usize).init(self.allocator);

        for (self.monkeys.items) |*monkey| {
            try inspected.append(monkey.inspected);
        }

        std.sort.sort(usize, inspected.items, {}, cmpByValue);

        return inspected.items[0] * inspected.items[1];

    }

    fn cmpByValue(context: void, a: usize, b: usize) bool {
        return std.sort.desc(usize)(context, a, b);
    }


};

test "sample input" { 
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const inspector = try Inspector.init(gpa.allocator(), @embedFile("sample"));
    const result = try inspector.findLevelOfMonkeyBusiness();
    std.debug.print("Sample {}\n", .{result});
    try std.testing.expect(result == 2713310158);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const inspector = try Inspector.init(gpa.allocator(), @embedFile("puzzle"));
    const result = try inspector.findLevelOfMonkeyBusiness();
    std.debug.print("Puzzle {}\n", .{result});
}