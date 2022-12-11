const std = @import("std");

const data = @embedFile("sample");
// const data = @embedFile("puzzle");

const WorryLevel = struct {
    index: usize,
    level: usize,
};

const Monkey = struct {
    
    const Self = @This();

    things: std.ArrayList(usize), 
    op: [3][]const u8, 
    divisor: usize, 
    success: usize, 
    failure: usize,
    inspected: usize = 0,

    fn execOp(self: Self, old: usize) !usize {
        const op1 = self.op[0];
        const op= self.op[1];
        const op2 = self.op[2];

        if (std.mem.eql(u8, op1, "old")) {
            if (std.mem.eql(u8, op2, "old")) {
                if (std.mem.eql(u8, op, "*")) {
                    return old * old;
                } else {
                    return old + old;
                }
            } else {
                const operand = try std.fmt.parseInt(usize, op2, 10);
                // std.debug.print("{} {}\n", .{operand, old});
                if (std.mem.eql(u8, op, "*")) {
                    return old * operand;
                } else {
                    return old + operand;
                }
            }
        }

        return 0;
    }

    fn inspectThings(self: *Self, allocator: std.mem.Allocator) !std.ArrayList(WorryLevel) {

        self.inspected += self.things.items.len;

        var result = std.ArrayList(WorryLevel).init(allocator);

        while (self.things.items.len > 0) {
            const thing = self.things.orderedRemove(0);
            const level = @divTrunc(try self.execOp(thing), 3);

            if (level % self.divisor == 0) {
                try result.append(WorryLevel{.level=level, .index=self.success});
            } else {
                try result.append(WorryLevel{.level=level, .index=self.failure});
            }
        }

        return result;

    }

};

fn parseMonkey(allocator: std.mem.Allocator, line_it: *std.mem.TokenIterator(u8)) !Monkey {
    
    const startingItems = line_it.next().?;
    var tokens_it_1 = std.mem.tokenize(u8, startingItems, ":");
    
    _ = tokens_it_1.next();
    var items_it_1 = std.mem.tokenize(u8, tokens_it_1.next().?, ", ");
    
    var things = std.ArrayList(usize).init(allocator);
    
    while (items_it_1.next()) |item| {
        // std.debug.print("item {s}\n", .{item});
        const num = try std.fmt.parseInt(usize, item, 10);
        try things.append(num);
    }


    var tokens_it_2 = std.mem.tokenize(u8, line_it.next().?, "=");
    _ = tokens_it_2.next();
    var ops_it = std.mem.tokenize(u8, tokens_it_2.next().?, " ");
    const op1 = ops_it.next().?;
    const op = ops_it.next().?;
    const op2 = ops_it.next().?;

    const ops = [3][]const u8{op1, op, op2};
    // std.debug.print("ops {any}\n", .{ops});

    var tokens_it_3 = std.mem.tokenize(u8, line_it.next().?, " ");

    var i: u8 = 0;
    while (i < 3) : (i+=1) {
        _ = tokens_it_3.next();
    }

    const divisor = try std.fmt.parseInt(u8, tokens_it_3.next().?, 10);
    // std.debug.print("divisor {d}\n", .{divisor});

    var tokens_it_4 = std.mem.tokenize(u8, line_it.next().?, " ");

    var ind2: u8 = 0;
    while (ind2 < 5) : (ind2 += 1) {
        _ = tokens_it_4.next();
    }

    const success = try std.fmt.parseInt(u8, tokens_it_4.next().?, 10);
    // std.debug.print("success {d}\n", .{success});


    var tokens_it_5 = std.mem.tokenize(u8, line_it.next().?, " ");

    var ind3: u8 = 0;
    while (ind3 < 5) : (ind3+=1) {
        _ = tokens_it_5.next();
    }

    const failure = try std.fmt.parseInt(u8, tokens_it_5.next().?, 10);
    // std.debug.print("failure {d}\n", .{failure});

    const monkey = Monkey {.things = things, .op = ops, .divisor = divisor, .success = success, .failure=failure};

    return monkey;
}

fn readMonkeys(allocator: std.mem.Allocator) !std.ArrayList(Monkey) {
        
    var line_it = std.mem.tokenize(u8, data, "\n");

    var monkeys = std.ArrayList(Monkey).init(allocator);

    while (line_it.next()) |_| {
        var monkey = try parseMonkey(allocator, &line_it);
        try monkeys.append(monkey);
    }

    return monkeys;
}

fn cmpByValue(context: void, a: usize, b: usize) bool {
    return std.sort.desc(usize)(context, a, b);
}

fn findLevelOfMonkeyBusiness(allocator: std.mem.Allocator) !usize {
    const monkeys = try readMonkeys(allocator);
    
    var i: usize = 0;

    while (i < 20) : (i += 1) {
        for (monkeys.items) |*monkey| {
            var toMove = try monkey.inspectThings(allocator);
            for (toMove.items) |worryLevel| {
                try monkeys.items[worryLevel.index].things.append(worryLevel.level);
            }
        }
    }

    var inspected = std.ArrayList(usize).init(allocator);

    for (monkeys.items) |*monkey| {
        try inspected.append(monkey.inspected);
    }

    std.sort.sort(usize, inspected.items, {}, cmpByValue);

    return inspected.items[0] * inspected.items[1];

}

pub fn main() !void {

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const sample_out = try findLevelOfMonkeyBusiness(gpa.allocator());
    std.debug.print("Sample {}\n", .{sample_out});

    // const puzzle_out = try findTailVisitCount(puzzle);
    // std.debug.print("{}\n", .{puzzle_out});
 
}