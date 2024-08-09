const std = @import("std");

// const input = @embedFile("sample.txt");
const input = @embedFile("puzzle.txt");

fn split(allocator: std.mem.Allocator, buffer: []const u8, delimiters: []const u8) ![]const []const u8 {
    var list = std.ArrayList([]const u8).init(allocator);

    var iter = std.mem.tokenizeAny(u8, buffer, delimiters);
    while (iter.next()) |item| {
        try list.append(item);
    }

    return list.items;
}

fn readLines(allocator: std.mem.Allocator, buffer: []const u8) ![]const []const u8 {
    return try split(allocator, buffer, "\r\n");
}

const LinesIterator = struct {
    lines: []const []const u8,
    index: usize = 0,
    const Self = @This();

    fn init(allocator: std.mem.Allocator, buffer: []const u8) !Self {
        return Self{ .lines = try readLines(allocator, buffer) };
    }

    fn next(self: *LinesIterator) ?[]const u8 {
        const index = self.index;
        for (self.lines[index..]) |line| {
            self.index += 1;
            return line;
        }
        return null;
    }
};

const Pair = struct { start: usize, count: usize };

const Record = struct {
    allocator: std.mem.Allocator,
    conditions: std.ArrayList(Condition),
    sizes: std.ArrayList(usize),
    conditions_len: u64,
    sizes_len: u64,
    // conditions3: []const Condition,
    // sizes3: []usize,
    count: u64,

    const Self = @This();

    fn init(allocator: std.mem.Allocator, line: []const u8) !Self {
        const frags = try split(allocator, line, " ");

        var sizes_iter = std.mem.tokenizeAny(u8, frags[1], ",");
        var sizes = std.ArrayList(usize).init(allocator);

        while (sizes_iter.next()) |size| {
            const sz: usize = @as(usize, @intCast(try std.fmt.parseInt(i32, size, 10)));
            try sizes.append(sz);
        }

        var conditions = std.ArrayList(Condition).init(allocator);
        for (frags[0]) |cond| {
            try conditions.append(@enumFromInt(cond));
        }

        return Record{ .allocator = allocator, .conditions_len = conditions.items.len, .sizes_len = sizes.items.len, .conditions = conditions, .sizes = sizes, .count = 0 };
    }

    fn count_arrangements_(self: *Record, conditions: *[]const Condition, sizes: []const usize, index: usize) !void {
        // if (index % self.conditions.len == 0) {
        //     std.debug.print("after {} {} {}\n", .{ index / self.conditions.len, index, self.conditions.len });
        // }

        if (sizes.len == 0) {
            return;
        }

        var total: usize = 0;
        for (sizes) |sz| total += sz + 1;

        if (index + total - 1 > conditions.len) {
            return;
        }

        if (index + 1 + sizes[0] <= conditions.len and conditions.*[index] != Condition.Broken) {
            try self.count_arrangements_(conditions, sizes, index + 1);
        }

        if (sizes.len > 0 and index + sizes[0] > conditions.len) {
            // std.debug.print("conditions: {any} ret: 0\n", .{conditions});
            return;
        }

        if (sizes.len > 0 and index + sizes[0] < conditions.len and conditions.*[index + sizes[0]] == Condition.Broken) {
            return;
        }

        for (conditions.*[index .. index + sizes[0]]) |condition| {
            if (condition == Condition.Operational) {
                // std.debug.print("conditions: {any} ret: 0\n", .{conditions});
                return;
            }
        }

        if (sizes.len == 1) {
            if (index + sizes[0] <= conditions.len) {
                for (conditions.*[index + sizes[0] ..]) |condition| {
                    if (condition == Condition.Broken) {
                        return;
                    }
                }
            }

            self.count += 1;
        } else if (sizes.len > 0 and conditions.len > sizes[0]) {
            try self.count_arrangements_(conditions, sizes[1..], index + sizes[0] + 1);
        }
    }

    fn add_set(self: *Record) !void {
        try self.conditions.append(Condition.Unknown);
        for (0..self.conditions_len) |i| {
            try self.conditions.append(self.conditions.items[i]);
        }
        for (0..self.sizes_len) |i| {
            try self.sizes.append(self.sizes.items[i]);
        }

        self.count = 0;
    }

    fn count_arrangements(self: *Record, lines: usize) !u64 {
        var counts = [_]u64{ 0, 0, 0, 0, 0 };

        try self.count_arrangements_(&self.conditions.items, self.sizes.items, 0);
        counts[0] = self.count;
        std.debug.print("line: {} count: {} \n", .{ lines, self.count });

        for (1..5) |i| {
            if (i == 3) {
                const factor1 = counts[1] / counts[0];
                const factor2 = counts[2] / counts[1];

                if (factor1 == factor2) {
                    return counts[0] * factor1 * factor1 * factor1;
                }
            }
            try self.add_set();
            try self.count_arrangements_(&self.conditions.items, self.sizes.items, 0);
            counts[i] = self.count;
            std.debug.print("line: {} count: {} \n", .{ lines, self.count });
        }

        std.debug.print("line: {} count: {} \n\n", .{ lines, self.count });
        return self.count;
    }
};

const Condition = enum(u8) {
    Broken = 35,
    Operational = 46,
    Unknown = 63,
};

pub fn main() !void {
    var lines: usize = 0;
    var total: u64 = 0;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var lines_iter = try LinesIterator.init(allocator, input);

    while (lines_iter.next()) |line| {
        var record = try Record.init(allocator, line);

        // std.debug.print("record: {} \n", .{record});

        const count = try record.count_arrangements(lines);
        total += count;

        lines += 1;
    }

    std.debug.print("lines: {d} total: {}\n", .{ lines, total });
}
