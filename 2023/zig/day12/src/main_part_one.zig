//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");

const input = @embedFile("input.txt");

fn split(allocator: std.mem.Allocator, buffer: []const u8, delimiters: []const u8) ![]const []const u8 {
    var list = std.ArrayList([]const u8).init(allocator);

    var iter = std.mem.tokenize(u8, buffer, delimiters);
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
    conditions: []const Condition,
    sizes: []usize,
    existing: std.StringHashMap(bool),

    const Self = @This();

    fn init(allocator: std.mem.Allocator, line: []const u8) !Self {
        const frags = try split(allocator, line, " ");

        var sizes_iter = std.mem.tokenize(u8, frags[1], ",");
        var sizes = std.ArrayList(usize).init(allocator);

        while (sizes_iter.next()) |size| {
            const sz: usize = @as(usize, @intCast(try std.fmt.parseInt(i32, size, 10)));
            try sizes.append(sz);
        }

        var conditions = std.ArrayList(Condition).init(allocator);
        for (frags[0]) |cond| {
            try conditions.append(@enumFromInt(cond));
        }

        return Record{ .allocator = allocator, .conditions = conditions.items, .sizes = sizes.items, .existing = std.StringHashMap(bool).init(allocator) };
    }

    fn get_key(self: *Record, conditions: []const Condition) ![]const u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        for (conditions) |condition| {
            try list.append(@intFromEnum(condition));
        }
        return list.items;
    }

    // fn get_key(self: *Record, pairs: std.ArrayList(Pair)) ![]const u8 {
    //     var list = std.ArrayList(u8).init(self.allocator);

    //     for (pairs.items) |pair| {
    //         var buf: [256]u8 = undefined;
    //         const str = try std.fmt.bufPrint(&buf, "{}-{} ", .{ pair.start, pair.count });
    //         try list.appendSlice(str);
    //     }

    //     return list.items;
    // }

    fn count_arrangements_(self: *Record, conditions: []const Condition, sizes: []const usize, index: usize) !void {
        if (sizes.len == 0) {
            return;
        }

        var total: usize = 0;
        for (sizes) |sz| total += sz;

        if (index + total > conditions.len) {
            return;
        }

        // std.debug.print("sizes:{any} index: {}\n", .{ sizes, index });

        if (index + 1 + sizes[0] <= conditions.len and conditions[index] != Condition.Broken) {
            try self.count_arrangements_(conditions, sizes, index + 1);
        }

        if (index < conditions.len and conditions[index] == Condition.Operational) {
            try self.count_arrangements_(conditions, sizes, index + 1);
        }

        if (sizes.len > 0 and index + sizes[0] > conditions.len) {
            // std.debug.print("conditions: {any} ret: 0\n", .{conditions});
            return;
        }

        for (conditions[index .. index + sizes[0]]) |condition| {
            if (condition == Condition.Operational) {
                // std.debug.print("conditions: {any} ret: 0\n", .{conditions});
                return;
            }
        }

        if (sizes.len > 0 and index + sizes[0] < conditions.len and conditions[index + sizes[0]] == Condition.Broken) {
            return;
        }

        var conditions1 = std.ArrayList(Condition).init(self.allocator);

        for (0..conditions.len) |i| {
            try conditions1.append(conditions[i]);
        }

        for (0..sizes[0]) |i| {
            conditions1.items[index + i] = Condition.Broken;
        }

        if (index + sizes[0] < conditions.len) {
            conditions1.items[index + sizes[0]] = Condition.Operational;
        }

        if (sizes.len == 1) {
            // std.debug.print("Here 5\n", .{});

            if (index + sizes[0] <= conditions.len) {
                for (conditions[index + sizes[0] ..]) |condition| {
                    if (condition == Condition.Broken) {
                        return;
                    }
                }
            }

            // std.debug.print("Here 6\n", .{});

            const s = try self.get_key(conditions1.items);
            // std.debug.print("final: {s}\n", .{s});
            try self.existing.put(s, true);

            // std.debug.print("conditions: {any} sizes: {any} ret: 1\n", .{ conditions, sizes });
        }

        if (conditions.len > 0 and sizes.len > 0) {
            // std.debug.print("Here 7\n", .{});

            if (conditions.len > sizes[0]) {
                // std.debug.print("Here 8\n", .{});
                try self.count_arrangements_(conditions1.items, sizes[1..], index + sizes[0] + 1);
            }
        }
    }

    // fn count_arrangements_(self: *Record, conditions: []const Condition, sizes: []const usize, index: usize, pairs: std.ArrayList(Pair)) !void {
    //     if (sizes.len == 0) {
    //         return;
    //     }

    //     if (conditions.len > 0 and conditions[0] == Condition.Operational) {
    //         try self.count_arrangements_(conditions[1..], sizes, index + 1, pairs);
    //     }

    //     if (sizes.len > 0 and conditions.len < sizes[0]) {
    //         // std.debug.print("conditions: {any} ret: 0\n", .{conditions});
    //         return;
    //     }

    //     for (conditions[0..sizes[0]]) |condition| {
    //         if (condition == Condition.Operational) {
    //             // std.debug.print("conditions: {any} ret: 0\n", .{conditions});
    //             return;
    //         }
    //     }

    //     if (sizes.len > 0 and conditions.len > sizes[0] and conditions[sizes[0]] == Condition.Broken) {
    //         return;
    //     }

    //     if (conditions.len >= sizes[0] and sizes.len == 1) {
    //         if (conditions.len > sizes[0]) {
    //             for (conditions[sizes[0]..]) |condition| {
    //                 if (condition == Condition.Broken) {
    //                     return;
    //                 }
    //             }
    //         }

    //         var pairs1 = try pairs.clone();
    //         try pairs1.append(Pair{ .start = index, .count = sizes[0] });

    //         const s = try self.get_key(pairs1);
    //         std.debug.print("final: {s}\n", .{s});
    //         try self.existing.put(s, true);

    //         // std.debug.print("conditions: {any} sizes: {any} ret: 1\n", .{ conditions, sizes });
    //     }

    //     if (conditions.len > 0 and sizes.len > 0) {
    //         try self.count_arrangements_(conditions[1..], sizes, index + 1, pairs);
    //         if (conditions.len > sizes[0]) {
    //             var pairs1 = try pairs.clone();
    //             try pairs1.append(Pair{ .start = index, .count = sizes[0] });
    //             try self.count_arrangements_(conditions[sizes[0] + 1 ..], sizes[1..], index + sizes[0] + 1, pairs1);
    //         }
    //     }
    // }

    fn count_arrangements(self: *Record) !usize {
        try self.count_arrangements_(self.conditions, self.sizes, 0);
        std.debug.print("count: {} \n", .{self.existing.count()});
        return self.existing.count();
    }
};

const Condition = enum(u8) {
    Broken = 35,
    Operational = 46,
    Unknown = 63,
};

pub fn main() !void {
    var lines: usize = 0;
    var total: usize = 0;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var lines_iter = try LinesIterator.init(allocator, input);

    while (lines_iter.next()) |line| {
        var record = try Record.init(allocator, line);

        // std.debug.print("record: {} \n", .{record});

        const count = try record.count_arrangements();
        total += count;

        lines += 1;
    }

    std.debug.print("lines: {d} total: {d}\n", .{ lines, total });
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "fuzz example" {
    // Try passing `--fuzz` to `zig build` and see if it manages to fail this test case!
    const input_bytes = std.testing.fuzzInput(.{});
    try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input_bytes));
}
