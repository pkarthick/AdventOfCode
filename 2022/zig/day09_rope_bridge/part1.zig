const std = @import("std");

const sample = @embedFile("sample");
const puzzle = @embedFile("puzzle");

const Position = struct {x: u32, y:32};

fn findTailVisitCount(lines: []const u8) !usize {
    const gpa = std.heap.GeneralPurposeAllocator(.{}){};
    
    var map = std.AutoHashMap(Position, u32).init(
        gpa.allocator(),
    );
    defer map.deinit();

    var line_it = std.mem.tokenize(u8, lines, "\n");

    var total: usize = 0;

    while (line_it.next()) |line| {
        var tokens_it = std.mem.tokenize(u8, line, " ");
        const dir = line[0];
        _ = tokens_it.next().?;

        const count = try std.fmt.parseInt(u8, tokens_it.next().?, 10);

        std.debug.print("{c} {}\n", .{dir, count});

        total += 1;
    }

    return total;
}

pub fn main() !void {

    const sample_out = try findTailVisitCount(sample);
    std.debug.print("{}\n", .{sample_out});

    // const puzzle_out = try findTailVisitCount(puzzle);
    // std.debug.print("{}\n", .{puzzle_out});
 
}