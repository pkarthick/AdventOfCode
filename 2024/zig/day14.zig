const std = @import("std");

const input = @embedFile("14.input");

const height: isize = 103;
const width: isize = 101;

const Robot = struct {
    x: isize,
    y: isize,
    xv: isize,
    yv: isize,

    var Self = @This();

    pub fn init(allocator: std.mem.Allocator, line: []const u8) !*Robot {
        const delimiters = [_]u8{ ' ', ',', '=', 'p', 'v' };

        var coords = std.ArrayList([]const u8).init(allocator);
        defer coords.deinit();

        var it2 = std.mem.tokenize(u8, line, &delimiters);

        while (it2.next()) |w| {
            if (!std.mem.eql(u8, w, "")) {
                try coords.append(w);
            }
        }

        const x = try std.fmt.parseInt(isize, coords.items[0], 10);
        const y = try std.fmt.parseInt(isize, coords.items[1], 10);
        const xv = try std.fmt.parseInt(isize, coords.items[2], 10);
        const yv = try std.fmt.parseInt(isize, coords.items[3], 10);

        // std.debug.print("{} {} {} {}\n", .{ x, y, xv, yv });

        return &Robot{
            .x = x,
            .y = y,
            .xv = xv,
            .yv = yv,
        };
        // std.debug.print("{}\n", .{coords.items.len});
    }

    fn move(self: *Robot) void {
        self.*.x = @mod(self.*.x + self.*.xv, width);
        self.*.y = @mod(self.*.y + self.*.yv, height);

        // const robot = self.*;
        // std.debug.print("{} {} {} {}\n", .{ robot.x, robot.y, robot.xv, robot.yv });

        // self.* = Robot{ .x = x, .y = y, .xv = self.xv, .yv = self.yv };
    }

    fn same(self: Robot, r: isize, c: isize) bool {
        return self.x == c and self.y == r;
    }
};

fn has_robot_at(robots: [500]*Robot, r: isize, c: isize) bool {
    for (robots) |robot| {
        if (robot.same(r, c)) {
            return true;
        }
    }

    return false;
}

fn has_christmas_tree(robots: [500]*Robot) !bool {
    var r: isize = 0;
    var c: isize = 0;
    var count: usize = 0;

    while (r < height) : (r += 1) {
        count = 0;
        while (c < width) : (c += 1) {
            if (has_robot_at(robots, r, c)) {
                count += 1;
                std.debug.print("count: {}\n", .{count});
                if (count == 9) {
                    return true;
                }
            } else {
                count = 0;
            }
        }
    }

    return false;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var it = std.mem.splitScalar(u8, input, '\n');
    var robots_list = std.ArrayList(Robot).init(allocator);
    defer robots_list.deinit();

    var robots: [500]*Robot = undefined;
    var i: usize = 0;
    // var robot: Robot = undefined;

    while (it.next()) |line| {
        // std.debug.print("{s}\n", .{line});
        // robot = try Robot.init(allocator, line);
        // robots.append(robot);
        robots[i] = try Robot.init(allocator, line);
        i += 1;

        // try robots_list.append(robot);
        // std.debug.print("{} {} {} {}\n", .{ robot.x, robot.y, robot.xv, robot.yv });
    }

    // std.debug.print("robots count: {}\n", .{robots.items.len});
    //

    for (0..10000) |seconds| {
        for (0..robots.len) |ri| {

            // std.debug.print("{}\n", .{i});
            var robot = robots[ri];
            // std.debug.print("before: {} {} {} {}\n", .{ robot.x, robot.y, robot.xv, robot.yv });
            robot.move();
            // std.debug.print("after: {} {} {} {}\n\n", .{ robot.x, robot.y, robot.xv, robot.yv });
        }
        // std.debug.print("{}\n", .{seconds});
        if (try has_christmas_tree(robots)) {
            std.debug.print("Tooks {} seconds to form christmas tree!\n", .{seconds});
            break;
        }
    }
}
