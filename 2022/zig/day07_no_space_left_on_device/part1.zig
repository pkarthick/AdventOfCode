const std = @import("std");

const input = @embedFile("sample");
const root_name: []const u8 = "/";

const Dir = struct {
    const Self = @This();

    name: []const u8,
    // path: []const u8,
    sub_dirs: std.StringHashMap(*Dir),
    parent: *Dir = undefined,
    files: std.ArrayList(File),

    fn root(allocator: std.mem.Allocator) Self {
        return Dir{ .name = root_name, .sub_dirs = std.StringHashMap(*Dir).init(allocator), .files = std.ArrayList(File).init(allocator) };
    }

    fn init(parent: *Dir, allocator: std.mem.Allocator, name: []const u8) Self {
        return Self{ .name = name, .parent = parent, .sub_dirs = std.StringHashMap(*Dir).init(allocator), .files = std.ArrayList(File).init(allocator) };
    }

    fn populate(self: *Dir, allocator: std.mem.Allocator, it: *std.mem.TokenIterator(u8)) !void {
        // std.debug.print("Populating directory {s}\n", .{self.name});
        std.debug.print("self name {s}\n", .{self.name});
        while (it.peek()) |line| {
            // std.debug.print("Inside! {s}", .{line});
            if (line[0] == '$') break;
            _ = it.next();
            var tokens_it = std.mem.tokenize(u8, line, " ");
            const token: []const u8 = tokens_it.next().?;
            // std.debug.print("{s}", .{token});
            if (std.mem.eql(u8, token, "dir")) {
                const name = tokens_it.next().?;
                std.debug.print("{s}\n", .{name});
                // std.debug.print("\n", .{});
                std.debug.print("parent dir passed to the child: {s}\n", .{self.name});
                var dir = self.init(allocator, name);
                // std.debug.print("After create\n", .{});
                std.debug.print("before {d}\n", .{self.sub_dirs.count()});
                try self.sub_dirs.put(name, &dir);
                std.debug.print("after {d}\n", .{self.sub_dirs.count()});
                // std.debug.print("After getorput\n", .{});
            } else {
                const size: usize = try std.fmt.parseInt(usize, token, 10);
                const name = tokens_it.next().?;
                try self.files.append(File{ .size = size, .name = name });
            }
        }
    }
};

const File = struct { name: []const u8, size: usize };

const EntryType = enum { cd, ls, dir, file };

const Entry = union(EntryType) { cd: []const u8, ls, dir: []const u8, file: File };

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var root_dir = Dir.root(allocator);
    // var current_dir: *Dir = undefined;
    var dirs = std.ArrayList(*Dir).init(allocator);
    try dirs.append(&root_dir);

    var line_it = std.mem.tokenize(u8, input, "\n");
    while (line_it.next()) |line| {
        std.debug.print("{s}\n", .{line});
        var tokens_it = std.mem.tokenize(u8, line, " ");
        const token: []const u8 = tokens_it.next().?;
        // std.debug.print("{s}\n", .{token});

        if (std.mem.eql(u8, token, "$")) {
            const command = tokens_it.next().?;
            if (std.mem.eql(u8, command, "cd")) {
                const name = tokens_it.next().?;
                if (std.mem.eql(u8, name, "/")) {
                    // current_dir = root_dir;
                    dirs.clearRetainingCapacity();
                    try dirs.append(&root_dir);
                } else if (std.mem.eql(u8, name, "..")) {
                    // std.debug.print("current_dir: {s}\n", .{current_dir.name});
                    // std.debug.print("parent: {s}\n", .{current_dir.?.parent.?.name});
                    _ = dirs.pop();
                    // current_dir = dirs.items[dirs.items.len - 1];
                    // current_dir = current_dir.?.parent.?.*;
                    // std.debug.print("parent_dir: {s}\n", .{current_dir.name});
                } else {
                    var current_dir = dirs.items[dirs.items.len - 1];
                    std.debug.print("current_dir: {s}\n", .{current_dir.name});
                    // std.debug.print("child dir: {s}\n", .{name});
                    var child_dir = current_dir.sub_dirs.get(name).?;
                    try dirs.append(child_dir);
                    var current_dir1 = dirs.items[dirs.items.len - 1];
                    std.debug.print("current_dir: {s}\n", .{current_dir1.name});
                }
            } else if (std.mem.eql(u8, command, "ls")) {
                // if (current_dir) |*cur| {
                // std.debug.print("current_dir: {s}\n", .{current_dir.name});
                var current_dir = dirs.items[dirs.items.len - 1];
                std.debug.print("current_dir: {s}\n", .{current_dir.name});
                try current_dir.populate(allocator, &line_it);
                // }
            }
        }

        //     while (tokens_it.next()) |token| {
        //         std.debug.print("{s}\n", .{token});
        //     }
    }
    std.debug.print("Hello!\n", .{});
}
