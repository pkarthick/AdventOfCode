const std = @import("std");

fn cmpByValue(context: void, a: usize, b: usize) bool {
    return std.sort.desc(usize)(context, a, b);
}


const Point = struct {

    const Self = @This();

    r: usize,
    c: usize,

    fn eql(self: Self, pt: Self) bool {
        return (self.r == pt.r) and (self.c == pt.c);
    }

    fn hash(self: Self, rows: usize) usize {
        return (self.r * rows) + self.c;
    }

};

const data = @embedFile("puzzle");

const MaxElevationIndex = struct {
    maxElevation: u9,
    index: usize
};

const NavigationField = struct {

    const Self = @This();

    rows: usize,
    cols: usize,
    elevation: std.ArrayList([] const u8),
    start: Point,
    end: Point,
    shortest: usize,
    allocator: std.mem.Allocator,
    available: std.ArrayList(NavigationPath),
    processed: std.AutoHashMap(usize, MaxElevationIndex),
    current: usize,

    fn initialPath(self: *Self) !void {
        const path = try NavigationPath.init(self.*);
        try self.available.append(path);
    }

    fn navigateAvailablePaths(self: *Self) !void {

        while (self.available.popOrNull()) |path| {

            std.debug.print("Items remaining: {}\n", .{self.available.items.len});
                        
            const last = path.getLastPoint();
            const elevation = path.getElevation(last);
            
            if (self.processed.get(last.hash(self.rows))) |existingElevationIndex| {
                // std.debug.print("{} {}\n", .{last.r, last.c});
                // std.debug.print("Found existing! Count: {}\n", .{existingCount});
                if (elevation > existingElevationIndex.maxElevation or (elevation == existingElevationIndex.maxElevation and path.points.items.len-1 < existingElevationIndex.index)) {
                    std.debug.print("Elevated: {}!\n", .{elevation});
                    try self.processed.put(last.hash(self.rows), MaxElevationIndex{.maxElevation = elevation, .index = path.points.items.len-1});
                } else {
                    // std.debug.print("Found existing and bigger, skipping!\n", .{});
                    continue;
                }
            } else {
                std.debug.print("Not found: Elevated: {}!\n", .{elevation});
                try self.processed.put(last.hash(self.rows), MaxElevationIndex{.maxElevation = elevation, .index = path.points.items.len-1});
            }

            if (path.isComplete()) {
                std.debug.print("!! Completed: {d} \n", .{path.getCount()});
                const newCount = path.getCount();
                if (newCount < self.shortest) {
                    self.shortest = newCount;
                } 
            } else {
                const paths = try path.availablePaths();

                for (paths.items) |path1| {
                    
                    const last1 = path1.getLastPoint();
                    const elevation1 = path.getElevation(last1);

                    if (self.processed.get(last1.hash(self.rows))) |existingElevationIndex| {
                        if (elevation1 > existingElevationIndex.maxElevation) {
                            std.debug.print("Elevated: {}!\n", .{elevation1});
                            _ = self.processed.remove(last1.hash(self.rows));
                            try self.available.append(path1);
                            // try self.processed.put(last1.hash(self.rows), MaxElevationIndex{.maxElevation = elevation1, .index = path1.points.items.len-1});
                        }
                    } else {
                        std.debug.print("Elevated: {}!\n", .{elevation1});
                        try self.available.append(path1);
                    }
                }
            }
        
        }
    }

};

fn findPoint(elevation: std.ArrayList([] const u8), toFind: u8) ?Point {
    for (elevation.items) |row, r| {
        var c: usize = 0;
        while (c < row.len) : (c += 1) {
            // std.debug.print("c: {c}\n", .{row[c]});
            if (row[c] == toFind) {
                return Point {
                    .r = r,
                    .c = c
                };
            }
        }
    }

    return null;
}

fn createField(allocator: std.mem.Allocator) !NavigationField {

    var lines_it = std.mem.tokenize(u8, data, "\n");
    var elevation = std.ArrayList([] const u8).init(allocator);
    var available = std.ArrayList(NavigationPath).init(allocator);
    var processed = std.AutoHashMap(usize, MaxElevationIndex).init(allocator);
    var rows1: u8 = 0;

    while (lines_it.next()) |line| {
        try elevation.append(line);
        rows1 += 1;
    }

    const cols = elevation.items[0].len;
    const rows = rows1;
    const start = findPoint(elevation, 'S').?;
    const end = findPoint(elevation, 'E').?;

    return NavigationField {
        .rows = rows,
        .cols = cols,
        .elevation = elevation,
        .start = start,
        .end = end,
        .allocator = allocator,
        .shortest = 100000,
        .available = available,
        .processed = processed,
        .current = 0
    };

}

const NavigationPath = struct {
    
    const Self = @This();

    points: std.ArrayList(Point),
    field: NavigationField,
    maxElevationIndex: MaxElevationIndex,

    fn init(field: NavigationField) !Self {

        var points = std.ArrayList(Point).init(field.allocator);
        try points.append(field.start);

        return Self {
            .points = points,
            .field = field,
            .maxElevationIndex = MaxElevationIndex{.maxElevation = 97, .index = 0},
        };

    }

    fn addPoint(self: Self, point: Point) !NavigationPath {
        var points = try self.points.clone();
        try points.append(point);

        var maxElevationIndex = self.maxElevationIndex;
        const el = self.getElevation(point);

        if (el > maxElevationIndex.maxElevation) {
            return NavigationPath {
                .points = points,
                .field = self.field,
                .maxElevationIndex = MaxElevationIndex{.maxElevation = 97, .index = self.points.items.len-1}
            };
        } else {
            return NavigationPath {
                .points = points,
                .field = self.field,
                .maxElevationIndex = self.maxElevationIndex
            };
        }

    }

    fn isComplete(self: Self) bool {
        var last = self.points.items[self.points.items.len - 1];
        return last.eql(self.field.end);
    }

    fn getLastPoint(self: Self) Point {
        return self.points.items[self.points.items.len - 1];
    }

    fn availablePaths(self: Self) !std.ArrayList(NavigationPath) {
        var paths = std.ArrayList(NavigationPath).init(self.field.allocator);
        var last = self.points.items[self.points.items.len - 1];

        if (self.moveRight(last)) |point| {
            if (self.isValid(point)) {
                var path = try self.addPoint(point);
                try paths.append(path);
            }
        }

        if (self.moveDown(last)) |point| {
            if (self.isValid(point)) {
                var path = try self.addPoint(point);
                try paths.append(path);
            }
        }

        if (self.moveLeft(last)) |point| {
            if (self.isValid(point)) {
                var path = try self.addPoint(point);
                try paths.append(path);
            }
        }
        
        if (self.moveUp(last)) |point| {
            if (self.isValid(point)) {
                var path = try self.addPoint(point);
                try paths.append(path);
            }
        }

        return paths;

    }

    fn contains(self:Self, new: Point) bool {
        for (self.points.items) |point| {
            // std.debug.print("{}: {} {} {} {}\n", .{index, point.r, point.c, new.r, new.c});
            if (new.eql(point)) {
                // std.debug.print("{}: {} {} {} {} {}\n", .{index, self.points.items.len, point.r, point.c, new.r, new.c});
                // std.debug.print("Creates cycle!\n\n", .{});
                return true;
            }
        }
        return false;
    }

    fn getElevation(self: Self, point: Point) u8 {

        if (self.field.elevation.items[point.r][point.c] == 'S') {
            return 97;
        }

        return self.field.elevation.items[point.r][point.c];
    }

    fn isValid(self: Self, new: Point) bool {

        const elNew = self.getElevation(new);
        
        const el = self.maxElevationIndex.maxElevation;
        
        if (el == 'z' and elNew == 'E') {
            std.debug.print("Found!!! {}\n", .{self.points.items.len});
        }

        return elNew <= el + 1;
                
    }

    fn moveDown(self:Self, last: Point) ?Point {

        if (last.r + 1 < self.field.rows) {
            const new = Point {.r = last.r + 1, .c = last.c};
            if (self.contains(new)) {
                return null;
            } else {
                return new;
            }
        }

        return null;

    }

    fn moveUp(self:Self, last: Point) ?Point {
        if (last.r > 0) {
            const new = Point {.r = last.r - 1, .c = last.c};
            if (self.contains(new)) {
                return null;
            } else {
                return new;
            }
        }

        return null;
    }

    fn moveRight(self:Self, last: Point) ?Point {

        if (last.c + 1 < self.field.cols) {
            const new = Point {.r = last.r, .c = last.c + 1};
            if (self.contains(new)) {
                return null;
            } else {
                return new;
            }
        }

        return null;

    }

    fn moveLeft(self:Self, last: Point) ?Point {

        if (last.c > 0) {
            const new = Point {.r = last.r, .c = last.c - 1};
            if (self.contains(new)) {
                return null;
            } else {
                return new;
            }
        }

        return null;
    }

    fn getCount(self: Self) usize {
        return self.points.items.len;
    }


};

fn numberOfStepsRequired(allocator: std.mem.Allocator) !usize {
    
    var field: NavigationField = try createField(allocator);

    std.debug.print("end: {} {}\n", .{field.end.r, field.end.c});

    try field.initialPath();
    var shortest: usize = 1000;

    try field.navigateAvailablePaths();

    // try field.findShortestNavigationPath(initial);

    return shortest - 1;

}

pub fn main() !void {

    
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const sample_out = try numberOfStepsRequired(gpa.allocator());
    std.debug.print("Sample {}\n", .{sample_out});

    // const puzzle_out = try findTailVisitCount(puzzle);
    // std.debug.print("{}\n", .{puzzle_out});
 
}