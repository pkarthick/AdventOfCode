const std = @import("std");

const sample = @embedFile("sample");
const puzzle = @embedFile("puzzle");

const ActivityKind = enum { Move, Open };

const Data = struct {
    connections: std.ArrayList(std.ArrayList(u64)),
    pressures: std.ArrayList(u64),
    openable: std.ArrayList(bool),
    start_valve: u64,
    openable_count: u64,
    available_minutes: u64,
    total_pressure: u64,

    const Self = @This();

    fn init(buffer: []const u8, allocator: std.mem.Allocator) !Self {
        var connections = std.ArrayList(std.ArrayList(u64)).init(allocator);
        var pressures = std.ArrayList(u64).init(allocator);
        var openable = std.ArrayList(bool).init(allocator);

        var connection_names = std.ArrayList(std.ArrayList([]const u8)).init(allocator);
        defer connection_names.deinit();

        var line_iter = std.mem.tokenizeAny(u8, buffer, "\n");

        var valves_name_index_map = std.StringHashMap(u64).init(allocator);
        defer valves_name_index_map.deinit();

        var len: u64 = 0;
        var start_valve: u64 = 0;
        var openable_count: u64 = 0;
        var total_pressure: u64 = 0;

        while (line_iter.next()) |line| {
            var tokens_iter = std.mem.tokenizeAny(u8, line, " ;=,");

            //Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
            _ = tokens_iter.next(); // Valve

            var name = tokens_iter.next().?; //{Name}
            if (std.mem.eql(u8, name, "AA")) {
                start_valve = len;
            }

            try valves_name_index_map.putNoClobber(name, len);
            _ = tokens_iter.next(); // has
            _ = tokens_iter.next(); // flow
            _ = tokens_iter.next(); // rate

            var rate = tokens_iter.next().?; // {rate}
            const pressure: u64 = try std.fmt.parseInt(u64, rate, 10);

            total_pressure += pressure;

            try pressures.append(pressure);
            const is_openable = pressure > 0;
            try openable.append(is_openable);

            if (is_openable) {
                openable_count += 1;
            }

            _ = tokens_iter.next(); // tunnel
            _ = tokens_iter.next(); // leads
            _ = tokens_iter.next(); // to
            _ = tokens_iter.next(); // valve or valves

            var valve_connection_names = std.ArrayList([]const u8).init(allocator);

            // read all valve names
            while (tokens_iter.next()) |token| {
                try valve_connection_names.append(token);
            }

            try connection_names.append(valve_connection_names);
            len += 1;
        }

        for (connection_names.items) |names| {
            var indices = std.ArrayList(u64).init(allocator);
            for (names.items) |name| {
                if (valves_name_index_map.get(name)) |index| {
                    try indices.append(index);
                }
            }
            try connections.append(indices);
            defer names.deinit();
        }
        return Data{
            .start_valve = start_valve,
            .connections = connections,
            .pressures = pressures,
            .openable = openable,
            .openable_count = openable_count,
            .available_minutes = 30,
            .total_pressure = total_pressure,
        };
    }

    fn deinit(self: Self) void {
        self.pressures.deinit();

        for (self.connections.items) |x| {
            x.deinit();
        }

        self.connections.deinit();
        self.openable.deinit();
    }
};

const OpenValves = struct {
    opened: std.ArrayList(u64),
    allocator: std.mem.Allocator,

    const Self = @This();

    fn init(allocator: std.mem.Allocator) Self {
        var opened = std.ArrayList(u64).init(allocator);

        return OpenValves{
            .opened = opened,
            .allocator = allocator,
        };
    }

    fn add_valve(self: Self, open_valve_id: u64) !Self {
        var opened = try self.opened.clone();
        try opened.append(open_valve_id);

        std.mem.sort(u64, opened.items, {}, comptime std.sort.asc(u64));

        return OpenValves{
            .opened = opened,
            .allocator = self.allocator,
        };
    }

    fn to_string(self: Self) ![]const u8 {
        var slices = std.ArrayList([]const u8).init(self.allocator);

        for (self.opened.items) |item| {
            const s = try std.fmt.allocPrint(self.allocator, "{d}", .{item});
            try slices.append(s);
        }

        return try std.mem.join(self.allocator, " ", slices.items);
    }
};

const OpenActivity = struct {
    data: *const Data,
    valve_id: u64,
    released_pressure: u64,
    minutes_elapsed: u64,
    open_valves: OpenValves,
    remaining_total_pressure: u64,
    allocator: std.mem.Allocator,

    const Self = @This();

    fn create_move_activity(self: *const Self, valve_id: u64) MoveActivity {
        return MoveActivity{
            .valve_id = valve_id,
            .data = self.data,
            .previous_move = null,
            .minutes_elapsed = self.minutes_elapsed + 1,
            .open = &self,
            .allocator = self.allocator,
        };
    }

    fn visit_connections(self: *const Self, state: *State) !u64 {
        var max_pressure: u64 = 0;
        var conns = self.data.connections.items[self.valve_id];
        for (conns.items) |conn_valve_id| {
            var move_activity = self.create_move_activity(conn_valve_id);
            if (move_activity.can_release_max_pressure(state)) {
                // var pressure = try move_activity.compute_maximum_pressure_released(state);
                // if (pressure > max_pressure) {
                //     max_pressure = pressure;
                // }
            }
        }

        return max_pressure;
    }
};

const MoveActivity = struct {
    data: *const Data,
    valve_id: u64,
    minutes_elapsed: u64,
    previous_move: ?*const MoveActivity,
    open: ?*const MoveActivity,
    allocator: std.mem.Allocator,
    released_pressure: u64,
    kind: ActivityKind,
    open_valves: OpenValves,
    remaining_total_pressure: u64,

    const Self = @This();

    fn init(allocator: std.mem.Allocator, data: *const Data) Self {
        return MoveActivity{
            .kind = ActivityKind.Move,
            .released_pressure = 0,
            .open_valves = OpenValves.init(allocator),
            .remaining_total_pressure = data.total_pressure,
            .valve_id = data.start_valve,
            .data = data,
            .previous_move = null,
            .open = null,
            .minutes_elapsed = 0,
            .allocator = allocator,
        };
    }

    fn create_open_activity(self: *const Self, state: *State) !?MoveActivity {
        state.count += 1;
        if (self.open) |open_activity| {
            const open_valves = try open_activity.open_valves.add_valve(self.valve_id);

            const oa = MoveActivity{
                .kind = ActivityKind.Open,
                .valve_id = self.valve_id,
                .open = self.open,
                .data = self.data,
                .previous_move = null,
                .minutes_elapsed = self.minutes_elapsed + 1,
                .released_pressure = open_activity.released_pressure + (self.data.available_minutes - 1 - self.minutes_elapsed) * self.data.pressures.items[self.valve_id],
                .remaining_total_pressure = open_activity.remaining_total_pressure - self.data.pressures.items[self.valve_id],
                .open_valves = open_valves,
                .allocator = self.allocator,
            };

            const key = try oa.open_valves.to_string();
            const state_oa = state.max_pressures.get(key) orelse return null;

            if (oa.released_pressure <= state_oa.released_pressure and oa.minutes_elapsed >= state_oa.minutes_elapsed) {
                return null;
            }

            if (oa.released_pressure > state.max_pressure) {
                state.max_pressure = state_oa.released_pressure;
            }

            try state.max_pressures.put(key, &oa);
            return oa;
        } else {
            const open_valves = OpenValves.init(self.allocator);

            return MoveActivity{
                .kind = ActivityKind.Open,
                .previous_move = self,
                .valve_id = self.valve_id,
                .open = null,
                .data = self.data,
                .minutes_elapsed = self.minutes_elapsed + 1,
                .released_pressure = (self.data.available_minutes - 1 - self.minutes_elapsed) * self.data.pressures.items[self.valve_id],
                .remaining_total_pressure = self.data.total_pressure - self.data.pressures.items[self.valve_id],
                .open_valves = open_valves,
                .allocator = self.allocator,
            };
        }
    }

    fn can_release_max_pressure(self: *const Self, state: *State) bool {
        if (self.open) |open| {
            if (self.minutes_elapsed < 30) {
                const mins = 30 - self.minutes_elapsed - 1; // it will take atleast take 1 minute to open the next valve

                if (mins > 0 and state.max_pressure > open.released_pressure) {
                    const avg = (state.max_pressure - open.released_pressure) / mins;
                    if (avg > open.remaining_total_pressure) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    fn valve_can_be_opened(self: *const Self, valve_id: u64) bool {
        if (self.data.openable.items[valve_id]) {
            if (self.open) |open_activity| {
                for (open_activity.open_valves.opened.items) |id| {
                    if (id == valve_id) {
                        return false;
                    }
                }
                return true;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    fn in_loop(self: *const Self, valve_id: u64) bool {
        var activity: ?*const MoveActivity = self;
        while (activity) |a| {
            if (a.valve_id == valve_id) {
                return true;
            } else {
                if (a.previous_move) |prev| {
                    activity = prev;
                } else {
                    return false;
                }
            }
        }
        return false;
    }

    // fn visit_connections(self: Self, state: *State) !u64 {
    //     var max_pressure: u64 = 0;
    //     if (self.minutes_elapsed < self.data.available_minutes) {
    //         var conns = self.data.connections.items[self.valve_id];
    //         for (conns.items) |conn_valve_id| {
    //             if (!self.in_loop(conn_valve_id)) {
    //                 state.count += 1;
    //                 const move_activity = self.create_move_activity(conn_valve_id);
    //                 if (move_activity.can_release_max_pressure(state)) {
    //                     const pressure = try move_activity.compute_maximum_pressure_released(state);
    //                     if (pressure > max_pressure) {
    //                         max_pressure = pressure;
    //                     }
    //                 }
    //             }
    //         }
    //     }

    //     return max_pressure;
    // }

    fn create_move_activity(self: *const Self, valve_id: u64) MoveActivity {
        return MoveActivity{
            .valve_id = valve_id,
            .data = self.data,
            .previous_move = self,
            .minutes_elapsed = self.minutes_elapsed + 1,
            .open = self.open,
            .allocator = self.allocator,
            .released_pressure = self.released_pressure,
            .kind = ActivityKind.Move,
            .remaining_total_pressure = self.remaining_total_pressure,
            .open_valves = self.open_valves,
        };
    }

    fn compute_maximum_pressure_released(self: *const Self, state: *State) !u64 {
        if (self.minutes_elapsed == self.data.available_minutes - 1) {
            if (self.open) |open| {
                return open.released_pressure;
            } else {
                return 0;
            }
        } else {
            var max_pressure: u64 = 0;
            if (self.valve_can_be_opened(self.valve_id)) {
                if (try self.create_open_activity(state)) |open_activity| {
                    if (open_activity.minutes_elapsed < open_activity.data.available_minutes) {
                        var conns = open_activity.data.connections.items[self.valve_id];
                        for (conns.items) |conn_valve_id| {
                            if (!(&open_activity).in_loop(conn_valve_id)) {
                                state.count += 1;
                                const move_activity = open_activity.create_move_activity(conn_valve_id);
                                if (move_activity.can_release_max_pressure(state)) {
                                    const pressure = try move_activity.compute_maximum_pressure_released(state);
                                    if (pressure > max_pressure) {
                                        max_pressure = pressure;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (self.minutes_elapsed < self.data.available_minutes) {
                var conns = self.data.connections.items[self.valve_id];
                for (conns.items) |conn_valve_id| {
                    if (!self.in_loop(conn_valve_id)) {
                        state.count += 1;
                        const move_activity = self.create_move_activity(conn_valve_id);
                        if (move_activity.can_release_max_pressure(state)) {
                            const pressure = try move_activity.compute_maximum_pressure_released(state);
                            if (pressure > max_pressure) {
                                max_pressure = pressure;
                            }
                        }
                    }
                }
            }

            return max_pressure;
        }
    }
};

const State = struct {
    max_pressures: std.StringHashMap(*const MoveActivity),
    max_pressure: u64,
    count: u64,

    const Self = @This();

    fn init(allocator: std.mem.Allocator) Self {
        var max_pressures = std.StringHashMap(*const MoveActivity).init(allocator);
        return State{
            .max_pressures = max_pressures,
            .max_pressure = 0,
            .count = 0,
        };
    }
};

test "Parsing Data" {
    const data = try Data.init(sample, std.testing.allocator);
    defer data.deinit();
    try std.testing.expectEqual(data.pressures.items.len, 10);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const data = try Data.init(sample, allocator);
    defer data.deinit();

    var state = State.init(allocator);
    const move_activity = MoveActivity.init(allocator, &data);
    const max_pressure = try move_activity.compute_maximum_pressure_released(&state);
    std.debug.print("{d}\n", .{max_pressure});
}
