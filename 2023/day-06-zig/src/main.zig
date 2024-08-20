const std = @import("std");
const isDigit = std.ascii.isDigit;
const stderr = std.debug;
const testing = std.testing;
const Allocator = std.mem.Allocator;

const input = @embedFile("input.txt");
const sample = @embedFile("sample.txt");

const Records = struct {
    times: []const u64,
    distances: []const u64,
};

const SingleRecord = struct {
    time: u64,
    distance: u64,
};

fn parseLine(allocator: Allocator, line: []const u8) ![]u64 {
    var line_iter = std.mem.split(u8, line, ":");
    _ = line_iter.next();
    const numbers_part = line_iter.next() orelse "";

    var numbers = std.ArrayList(u64).init(allocator);
    defer numbers.deinit();

    var numbers_iter = std.mem.tokenize(u8, numbers_part, " ");
    while (numbers_iter.next()) |token| {
        const num = try std.fmt.parseInt(u64, token, 10);
        try numbers.append(num);
    }

    return numbers.toOwnedSlice();
}

fn parseLines(allocator: Allocator, content: []const u8) !Records {
    var line_iter = std.mem.split(u8, content, "\n");

    const time_line = line_iter.next() orelse "";
    const distance_line = line_iter.next() orelse "";

    return Records{
        .times = try parseLine(allocator, time_line),
        .distances = try parseLine(allocator, distance_line),
    };
}

test "parseLines parses sample" {
    const allocator = testing.allocator;
    const result = try parseLines(allocator, sample);
    defer allocator.free(result.times);
    defer allocator.free(result.distances);

    try testing.expect(std.mem.eql(u64, result.times, &.{ 7, 15, 30 }));
    try testing.expect(std.mem.eql(u64, result.distances, &.{ 9, 40, 200 }));
}

fn simulateRace(race_time: u64, acceleration_time: u64) u64 {
    const remaining_time = race_time -% acceleration_time;
    return remaining_time * acceleration_time;
}

test "simulateRace works for the first race of the sample" {
    const race_time = 7;
    try testing.expectEqual(simulateRace(race_time, 0), 0);
    try testing.expectEqual(simulateRace(race_time, 1), 6);
    try testing.expectEqual(simulateRace(race_time, 2), 10);
    try testing.expectEqual(simulateRace(race_time, 3), 12);
    try testing.expectEqual(simulateRace(race_time, 4), 12);
    try testing.expectEqual(simulateRace(race_time, 5), 10);
    try testing.expectEqual(simulateRace(race_time, 6), 6);
    try testing.expectEqual(simulateRace(race_time, 7), 0);
}

fn countWinningRaces(record_time: u64, record_distance: u64) u64 {
    var wins: u64 = 0;
    for (0..record_time + 1) |acceleration_time| {
        const distance = simulateRace(record_time, @intCast(acceleration_time));
        if (distance > record_distance) {
            wins += 1;
        }
    }
    return wins;
}

test "countWinningRaces works for the first race of the sample" {
    const record_time = 7;
    const distance = 9;
    try testing.expectEqual(countWinningRaces(record_time, distance), 4);
}

test "countWinningRaces works for the second race of the sample" {
    const record_time = 15;
    const distance = 40;
    try testing.expectEqual(countWinningRaces(record_time, distance), 8);
}

test "countWinningRaces works for the third race of the sample" {
    const record_time = 30;
    const distance = 200;
    try testing.expectEqual(countWinningRaces(record_time, distance), 9);
}

fn fixKerning(allocator: Allocator, records: Records) !SingleRecord {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const aa = arena.allocator();

    var time: []u8 = "";
    for (records.times) |t| {
        time = try std.fmt.allocPrint(aa, "{s}{d}", .{ time, t });
    }

    var distance: []u8 = "";
    for (records.distances) |d| {
        distance = try std.fmt.allocPrint(aa, "{s}{d}", .{ distance, d });
    }

    return SingleRecord{
        .time = try std.fmt.parseInt(u64, time, 10),
        .distance = try std.fmt.parseInt(u64, distance, 10),
    };
}

test "fixKerning concatenates the time and distance numbers of the sample" {
    const records = Records{
        .times = &[_]u64{ 7, 15, 30 },
        .distances = &[_]u64{ 9, 40, 200 },
    };

    const result = try fixKerning(testing.allocator, records);

    try testing.expectEqual(result.time, 71530);
    try testing.expectEqual(result.distance, 940200);
}

fn part1(records: Records) u64 {
    var result: u64 = 1;
    for (0..records.times.len) |i| {
        const wins = countWinningRaces(records.times[i], records.distances[i]);
        result *= wins;
    }
    return result;
}

test "part1 returns correct result for the sample" {
    const allocator = testing.allocator;
    const records = try parseLines(allocator, sample);
    defer allocator.free(records.times);
    defer allocator.free(records.distances);

    const result = part1(records);

    try testing.expectEqual(result, 288);
}

test "part1 returns correct result for the real input" {
    const allocator = testing.allocator;
    const records = try parseLines(allocator, input);
    defer allocator.free(records.times);
    defer allocator.free(records.distances);

    const result = part1(records);

    try testing.expectEqual(result, 2065338);
}

fn part2(records: Records, allocator: Allocator) !u64 {
    const single_race_records = fixKerning(allocator, records) catch unreachable;
    return countWinningRaces(single_race_records.time, single_race_records.distance);
}

test "part2 returns correct result for the real input" {
    const allocator = testing.allocator;
    const records = try parseLines(allocator, input);
    defer allocator.free(records.times);
    defer allocator.free(records.distances);

    const result = try part2(records, allocator);

    try testing.expectEqual(result, 34934171);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    // const records = try parseLines(allocator, sample);
    const records = try parseLines(allocator, input);
    defer allocator.free(records.times);
    defer allocator.free(records.distances);

    const p1 = part1(records);
    const p2 = part2(records, allocator) catch 0;

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("{d}\n", .{p1});
    try stdout.print("{d}\n", .{p2});

    try bw.flush();
}
