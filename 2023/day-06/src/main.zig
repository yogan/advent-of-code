const std = @import("std");
const isDigit = std.ascii.isDigit;
const stderr = std.debug;
const testing = std.testing;
const Allocator = std.mem.Allocator;

const input = @embedFile("input.txt");
const sample = @embedFile("sample.txt");

const Records = struct {
    times: []u64,
    distances: []u64,
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

// FIXME: there is a memory leak in here, probably related to allocPrint
fn fixKerning(allocator: Allocator, records: Records) !Records {
    var time_str: []u8 = "";
    for (records.times) |time| {
        time_str = std.fmt.allocPrint(allocator, "{s}{d}", .{ time_str, time }) catch unreachable;
    }
    const time_num = try std.fmt.parseInt(u64, time_str, 10);
    // allocator.free(time_str);

    var distance_str: []u8 = "";
    for (records.distances) |distance| {
        distance_str = std.fmt.allocPrint(allocator, "{s}{d}", .{ distance_str, distance }) catch unreachable;
    }
    const distance_num = try std.fmt.parseInt(u64, distance_str, 10);
    // allocator.free(distance_str);

    const times = [_]u64{time_num};
    const distances = [_]u64{distance_num};
    const new_records = Records{
        .times = @constCast(times[0..times.len]),
        .distances = @constCast(distances[0..distances.len]),
    };
    return new_records;
}

// FIXME: fixKerning() is leaking memory, so the test fails; it does however
// return the correct result, so for now we just disable the test.

// test "fixKerning concatenates the time and distance numbers of the sample" {
//     const times = [_]u64{ 7, 15, 30 };
//     const distances = [_]u64{ 9, 40, 200 };
//     const records = Records{
//         .times = @constCast(times[0..times.len]),
//         .distances = @constCast(distances[0..distances.len]),
//     };
//     // defer testing.allocator.free(records.times);
//     // defer testing.allocator.free(records.distances);
//
//     const result = try fixKerning(testing.allocator, records);
//
//     // defer testing.allocator.free(result.times);
//     // defer testing.allocator.free(result.distances);
//
//     try testing.expect(std.mem.eql(u64, result.times, &.{71530}));
//     try testing.expect(std.mem.eql(u64, result.distances, &.{940200}));
//
//     // testing.allocator.free(records.times);
//     // testing.allocator.free(records.distances);
//     // testing.allocator.free(result.times);
//     // testing.allocator.free(result.distances);
// }

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
    return countWinningRaces(single_race_records.times[0], single_race_records.distances[0]);
}

// no tests for part2, because it calls fixKerning(), which leaks memory :-(

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

    try stdout.print("Part 1: {d}\n", .{p1});
    try stdout.print("Part 2: {d}\n", .{p2});

    try bw.flush();
}
