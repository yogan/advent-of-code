const std = @import("std");
const stderr = std.debug;
const testing = std.testing;
const Allocator = std.mem.Allocator;

// The easiest way to get the input data - just embed it in the binary.
// Found here: https://www.huy.rocks/everyday/12-11-2022-zig-using-zig-for-advent-of-code#read-input-file
// The whole article is great, it has a lot of useful hints for advent of code
// in Zig, without spoiling anything.
const content = @embedFile("input.txt");

pub fn parseLine(line: []const u8) ![3]u32 {
    var num_buffer = [3]u32{ 0, 0, 0 };
    var it = std.mem.split(u8, line, "x");
    var i: usize = 0;
    while (it.next()) |num| {
        const num_u32 = try std.fmt.parseInt(u32, num, 10);
        num_buffer[i] = num_u32;
        i += 1;
    }
    // stderr.print("num_buffer: {d}\n", .{num_buffer});
    return num_buffer;
}

test "parseLine splits line by x and returns array of u32 (single digit numbers)" {
    const result = try parseLine("1x2x3");
    try testing.expectEqual(result, .{ 1, 2, 3 });
}

test "parseLine splits line by x and returns array of u32 (multi digit numbers)" {
    const result = try parseLine("11x420x1337");
    try testing.expectEqual(result, .{ 11, 420, 1337 });
}

fn calc_volume(l: u32, w: u32, h: u32) u32 {
    return l * w * h;
}

fn calc_surface_area(l: u32, w: u32, h: u32) u32 {
    return 2 * l * w + 2 * w * h + 2 * h * l;
}

fn part1() !u32 {
    var total_volume: u32 = 0;

    var line_iter = std.mem.split(u8, content, "\n");
    while (line_iter.next()) |line| {
        const numbers = try parseLine(line);
        const volume = calc_volume(numbers[0], numbers[1], numbers[2]);
        total_volume += volume;
    }

    return total_volume;
}

fn part2() !u32 {
    var total_surface_area: u32 = 0;

    var line_iter = std.mem.split(u8, content, "\n");
    while (line_iter.next()) |line| {
        const numbers = try parseLine(line);
        const surface_area = calc_surface_area(numbers[0], numbers[1], numbers[2]);
        total_surface_area += surface_area;
    }

    return total_surface_area;
}

test "part1 returns correct result" {
    const result = try part1();
    try testing.expectEqual(result, 14545);
}

test "part2 returns correct result" {
    const result = try part2();
    try testing.expectEqual(result, 4978);
}

pub fn main() !void {
    const p1 = try part1();
    const p2 = try part2();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("{d}\n", .{p1});
    try stdout.print("{d}\n", .{p2});

    try bw.flush();
}
