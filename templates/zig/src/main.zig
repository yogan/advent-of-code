const std = @import("std");
const stderr = std.debug;
const testing = std.testing;

const alloc = std.heap.page_allocator;

const Box = struct { l: u32, w: u32, h: u32 };

pub fn readInputFile() ![]Box {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf: [1024]u8 = undefined;
    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var parsedLines = std.ArrayList(Box).init(alloc);

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try parsedLines.append(try parseLine(line));
    }

    return parsedLines.items;
}

pub fn parseLine(line: []const u8) !Box {
    var buf = [3]u32{ 0, 0, 0 };
    var it = std.mem.split(u8, line, "x");
    var i: usize = 0;
    while (it.next()) |num| {
        const num_u32 = try std.fmt.parseInt(u32, num, 10);
        buf[i] = num_u32;
        i += 1;
    }
    return Box{ .l = buf[0], .w = buf[1], .h = buf[2] };
}

test "parseLine splits line by x and returns a box (single digit numbers)" {
    const result = try parseLine("1x2x3");
    try testing.expectEqual(result, Box{ .l = 1, .w = 2, .h = 3 });
}

test "parseLine splits line by x and returns a box (multi digit numbers)" {
    const result = try parseLine("11x420x1337");
    try testing.expectEqual(result, Box{ .l = 11, .w = 420, .h = 1337 });
}

fn calc_volume(box: Box) u32 {
    return box.l * box.w * box.h;
}

fn calc_surface_area(box: Box) u32 {
    return 2 * (box.l * box.w + box.w * box.h + box.h * box.l);
}

fn part1(boxes: []const Box) !u32 {
    var sum: u32 = 0;
    for (boxes) |box| {
        sum += calc_volume(box);
    }
    return sum;
}

fn part2(boxes: []const Box) !u32 {
    var sum: u32 = 0;
    for (boxes) |box| {
        sum += calc_surface_area(box);
    }
    return sum;
}

test "part1 returns sum of volumes of boxes" {
    const boxes = [_]Box{ Box{ .l = 2, .w = 3, .h = 4 }, Box{ .l = 1, .w = 1, .h = 10 } };
    const expected = (2 * 3 * 4) + (1 * 1 * 10);

    try testing.expectEqual(try part1(&boxes), expected);
}

test "part2 returns sum of surface areas of boxes" {
    const boxes = [_]Box{ Box{ .l = 2, .w = 3, .h = 4 }, Box{ .l = 1, .w = 1, .h = 10 } };
    const expected = 2 * (2 * 3 + 3 * 4 + 4 * 2) + 2 * (1 * 1 + 1 * 10 + 10 * 1);

    try testing.expectEqual(try part2(&boxes), expected);
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const boxes = try readInputFile();
    try stdout.print("{d}\n", .{try part1(boxes)});
    try stdout.print("{d}\n", .{try part2(boxes)});

    try bw.flush();
}
