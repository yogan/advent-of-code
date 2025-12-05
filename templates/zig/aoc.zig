const std = @import("std");
const print = std.debug.print;
const testing = std.testing;
const Allocator = std.mem.Allocator;

const Box = struct { l: u32, w: u32, h: u32 };

fn part1(boxes: []const Box) !u32 {
    var sum: u32 = 0;
    for (boxes) |box| {
        sum += calc_volume(box);
    }
    return sum;
}

test "part1 returns sum of volumes of boxes" {
    const boxes = [_]Box{ Box{ .l = 2, .w = 3, .h = 4 }, Box{ .l = 1, .w = 1, .h = 10 } };
    const expected = (2 * 3 * 4) + (1 * 1 * 10);

    try testing.expectEqual(try part1(&boxes), expected);
}

fn calc_volume(box: Box) u32 {
    return box.l * box.w * box.h;
}

pub fn main() !u8 {
    const alloc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len != 2) {
        print("Usage: {s} <input-file>\n", .{std.fs.path.basename(args[0])});
        return 1;
    }

    const boxes = try readInputFile(args[1], alloc);
    defer alloc.free(boxes);

    print("{d}\n", .{try part1(boxes)});
    return 0;
}

pub fn readInputFile(filename: []const u8, alloc: Allocator) ![]const Box {
    var file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer file.close();

    const content = try file.readToEndAlloc(alloc, 1024 * 1024); // 1MB max
    defer alloc.free(content);

    var parsedLines = std.ArrayList(Box){};
    defer parsedLines.deinit(alloc);

    var lines = std.mem.splitSequence(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue; // skip empty lines
        try parsedLines.append(alloc, try parseLine(line));
    }

    return try parsedLines.toOwnedSlice(alloc);
}

pub fn parseLine(line: []const u8) !Box {
    var it = std.mem.splitScalar(u8, line, 'x');
    const l = try std.fmt.parseInt(u32, it.next().?, 10);
    const w = try std.fmt.parseInt(u32, it.next().?, 10);
    const h = try std.fmt.parseInt(u32, it.next().?, 10);
    return Box{ .l = l, .w = w, .h = h };
}

test "parseLine splits line by x and returns a box" {
    const result = try parseLine("11x420x1337");
    try testing.expectEqual(result, Box{ .l = 11, .w = 420, .h = 1337 });
}
