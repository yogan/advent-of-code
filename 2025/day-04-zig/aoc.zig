const std = @import("std");
const print = std.debug.print;
const testing = std.testing;
const Allocator = std.mem.Allocator;

fn forklift(grid: [][]u8) struct { p1: u32, p2: u32 } {
    var p1: u32 = 0;
    var p2: u32 = 0;

    while (true) {
        var rolls: u32 = 0;
        for (grid, 0..) |row, r| {
            for (row, 0..) |cell, c| {
                if (cell == '@' and liftable(grid, @intCast(r), @intCast(c))) {
                    rolls += 1;
                    if (p1 != 0) {
                        grid[r][c] = '.';
                    }
                }
            }
        }
        if (p1 == 0) {
            p1 = rolls;
        } else {
            p2 += rolls;
        }

        if (rolls == 0) {
            return .{ .p1 = p1, .p2 = p2 };
        }
    }
}

test "forklift works for the sample" {
    const alloc = testing.allocator;
    const grid = try readInputFile("sample.txt", alloc);
    defer freeGrid(grid, alloc);

    const result = forklift(grid);
    try testing.expectEqual(13, result.p1);
    try testing.expectEqual(43, result.p2);
}

fn liftable(grid: [][]u8, r: isize, c: isize) bool {
    const rows = grid.len;
    const cols = grid[0].len;

    const directions = [_][2]isize{
        .{ -1, -1 }, .{ -1, 0 }, .{ -1, 1 },
        .{ 0, -1 },  .{ 0, 1 },  .{ 1, -1 },
        .{ 1, 0 },   .{ 1, 1 },
    };

    var neighbors: u32 = 0;

    for (directions) |dir| {
        const nr = r + dir[0];
        const nc = c + dir[1];

        if (nr >= 0 and nc >= 0 and nr < rows and nc < cols) {
            if (grid[@intCast(nr)][@intCast(nc)] == '@') {
                neighbors += 1;
            }
        }
    }

    return neighbors < 4;
}

pub fn main() !u8 {
    const alloc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len != 2) {
        print("Usage: {s} <input-file>\n", .{std.fs.path.basename(args[0])});
        return 1;
    }

    const grid = try readInputFile(args[1], alloc);
    defer freeGrid(grid, alloc);

    const result = forklift(grid);
    print("{d}\n{d}\n", .{ result.p1, result.p2 });
    return 0;
}

pub fn readInputFile(filename: []const u8, alloc: Allocator) ![][]u8 {
    const content = try std.fs.cwd().readFileAlloc(alloc, filename, 1024 * 1024);
    defer alloc.free(content);

    var lines = std.ArrayList([]u8){};
    defer lines.deinit(alloc);

    var iter = std.mem.splitSequence(u8, content, "\n");
    while (iter.next()) |line| {
        if (line.len == 0) continue;
        const copy = try alloc.dupe(u8, line);
        try lines.append(alloc, copy);
    }

    return try lines.toOwnedSlice(alloc);
}

fn freeGrid(grid: [][]u8, alloc: Allocator) void {
    for (grid) |row| {
        alloc.free(row);
    }
    alloc.free(grid);
}
