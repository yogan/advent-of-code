const std = @import("std");
const stderr = std.debug;
const testing = std.testing;

const alloc = std.heap.page_allocator;

fn readInputFile(filename: []const u8) !u32 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var buf: [10]u8 = undefined;
    const len = try file.read(&buf);

    const line = std.mem.trimRight(u8, buf[0..len], "\r\n");
    return try std.fmt.parseInt(u32, line, 10);
}

fn part1(elves: u32) !u32 {
    const slice = try alloc.alloc(u32, elves);
    defer alloc.free(slice);
    @memset(slice, 1);

    while (true) {
        var i: u32 = 0;
        while (i < elves) {
            if (slice[i] != 0) {
                // find next elf that still has presents
                var j = (i + 1) % elves;
                while (slice[j] == 0) {
                    j = (j + 1) % elves;
                }

                // steal presents
                // stderr.print("{d} steals {d} from {d}\n", .{ i + 1, slice[j], j + 1 });
                slice[i] += slice[j];
                slice[j] = 0;

                // winner takes it all
                if (slice[i] == elves) {
                    return i + 1;
                }
            }
            i += 1;
        }
    }
}

test "part1 works for five elves" {
    try testing.expectEqual(part1(5), 3);
}

pub fn main() !u8 {
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len != 2) {
        stderr.print("Usage: {s} <input-file>\n", .{std.fs.path.basename(args[0])});
        return 1;
    }

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const elves = try readInputFile(args[1]);
    try stdout.print("{d}\n", .{try part1(elves)});

    try bw.flush();
    return 0;
}
