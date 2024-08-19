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

    var i: u32 = 0;
    while (true) {
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
        i = (i + 1) % elves;
    }
}

const Elf = struct {
    id: usize,
    left: *Elf,
    right: *Elf,
};

fn part2(numberOfElves: u32) !usize {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var elf: *Elf = try alloc.create(Elf);
    elf.* = Elf{ .id = 1, .left = undefined, .right = undefined };

    const firstElf = elf;
    var target: *Elf = undefined;
    var i: usize = 2;

    while (i <= numberOfElves) {
        const nextElf = try allocator.create(Elf);
        nextElf.* = Elf{ .id = i, .left = elf, .right = undefined };
        elf.right = nextElf;
        elf = nextElf;
        if (i == numberOfElves / 2 + 1) {
            target = elf;
        }
        i += 1;
    }

    // close the circle
    elf.right = firstElf;
    firstElf.left = elf;

    var elvesLeft = numberOfElves;
    var e = firstElf;

    while (true) {
        // remove target
        target.left.right = target.right;
        target.right.left = target.left;

        // find next target
        if (elvesLeft % 2 == 0) {
            target = target.right;
        } else {
            target = target.right.right;
        }

        elvesLeft -= 1;
        if (elvesLeft == 1) {
            return e.id;
        }

        e = e.right;
    }
}

test "part1 works for five elves" {
    try testing.expectEqual(part1(5), 3);
}

test "part2 works for five elves" {
    try testing.expectEqual(part2(5), 2);
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
    try stdout.print("{d}\n", .{try part2(elves)});

    try bw.flush();
    return 0;
}
