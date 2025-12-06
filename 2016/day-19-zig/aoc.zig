const std = @import("std");
const print = std.debug.print;
const testing = std.testing;
const Allocator = std.mem.Allocator;

fn part1(elves: u24, alloc: Allocator) !u24 {
    const slice = try alloc.alloc(u24, elves);
    defer alloc.free(slice);
    @memset(slice, 1);

    var i: u24 = 0;
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
    id: u24,
    left: *Elf,
    right: *Elf,
};

fn part2(numberOfElves: u24, alloc: Allocator) !u24 {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const arenaAlloc = arena.allocator();

    var elf: *Elf = try arenaAlloc.create(Elf);
    elf.* = Elf{ .id = 1, .left = undefined, .right = undefined };

    const firstElf = elf;
    var target: *Elf = undefined;
    var i: u24 = 2;

    while (i <= numberOfElves) {
        const nextElf = try arenaAlloc.create(Elf);
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
    var cur = firstElf;

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
            return cur.id;
        }

        cur = cur.right;
    }
}

test "part1 works for five elves" {
    try testing.expectEqual(part1(5, testing.allocator), 3);
}

test "part2 works for five elves" {
    try testing.expectEqual(part2(5, testing.allocator), 2);
}

pub fn main() !u8 {
    const alloc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

    if (args.len != 2) {
        print("Usage: {s} <input-file>\n", .{std.fs.path.basename(args[0])});
        return 1;
    }

    const elves = try readInputFile(args[1], alloc);

    print("{d}\n", .{try part1(elves, alloc)});
    print("{d}\n", .{try part2(elves, alloc)});
    return 0;
}

pub fn readInputFile(filename: []const u8, alloc: Allocator) !u24 {
    var file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer file.close();

    const content = try file.readToEndAlloc(alloc, 1024 * 1024); // 1MB max
    defer alloc.free(content);

    const line = std.mem.trim(u8, content, "\r\n ");
    return try std.fmt.parseInt(u24, line, 10);
}
