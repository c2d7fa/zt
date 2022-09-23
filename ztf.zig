const std = @import("std");

pub fn main() !void {
  var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
  defer arena.deinit();
  var allocator = arena.allocator();

  const stdout = std.io.getStdOut().writer();

  const args = try std.process.argsAlloc(allocator);
  const dirPath = args[1];

  const path = try std.fs.path.resolve(allocator, &.{dirPath});
  var dir = try std.fs.openDirAbsolute(path, .{.iterate = true});
  defer dir.close();

  var iterator = dir.iterate();
  while (try iterator.next()) |entry| {
    try stdout.print("{s}\n", .{entry.name});
  }
}

