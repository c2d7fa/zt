const std = @import("std");

fn readTitle(buffer: []u8, file: std.fs.File) std.os.ReadError![]u8 {
  _ = try file.read(buffer);

  for (buffer) |*char, i| {
    if (char.* == '\n') return buffer[0..i];
  }

  return buffer[0..1024];
}

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

  var buffer: [1024]u8 = [_]u8{0} ** 1024;

  var iterator = dir.iterate();
  while (try iterator.next()) |entry| {
    if (entry.kind != std.fs.Dir.Entry.Kind.File) continue;
    var file = try dir.openFile(entry.name, .{});
    defer file.close();
    const title = try readTitle(&buffer, file);
    try stdout.print("{s}\n", .{title});
  }
}

