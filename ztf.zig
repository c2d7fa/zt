const std = @import("std");

fn readTitle(buffer: []u8, file: std.fs.File) anyerror![]u8 {
  var len = try file.read(buffer);
  if (len >= buffer.len) len = buffer.len - 1;

  var inFrontmatter = false;
  var start: u64 = 0;
  var end: u64 = len;

  var i: u64 = 0;
  while (i < len) {
    if (i == 0 and std.mem.startsWith(u8, buffer, "---\n")) {
      inFrontmatter = true;
      i += 4;
      continue;
    }

    if (inFrontmatter and std.mem.startsWith(u8, buffer[i..], "\n---\n")) {
      inFrontmatter = false;
      i += 5;
      start = i;
      continue;
    }

    if (!inFrontmatter and buffer[i] == '\n') {
      if (i == start) {
        i += 1;
        start = i;
        continue;
      }
      end = i;
      break;
    }

    i += 1;
  }

  return buffer[start..end];
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

