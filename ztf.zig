const std = @import("std");

fn readTitle(buffer: []u8, file: std.fs.File, name: []const u8) ![]const u8 {
  var len = try file.read(buffer);
  if (len >= buffer.len) len = buffer.len - 1;

  var bestFound: u8 = 0;

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

    if (inFrontmatter and std.mem.startsWith(u8, buffer[i..], "title: ")) {
      inFrontmatter = false;
      i += 7;
      start = i;
      bestFound = 2;
      continue;
    }

    if (!inFrontmatter and std.mem.startsWith(u8, buffer[i..], "\n# ")) {
      i += 3;
      start = i;
      bestFound = 1;
      continue;
    }

    if (i == 0 and std.mem.startsWith(u8, buffer[i..], "# ")) {
      i += 2;
      start = i;
      bestFound = 1;
      continue;
    }

    if (std.mem.startsWith(u8, buffer[i..], "\n* ")) {
      i += 3;
      start = i;
      bestFound = 1;
      continue;
    }

    if (i == 0 and std.mem.startsWith(u8, buffer, "* ")) {
      i += 2;
      start = i;
      bestFound = 1;
      continue;
    }

    if (std.mem.startsWith(u8, buffer[i..], "#+TITLE: ") or std.mem.startsWith(u8, buffer[i..], "#+title: ")) {
      i += 9;
      start = i;
      bestFound = 2;
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

  if (bestFound == 0) {
    var nameEnd: u64 = name.len - 1;

    while (nameEnd > 0) {
      if (nameEnd <= 17) {
        return buffer[start..end];
      }
      if (name[nameEnd] == '.') {
        nameEnd = nameEnd;
        break;
      }
      nameEnd -= 1;
    }

    return name[16..nameEnd];
  }

  return buffer[start..end];
}

const InvalidIdError = error { InvalidId };

pub fn parseId(name: []const u8) anyerror![]const u8 {
  if (name.len < 17) return InvalidIdError.InvalidId;
  return name[0..15];
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

  var buffer: [2048]u8 = [_]u8{0} ** 2048;

  var iterator = dir.iterate();
  while (try iterator.next()) |entry| {
    if (entry.kind != std.fs.Dir.Entry.Kind.File) continue;
    var file = try dir.openFile(entry.name, .{});
    defer file.close();
    const title = try readTitle(&buffer, file, entry.name);
    const id = parseId(entry.name) catch { continue; };
    try stdout.print("{s} {s}\n", .{id, title});
  }
}

