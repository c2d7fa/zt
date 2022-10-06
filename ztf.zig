const std = @import("std");

fn firstLine(buffer: []const u8) []const u8 {
  var i: u64 = 0;
  while (i < buffer.len) {
    if (buffer[i] == '\n') return buffer[0..i];
    i += 1;
  }
  return buffer;
}

fn readTitle(buffer: []const u8, name: []const u8) ![]const u8 {
  var bestFound: u8 = 0;

  var inFrontmatter = false;
  var start: u64 = 0;

  var i: u64 = 0;
  while (i < buffer.len) {
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
      return firstLine(buffer[i..]);
    }

    if (!inFrontmatter and std.mem.startsWith(u8, buffer[i..], "\n# ")) {
      return firstLine(buffer[i+1..])[2..];
    }

    if (i == 0 and std.mem.startsWith(u8, buffer[i..], "# ")) {
      return firstLine(buffer[i..])[2..];
    }

    if (std.mem.startsWith(u8, buffer[i..], "\n* ")) {
      return firstLine(buffer[i+1..])[2..];
    }

    if (i == 0 and std.mem.startsWith(u8, buffer, "* ")) {
      return firstLine(buffer[i..])[2..];
    }

    if (std.mem.startsWith(u8, buffer[i..], "#+TITLE: ") or std.mem.startsWith(u8, buffer[i..], "#+title: ")) {
      return firstLine(buffer[i..])[9..];
    }

    if (firstLine(buffer[i..]).len == 1) {
      i += 1;
      continue;
    } else {
      break;
    }
  }

  if (bestFound == 0) {
    var nameEnd: u64 = name.len - 1;

    while (nameEnd > 0) {
      if (nameEnd <= 17) {
        return firstLine(buffer[i..]);
      }
      if (name[nameEnd] == '.') {
        nameEnd = nameEnd;
        break;
      }
      nameEnd -= 1;
    }

    return name[16..nameEnd];
  }

  return firstLine(buffer[i..]);
}

pub fn doesMatch(buffer: []const u8, searchTerm: []const u8) bool {
  for (buffer) |char, i| {
    if (char == searchTerm[0] and buffer[i+1] == searchTerm[1] and std.mem.startsWith(u8, buffer[i..], searchTerm)) return true;
  }
  return false;
}

const InvalidIdError = error { InvalidId };

fn parseId(name: []const u8) anyerror![]const u8 {
  if (name.len < 17) return InvalidIdError.InvalidId;
  return name[0..15];
}

fn cmpId(context: void, a: [:0]u8, b: [:0]u8) bool {
  _ = context;

  var i: u64 = 0;
  while (i < 15) {
    if (a[i] > b[i]) {
      return true;
    } else if (a[i] < b[i]) {
      return false;
    }
    i += 1;
  }
  return false;
}

pub fn main() !void {
  var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
  defer arena.deinit();
  var allocator = arena.allocator();

  const stdout = std.io.getStdOut().writer();

  const args = try std.process.argsAlloc(allocator);
  const dirPath = args[1];

  if (std.mem.eql(u8, dirPath, "--version")) {
    try stdout.print("{s}\n", .{"1"});
    return;
  }

  var fileBuffer: []u8 = try allocator.alloc(u8, 1048576);

  const path = try std.fs.path.resolve(allocator, &.{dirPath});
  var dir = std.fs.openDirAbsolute(path, .{.iterate = true}) catch {
    // I couldn't figure out how to actually check the kind of a path, so instead
    // we do this to guess if we're dealing with a file.
    var file = std.fs.openFileAbsolute(path, .{}) catch {
      // Actual error.
      try stdout.print("{s}\n", .{dirPath});
      return;
    };
    defer file.close();

    var len = try file.read(fileBuffer);
    if (len >= fileBuffer.len) len = fileBuffer.len - 1;

    const title = try readTitle(fileBuffer[0..len], std.fs.path.basename(path));
    try stdout.print("{s}\n", .{title});

    return;
  };
  defer dir.close();

  var outputLines = try allocator.alloc([:0]u8, 4096);
  var outputI: u64 = 0;

  var iterator = dir.iterate();
  while (try iterator.next()) |entry| {
    if (entry.kind != std.fs.Dir.Entry.Kind.File) continue;
    var file = dir.openFile(entry.name, .{}) catch { continue; };
    defer file.close();

    var len = try file.read(fileBuffer);
    if (len >= fileBuffer.len) len = fileBuffer.len - 1;

    if (args.len > 2 and !doesMatch(fileBuffer[0..len], args[2])) {
      continue;
    }

    const title = try readTitle(fileBuffer[0..len], entry.name);
    const id = parseId(entry.name) catch { continue; };

    outputLines[outputI] = try allocator.allocSentinel(u8, id.len + 1 + title.len, 0);
    std.mem.copy(u8, outputLines[outputI], id);
    outputLines[outputI][id.len] = ' ';
    std.mem.copy(u8, outputLines[outputI][id.len + 1..], title);

    outputI += 1;
  }

  std.sort.sort([:0]u8, outputLines[0..outputI], {}, cmpId);

  for (outputLines[0..outputI]) |line| {
    _ = try stdout.write(line[0..]);
    _ = try stdout.write("\n");
  }

  //for (outputLines.items) |line| {
  //  _ = try stdout.write(line);
  //}
}

