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
  var inFrontmatter = false;

  const isMarkdown = std.mem.endsWith(u8, name, ".md");
  const isOrg = std.mem.endsWith(u8, name, ".org");

  var mainStart: u64 = 0;

  var i: u64 = 0;
  while (i < buffer.len) {
    const line = firstLine(buffer[i..]);

    if (isMarkdown and std.mem.startsWith(u8, line, "---")) {
      inFrontmatter = !inFrontmatter;
      if (!inFrontmatter) {
        mainStart = i + line.len;
        while (mainStart < buffer.len and buffer[mainStart] == '\n') { mainStart += 1; }
      }
    }

    if (inFrontmatter) {
      if (std.mem.startsWith(u8, line, "title: ")) {
        return line[7..];
      }
    } else {
      if (isMarkdown and std.mem.startsWith(u8, line, "# ")) {
        return line[2..];
      } else if (isOrg and std.mem.startsWith(u8, line, "* ")) {
        return line[2..];
      } else if (std.mem.startsWith(u8, line, "#+TITLE: ") or std.mem.startsWith(u8, line, "#+title: ")) {
        return line[9..];
      }
    }

   i += line.len + 1;
  }

  var nameEnd: u64 = name.len - 1;
  while (nameEnd > 0) {
    if (nameEnd <= 17) {
      return firstLine(buffer[mainStart..]); // Can't use filename
    }
    if (name[nameEnd] == '.') {
      nameEnd = nameEnd;
      break;
    }
    nameEnd -= 1;
  }
  return name[16..nameEnd];
}

fn doesMatch(buffer: []const u8, searchTerm: []const u8) bool {
  for (buffer) |char, i| {
    if (char == searchTerm[0] and buffer[i+1] == searchTerm[1] and std.mem.startsWith(u8, buffer[i..], searchTerm)) return true;
  }
  return false;
}

const InvalidIdError = error { InvalidId };

fn parseId(name: []const u8) InvalidIdError![]const u8 {
  if (name.len < 17) return InvalidIdError.InvalidId;
  if (name[0] != '2' or name[8] != 'T') return InvalidIdError.InvalidId;
  return name[0..15];
}

fn cmpId(context: void, a: []const u8, b: []const u8) bool {
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

  var filenames = try allocator.alloc([]u8, 4096);
  var filenamesI: u64 = 0;

  var iterator = dir.iterate();
  while (try iterator.next()) |entry| {
    if (filenamesI >= filenames.len) break;
    if (entry.kind != std.fs.Dir.Entry.Kind.File) continue;
    filenames[filenamesI] = try allocator.alloc(u8, entry.name.len);
    std.mem.copy(u8, filenames[filenamesI], entry.name);
    filenamesI += 1;
    continue;
  }

  std.sort.sort([]const u8, filenames[0..filenamesI], {}, cmpId);

  for (filenames[0..filenamesI]) |filename| {
    const id = parseId(filename) catch { continue; };

    const file = dir.openFile(filename, .{}) catch { continue; };
    defer file.close();
    const len = file.read(fileBuffer) catch { continue; };

    if (args.len > 2 and !doesMatch(fileBuffer[0..len], args[2])) {
      continue;
    }

    const title = readTitle(fileBuffer[0..len], filename) catch { continue; };

    _ = try stdout.write(id);
    _ = try stdout.write(" ");
    _ = try stdout.write(title);
    _ = try stdout.write("\n");
  }
}

