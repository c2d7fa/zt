import std/os
import std/nre
import std/sugar
import std/strutils

var dir = commandLineParams()[0]

var searchTerm =
  if commandLineParams().len >= 2:
    commandLineParams()[1]
  else:
    ""

proc matchId(s: string): Option[string] =
  s.match(re"\d{8}T\d{6}").map(match)

proc fileTitle(path: string): string =
  if path.endsWith(".md"):
    let content = path.readFile

    let h1 = content.find(re"# (.+)")
    if h1.isSome:
      return h1.get.captures[0]

    let title = content.find(re"title: (.+)(?s:.)+?---")
    if title.isSome:
      return title.get.captures[0]

    let afterFrontmatter = content.find(re"---\n(?s:\s)+(.+)")
    if afterFrontmatter.isSome:
      return afterFrontmatter.get.captures[0]

  return path.readLines(1)[0]

let allIds = collect:
  for file in walkDir(dir):
    let id = file.path.lastPathPart.matchId
    if id.isSome:
      if file.path.readFile.find(re(searchTerm)).isSome:
        id.get & " " & file.path.fileTitle

for id in allIds:
  echo id

