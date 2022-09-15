import std/os
import std/nre
import std/sugar

var dir = commandLineParams()[0]

var searchTerm =
  if commandLineParams().len >= 2:
    commandLineParams()[1]
  else:
    ""

proc matchId(s: string): Option[string] =
  s.match(re"\d{8}T\d{6}").map(match)

let allIds = collect:
  for file in walkDir(dir):
    let id = file.path.lastPathPart.matchId
    if id.isSome:
      if file.path.readFile.find(re(searchTerm)).isSome:
        id.get

for id in allIds:
  echo id

