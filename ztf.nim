import std/os
import std/nre
import std/sugar

var dir = commandLineParams()[0]

proc matchId(s: string): Option[string] =
  s.match(re"\d{8}T\d{6}").map(match)

let allIds = collect:
  for file in walkDir(dir):
    let id = file.path.lastPathPart.matchId
    if id.isSome:
      id.get

echo allIds

