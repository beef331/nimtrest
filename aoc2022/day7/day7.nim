import std/[tables, strscans, times, monotimes, streams, strutils]

type
  DirectoryEntry = ref object
    size: int
    children: Table[string, DirectoryEntry]
    parent: DirectoryEntry

proc addSizeToParents(dirEntry: DirectoryEntry, size: int) =
  var parent = dirEntry.parent
  while parent != nil:
    parent.size += size
    parent = parent.parent

proc generateFs(s: string): DirectoryEntry =
  result = DirectoryEntry()
  var currentDir = result
  let fs = newFileStream(s)
  defer: fs.close()
  var buffer, nameBuffer = newStringOfCap(80)
  while not fs.atEnd():
    let line = fs.readLine()
    if line == "$ ls":
      while not fs.atEnd() and fs.peekChar() != '$':
        discard fs.readLine(buffer)
        var size: int
        if buffer.scanf("dir $+", nameBuffer):
          currentDir.children[nameBuffer] = DirectoryEntry(parent: currentDir)
        elif buffer.scanf("$i $+", size, nameBuffer):
          currentDir.size += size
          currentDir.addSizeToParents(size)
        else:
          echo "Failed to parse: ", buffer
    elif line.scanf("$$ cd $+", nameBuffer):
      case nameBuffer
      of "..":
        if currentDir.parent != nil:
          currentDir = currentDir.parent
        else:
          currentDir = result
      of "/":
        currentDir = result
      else:
        if nameBuffer in currentDir.children:
          currentDir = currentDir.children[nameBuffer]

proc solve1(root: DirectoryEntry): int =
  # Buh bye poor stack, I hardly knew ye
  for dir, entry in root.children.pairs:
    if entry.size <= 100000:
      result.inc entry.size
    result.inc solve1(entry)

proc solve2(root: DirectoryEntry, requiredSize = 0): int =
  let requiredSize =
    if requiredSize == 0:
      30000000 - (70000000 - root.size)
    else:
      requiredSize
  result = int.high
  for dir, entry in root.children.pairs:
    let size = entry.size
    if entry.size >= requiredSize:
      result = min(entry.size, result)
    result = min(solve2(entry, requiredSize), result)

import std/[times, monotimes]
var start = getMonoTime()
let myFS = generateFs("input.txt")
echo "Parse: ", getMonoTime() - start

echo solve1(myFs)
echo "Pt1: ", getMonoTime() - start

echo solve2(myFs)
echo "Pt2: ", getMonoTime() - start

