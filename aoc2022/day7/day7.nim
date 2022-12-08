import std/[tables, strscans, times, monotimes, streams, strutils]

type
  DirectoryEntry {.acyclic.} = ref object
    size: int
    children: Table[string, DirectoryEntry]
    parent {.cursor.}: DirectoryEntry

proc addSizeToParents(dirEntry: DirectoryEntry, size: int) =
  var parent = dirEntry.parent
  while parent != nil:
    parent.size += size
    parent = parent.parent

proc generateFs(s: string): seq[DirectoryEntry] =
  result = newSeqOfCap[DirectoryEntry](100)
  result.add DirectoryEntry()
  var currentDir = result[0]
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
          let entry = DirectoryEntry(parent: currentDir)
          currentDir.children[nameBuffer] = entry
          result.add entry

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
          currentDir = result[0]
      of "/":
        currentDir = result[0]
      else:
        if nameBuffer in currentDir.children:
          currentDir = currentDir.children[nameBuffer]

proc solve(entries: seq[DirectoryEntry]): (int, int) =
  # Buh bye poor stack, I hardly knew ye
  let requiredSize = 30000000 - (70000000 - entries[0].size)
  result[1] = int.high
  for entry in entries:
    let size = entry.size
    if size >= requiredSize:
      result[1] = min(size, result[1])
    if size <= 100_000:
      result[0] += size

import std/[times, monotimes]
var start = getMonoTime()
let myFS = generateFs("input.txt")
echo "Parse: ", getMonoTime() - start

echo solve(myFs)
echo "Pt 1 and 2: ", getMonoTime() - start

