import std/[strutils, parseutils, os, tables]

proc isSymbol(c: char): bool = c notin Digits + {'.'}

proc adjacentCheck(start, nd, width, height: int, data: string): (bool, int) =
  template returnIf(cond: bool, charInd: int) =
    let 
      ind = charInd
      chr = data[ind]
    if cond and chr.isSymbol():
      if chr == '*':
        return (true, ind)
      return (true, -1)

  result = (false, -1)

  for i in start .. nd:
    assert data[i] in Digits

    let 
      leftRoom = i mod width > 0
      rightRoom = i mod width < width - 1
    
    returnIf leftRoom, i - 1 
    returnIf rightRoom, i + 1 

    if i div width > 0:
      returnIf true, i - 1
      returnIf leftRoom, i - width - 1
      returnIf rightRoom, i - width + 1 

    if i div width < height - 1:
      returnIf true, i + width 
      returnIf leftRoom, i + width - 1 
      returnIf rightRoom, i + width + 1 

proc solveIt(name: string): (int, int) =
  var
    data = newStringOfCap(140 * 140)
    width: int
    height: int
    gears = initTable[int, (int, int)](128)

  for line in lines(name):
    if width == 0:
      width = line.len
    data.add line
    inc height

  var i = 0 
  doAssert data.len == width * height

  while (i += data.skipUntil(Digits, i); i < data.len):
    var val: int
    let valLen = data.toOpenArray(i, i div width * width + width - 1).parseInt(val)
    assert data[i..valLen + i - 1].allCharsInSet(Digits)
    if (let (adj, gearInd) = adjacentCheck(i, valLen + i - 1, width, height, data); adj):
      result[0] += val
      if gearInd >= 0:
        if gears.hasKeyOrPut(gearInd, (val, -1)):
          gears[gearInd][1] = val

    i += valLen
  for val in gears.values:
    if val[1] != -1:
      result[1] += val[0] * val[1]
import std/[times, monotimes]
let start = getMonoTime()
discard solveIt(paramStr(1))
echo getMonoTime() - start
