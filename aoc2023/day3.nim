import std/[strutils, parseutils, os, tables]

proc isSymbol(c: char): bool = c notin Digits + {'.'}

proc adjacentCheck(start, nd, width, height: int, data: string): (bool, int) =
  template returnIf(cond: bool, charInd: int) =
    let chr = data[charInd]
    if cond and chr.isSymbol():
      if chr == '*':
        return (true, charInd)
      return (true, -1)

  result = (false, -1)
  var outChar: char
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
    gears: Table[int, seq[int]]

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
        if gears.hasKeyOrPut(gearInd, @[val]):
          gears[gearInd].add val

    i += valLen
  for val in gears.values:
    if val.len == 2:
      result[1] += val[0] * val[1]

echo solveIt(paramStr(1))
