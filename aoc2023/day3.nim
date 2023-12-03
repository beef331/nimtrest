import std/[strutils, parseutils, os]

proc isSymbol(c: char): bool = c notin Digits + {'.'}

proc isAdjacent(start, nd, width, height: int, data: string): bool =
  template returnIf(b: bool) =
    if b:
      return true

  for i in start .. nd:
    assert data[i] in Digits

    let 
      leftRoom = i mod width > 0
      rightRoom = i mod width < width - 1
    
    returnIf leftRoom and data[i - 1].isSymbol # Left
    returnIf rightRoom and data[i + 1].isSymbol #Right

    if i div width > 0:
      returnIf data[i - width].isSymbol # Top
      returnIf leftRoom and data[i - width - 1].isSymbol # Top Left
      returnIf rightRoom and data[i - width + 1].isSymbol # Top Right

    if i div width < height - 1:
      returnIf data[i + width].isSymbol # Below
      returnIf leftRoom and data[i + width - 1].isSymbol # Below Left
      returnIf rightRoom and data[i + width + 1].isSymbol # Below Right

proc solveIt(name: string): int =
  var
    data = newStringOfCap(140 * 140)
    width: int
    height: int

  for line in lines(name):
    if width == 0:
      width = line.len
    data.add line
    inc height

  var i = data.skipUntil(Digits)

  while i < data.len:
    var val: int
    let valLen = data.parseInt(val, i)
    assert data[i..valLen + i - 1].allCharsInSet(Digits)
    if isAdjacent(i, valLen + i - 1, width, height, data):
      result += val

    i += valLen + data.skipUntil(Digits, i)

echo solveIt(paramStr(1))
