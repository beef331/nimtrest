import std/[os, parseutils, strutils, enumerate, tables]

proc solveIt(file: string): (int, int) =
  var copies = newSeq[int]() 
  for lineNum, line in enumerate lines file:
    var 
      winning: set[range[0..100]]
      i = line.find(":") + 1
      inWinning = true
      lineValue = 0
      winCount = 0

    while i < line.len:
      i += line.toOpenArray(i, line.high).skipWhile(Whitespace)
      case line[i]
      of Digits:
        var val: int
        i += line.toOpenArray(i, line.high).parseInt(val)
        if inWinning:
          winning.incl range[0..100](val)
        elif val in winning:
          lineValue = max(lineValue * 2, 1)
          inc winCount

      of '|':
        inWinning = false
        i += 1
      else:
        doAssert false

    if lineNum + winCount >= copies.len:
      copies.setLen lineNum + winCount + 1
    
    inc copies[lineNum]

    for ind in lineNum + 1 .. lineNum + winCount:
      copies[ind] += copies[lineNum]

  
    result[0] += lineValue

  for x in copies:
    result[1] += x

echo solveIt(paramStr(1))
