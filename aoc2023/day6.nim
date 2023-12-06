import std/[os, enumerate, parseutils, strutils]


proc solveIt(name: string): (int, int) =
  var 
    times: seq[int]
    distances: seq[int]
    time2: int
    distance2: int
  for lineNum, line in enumerate lines name:
    var i = line.skipUntil(Digits)
    let num = line[i..^1].replace(" ")
    if lineNum == 0:
      time2 = parseInt(num)
    else:
      distance2 = parseInt(num)

    while i < line.len:
      var val: int
      i += line.toOpenArray(i, line.high).parseInt(val)
      i += line.toOpenArray(i, line.high).skipUntil(Digits)
      if lineNum == 0:
        times.add val
      else:
        distances.add val

  result = (1, 0)
  for i in 0..times.high:
    let time = times[i]
    var winCount = 0
    for val in 1..time:
      if (time - val) * val > distances[i]:
        inc winCount
    result[0] *= winCount

  var winCount = 0
  for val in 1..time2:
    if (time2 - val) * val > distance2:
      inc result[1] 


echo solveIt(paramStr(1))
