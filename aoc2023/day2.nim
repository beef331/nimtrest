import std/[parseutils, strutils, enumerate]

type
  Colour = enum
    Red, Green, Blue

  Bag = array[Colour, int]


iterator turns(oa: openArray[char]): Bag =
  var 
    i = 0
    count = 0
    result: Bag
  while i < oa.len:
    i += oa.skipWhile(WhiteSpace)
    i += oa.toOpenArray(i, oa.high).parseInt(count)
    i += oa.toOpenArray(i, oa.high).skipUntil(Letters)
    case oa[i]
    of 'r':
      result[Red] += count
    of 'g':
      result[Green] += count
    of 'b':
      result[Blue] += count
    else:
      doAssert false
    reset count
    i += oa.toOpenArray(i, oa.high).skipUntil({',', ';'} + WhiteSpace)
    if i >= oa.len or oa[i] == ';':
      yield result
      reset result
    inc i

proc solveIt(name: string, bag: Bag): (int, int) =
  for id, line in enumerate lines(name):
    let start = line.find(":") + 1
    var 
      valid = true
      maxVals: Bag = [0, 0, 0]
    for turn in line.toOpenArray(start, line.high).turns:
      for col in Colour:
        valid = valid and turn[col] <= bag[col]
        maxVals[col] = max(maxVals[col], turn[col])

    result[1] += maxVals[Red] * maxVals[Green] * maxVals[Blue]
    if valid:
      result[0] += (id + 1)


echo solveIt("day02.txt", [12, 13, 14])



