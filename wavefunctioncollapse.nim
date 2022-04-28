import std/[random, terminal]

const
  WorldSize = 10
  WorldArea = WorldSize * WorldSize

type
  Id = enum
    none
    water = ansiForegroundColorCode(fgBlue) & "W" & ansiResetCode
    sand = ansiForegroundColorCode(fgYellow) & "S" & ansiResetCode
    grass = ansiForegroundColorCode(fgWhite) & ansiStyleCode(styleBright) & "G" & ansiResetCode
    trees = ansiForegroundColorCode(fgGreen) & ansiStyleCode(styleDim) & "T" & ansiResetCode
    forest = ansiForegroundColorCode(fgGreen) & "F" & ansiResetCode
  MyData = array[WorldArea, Id]
  
iterator adjacentIndices*(myData: MyData, ind: int): int =
  if ind mod WorldSize > 0:
    yield ind - 1
  if ind mod WorldSize < WorldSize - 1:
    yield ind + 1
  if ind >= WorldSize:
    yield ind - WorldSize
  if ind + WorldSize < myData.len:
    yield ind + WorldSize
    
proc getValid(id: Id): set[Id] =
  case id
  of none: {}
  of water: {sand}
  of sand: {water, grass}
  of grass: {sand, trees}
  of trees: {grass, forest}
  of forest: {trees}
  
proc hasNone(data: MyData): bool =
  for x in data:
    if x == none:
      return true

proc nicePrint(myData: MyData) = 
  var myStr = ""
  for i, id in myData:
    if i mod WorldSize == 0:
      stdout.writeLine(myStr)
      myStr.setLen(0)
    myStr.add $id
    myStr.add " "
  stdout.flushFile()

proc generate(myData: var MyData, start: int) =
  var 
    visted: set[0..WorldArea] = {range[0..WorldArea] start}
    toVisit: seq[int]
  for x in myData.adjacentIndices(start):
    toVisit.add x
  while toVisit.len > 0:
    let ind = toVisit.pop
    if myData[ind] == none:
      var valid: set[Id]
      for adjacent in myData.adjacentIndices(ind):
        if adjacent notin visted:
          toVisit.add adjacent
        if myData[adjacent] != none:
          if valid == {}:
            valid = myData[adjacent].getValid
          else:
            valid = valid * myData[adjacent].getValid
      if valid == {}:
        myData[ind] = rand(succ(none)..Id.high)
      else:
        var val = rand(succ(none)..Id.high)
        while val notin valid:
          val = rand(succ(none)..Id.high)
        myData[ind] = val

randomize()

var myData: MyData
let startInd = rand(0..<myData.len)
myData[startInd] = rand(succ(none)..Id.high)
myData.generate(startInd)

myData.nicePrint()

