import std/[random, terminal, colors]

const
  WorldSize = 20
  WorldArea = WorldSize * WorldSize

type
  Id = enum
    none
    water = ansiBackgroundColorCode(colAqua) & " " & ansiResetCode
    sand = ansiBackgroundColorCode(colSandyBrown) & " " & ansiResetCode
    grass = ansiBackgroundColorCode(colLimeGreen) & " " & ansiResetCode
    trees = ansiBackgroundColorCode(colGreen) & " " & ansiResetCode
    forest = ansiBackgroundColorCode(colDarkGreen) & " " & ansiResetCode
    mountain = ansiBackgroundColorCode(colDarkGray) & " " & ansiResetCode
    mountainTop = ansiBackgroundColorCode(colWhite) & " " & ansiResetCode


  MyData = array[WorldArea, Id]
  
iterator adjacentIndices*(myData: MyData, ind: int): int =
  if ind mod WorldSize > 0:
    yield ind - 1
  if ind mod WorldSize + 1 < WorldSize:
    yield ind + 1
  if ind >= WorldSize:
    yield ind - WorldSize
  if ind + WorldSize <= myData.high:
    yield ind + WorldSize
    
proc getValid(id: Id): set[Id] =
  case id
  of none: {}
  of water: {sand, grass, water}
  of sand: {water, grass, sand}
  of grass: {sand, trees, grass}
  of trees: {grass, forest, trees}
  of forest: {trees, forest}
  of mountain: {forest, trees, mountaintop, mountain}
  of mountainTop: {mountain, mountaintop}
  
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
    myStr.add $id
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
        echo "No valid so putting, ", myData[ind]
      else:
        var val = rand(succ(none)..Id.high)
        while val notin valid:
          val = rand(succ(none)..Id.high)
        myData[ind] = val
        if myData[ind] == mountaintop:
          echo valid

randomize()

var myData: MyData
let startInd = rand(0..<myData.len)
myData[startInd] = rand(succ(none)..Id.high)
myData.generate(startInd)

myData.nicePrint()

