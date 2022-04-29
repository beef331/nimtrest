import std/[random, terminal, colors]

const
  WorldSize = 30
  WorldArea = WorldSize * WorldSize

proc colouredBlock(col: Color): string = ansiBackgroundColorCode(col) & " " & ansiResetCode

type
  Id = enum
    none = ""
    water = colAqua.colouredBlock()
    sand = colSandyBrown.colouredBlock()
    grass = colLimeGreen.colouredBlock()
    trees = colGreen.colouredBlock()
    forest = colDarkGreen.colouredBlock()
    mountain = colDarkGray.colouredBlock()
    mountainTop = colWhite.colouredBlock()

  MyData = object
    placementData: array[WorldArea, set[Id]]
    tileData: array[WorldArea, Id]

const fullIdSet = {succ(none) .. Id.high}

iterator adjacentTiles(myData: var MyData, ind: int): var set[Id]=
  let
    x = ind mod WorldSize
    y = ind div WorldSize
  if x > 0:
    yield myData.placementData[ind - 1]

  if y > 0:
    yield myData.placementData[ind - WorldSize]

  if x + 1 < WorldSize:
    yield myData.placementData[ind + 1]

  if y + 1 < WorldSize:
    yield myData.placementData[ind + WorldSize]


iterator adjacentTiles(myData: MyData, ind: int): set[Id]=
  let
    x = ind mod WorldSize
    y = ind div WorldSize
  if x > 0:
    yield myData.placementData[ind - 1]

  if y > 0:
    yield myData.placementData[ind - WorldSize]

  if x + 1 < WorldSize:
    yield myData.placementData[ind + 1]

  if y + 1 < WorldSize:
    yield myData.placementData[ind + WorldSize]


proc getAllowedNeighbours(id: Id): set[Id] =
  case id
  of none: {}
  of water: {sand, water, grass}
  of sand: {water, grass, sand}
  of grass: {sand, grass, trees, forest, mountain}
  of trees: {grass, forest, trees, water}
  of forest: {trees, forest, mountain, grass}
  of mountain: {mountaintop, mountain, forest}
  of mountainTop: {mountain, mountainTop}


proc nicePrint(myData: MyData) =
  var myStr = ""
  for i, id in myData.tileData:
    if i mod WorldSize == 0:
      stdout.writeLine(myStr)
      myStr.setLen(0)
    myStr.add $id
    myStr.add $id
  if myStr.len > 0:
    stdout.writeLine(myStr)
  stdout.flushFile()

proc randomPick(s: set[Id]): Id =
  while result notin s:
    result = rand(Id.low.succ .. Id.high)

proc isValidPlacement(myData: MyData, index: int, id: Id): bool =
  result = true
  for adjacentTile in myData.adjacentTiles(index):
    if id.getAllowedNeighbours() * adjacentTile == {}:
      return false

proc setAllowedIds(myData: var MyData, index: int) =
  var
    entry: Id
    canEntryHere = false
  while not canEntryHere:
    entry = myData.placementData[index].randomPick()
    canEntryHere = myData.isValidPlacement(index, entry)

  for adjacentTile in myData.adjacentTiles(index):
    adjacentTile = adjacentTile * entry.getAllowedNeighbours()
  myData.tileData[index] = entry

proc generateWorld(): MyData =
  for x in result.placementData.mitems:
    x = fullIdSet
  result.setAllowedIds(0)
  for ind in 1..<result.placementData.len:
    result.setAllowedIds(ind)

randomize()
let data = generateWorld()
data.nicePrint()



