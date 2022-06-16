import std/[random, terminal, colors]

const
  WorldSize = 10
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
  of none: fullIdSet
  of water: {sand, water, grass}
  of sand: {water, grass, sand}
  of grass: {sand, grass, trees, forest, mountain}
  of trees: {grass, forest, trees, water}
  of forest: {trees, forest, mountain, grass}
  of mountain: {mountaintop, mountain, forest}
  of mountainTop: {mountain, mountainTop}

proc isDone(myData: MyData): bool =
  result = true
  for tile in myData.placementData:
    if tile.len > 0:
      return false

proc nicePrint(myData: MyData, failed = -1) =
  var myStr = ""
  for i, id in myData.tileData:
    if i mod WorldSize == 0:
      stdout.writeLine(myStr)
      myStr.setLen(0)
    if i != failed:
      myStr.add $id
      myStr.add $id
    else:
      myStr.add colouredBlock(colRed)
      myStr.add colouredBlock(colRed)
  if myStr.len > 0:
    stdout.writeLine(myStr)
  stdout.flushFile()

proc isValidPlacement(myData: MyData, index: int, id: Id): bool =
  var allowed: set[Id]
  for adjacentTile in myData.adjacentTiles(index):
    for adj in adjacentTile:
      allowed.incl adj.getAllowedNeighbours()
  id in allowed

proc getAllowedPlacables(myData: MyData, index: int): set[Id] =
  for adjacentTile in myData.adjacentTiles(index):
    for id in adjacentTile:
      result = result + id.getAllowedNeighbours()
  result = result * myData.placementData[index]

proc setAllowedIds(myData: var MyData, index: int) =
  var
    entry: Id
    canEntryHere = false
    allowed: set[Id]
  for adjacentTile in myData.adjacentTiles(index):
    for id in adjacentTile:
      allowed = allowed + id.getAllowedNeighbours()
  allowed = allowed * myData.placementData[index]
  if allowed == {}:
    myData.nicePrint(index)
    echo myData.placementData[index]
    quit()
  while not canEntryHere:
    entry = allowed.sample()
    canEntryHere = myData.isValidPlacement(index, entry)

  for adjacentTile in myData.adjacentTiles(index):
    adjacentTile = adjacentTile * entry.getAllowedNeighbours()
  myData.tileData[index] = entry

proc generateWorld(): MyData =
  for x in result.placementData.mitems:
    x = fullIdSet
  result.setAllowedIds(0)
  while not result.isDone:
    result.setAllowedIds(rand(0..<WorldArea))

randomize()
let data = generateWorld()
data.nicePrint()



