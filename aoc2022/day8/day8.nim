import std/[enumerate, times, monotimes]
type
  SearchDir = enum Up, Left, Right, Down # Probably need to do diagnals for pt2.
  Tree = object
    height: char


proc parse(s: string): (seq[Tree], int) =
  var width = 0
  for i, line in enumerate lines(s):
    if i == 0:
      result[0] = newSeqOfCap[Tree](1000)
      result[1] = line.len
      width = result[1]

    for chI, ch in line:
      assert ch in '0'..'9'
      result[0].add Tree(height: ch)


proc visibleCheck*(data: seq[Tree], ind, width: int): bool =
  let
    height = data.len div width
    tree = data[ind]
    offset = [Up: -width, Left: -1, Right: 1, Down: width]

  for dir in SearchDir:
    var i = ind + offset[dir]

    while i in 0..data.high:
      if tree.height <= data[i].height:
        break
      if (i mod width in [0, width - 1] or i div width in [0, height - 1]) and data[i].height < tree.height:
        return true
      i += offset[dir]


proc rangeCheck*(data: seq[Tree], ind, width: int): int =
  let
    height = data.len div width
    tree = data[ind]
    offset = [Up: -width, Left: -1, Right: 1, Down: width]

  var viewDist: array[SearchDir, int]

  for dir in SearchDir:
    var i = ind + offset[dir]
    while i in 0..data.high:
      inc viewDist[dir]
      if (i mod width in [0, width - 1] or i div width in [0, height - 1]) and data[i].height < tree.height:
        break
      if tree.height <= data[i].height:
        break
      i += offset[dir]

  result = viewDist[SearchDir.low]
  for dir in succ(SearchDir.low)..SearchDir.high:
    result *= viewDist[dir]

proc solve(trees: seq[Tree], width: int): (int, int) =
 let height = trees.len div width
 for x in 1 ..< width - 1:
   for y in 1 ..< height - 1:
     if trees.visibleCheck(y * width + x, width):
       inc result[0]
     result[1] = max(trees.rangeCheck(y * width + x, width), result[1])

 result[0].inc width * 2
 result[0].inc 2 * (height - 2)

let (trees, width) = parse("input.txt")
let start = getMonoTime()

echo trees.solve(width), " ", getMonoTime() - start






