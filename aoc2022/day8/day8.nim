import std/[enumerate, times, monotimes]
type
  SearchDir = enum Up, Left, Right, Down # Probably need to do diagnals for pt2.
  Tree = char


proc parse(s: string): (seq[Tree], int) =
  var width = 0
  result[0] = newSeqOfCap[Tree](99 * 99)
  for i, line in enumerate lines(s):
    if i == 0:
      result[1] = line.len
      width = result[1]
    result[0].add line


proc check(data: seq[Tree], ind, width: int): (int, bool) =
  let
    height = data.len div width
    tree = data[ind]
    offset = [Up: -width, Left: -1, Right: 1, Down: width]

  var viewDist: array[SearchDir, int]
  for dir in SearchDir:
    var i = ind + offset[dir]

    while i in 0..data.high:
      inc viewDist[dir]
      if tree <= data[i]:
        break
      if (i mod width in [0, width - 1] or i div width in [0, height - 1]) and data[i] < tree:
        result[1] = true
        break
      i += offset[dir]

  result[0] = viewDist[SearchDir.low]
  for dir in succ(SearchDir.low)..SearchDir.high:
    result[0] *= viewDist[dir]

proc solve(trees: seq[Tree], width: int): (int, int) =
 let height = trees.len div width
 for x in 1 ..< width - 1:
   for y in 1 ..< height - 1:
     let (rng, visible) = trees.check(x + y * width, width)
     if visible:
       inc result[0]
     result[1] = max(rng, result[1])

 result[0].inc width * 2
 result[0].inc 2 * (height - 2)

var start = getMonoTime()
let (trees, width) = parse("input.txt")
echo "Parsing: ", getMonoTime() - start
start = getMonoTime()
echo trees.solve(width), " ", getMonoTime() - start






