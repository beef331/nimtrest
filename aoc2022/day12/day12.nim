import std/[times, monotimes, intsets, deques]
type
  Map = object
    nodes: seq[char]
    allAs: seq[int]
    width: int
    start: int
    target: int

proc loadMap(): Map =
  result.nodes = newSeqOfCap[char](1000)
  for line in lines("input.txt"):
    result.width = line.len
    for ch in line:
      if ch == 'S':
        result.nodes.add 'a'
        result.start = result.nodes.high
        result.allAs.add result.nodes.high
      elif ch == 'E':
        result.nodes.add 'z'
        result.target = result.nodes.high
      else:
        result.nodes.add ch
        if ch == 'a':
          result.allAs.add result.nodes.high


proc getShortest(map: Map, pt2: static bool = false): int =
  var
    queue = initDeque[(int, int)]()
    visted = initIntSet()

  when pt2:
    for a in map.allAs:
      queue.addFirst (a, 0)
  else:
    queue.addFirst (map.start, 0)

  while queue.len > 0:
    let
      (ind, steps) = queue.popFirst
      node = map.nodes[ind]
      upIndex = ind - map.width
      downIndex = ind + map.width
      leftIndex = ind - 1
      rightIndex = ind + 1

    proc maybeAdd(val: int) =
      if map.nodes[val] <= succ(node) and val notin visted:
        queue.addLast (val, steps + 1)
        visted.incl val

    template breakIfEnd(val: int) =
      if val == map.target and map.nodes[val] <= succ(node):
        result = steps + 1
        break

    if upIndex >= 0:
      breakIfEnd(upIndex)
      maybeAdd(upIndex)

    if downIndex <= map.nodes.high:
      breakIfEnd(downIndex)
      maybeAdd(downIndex)

    if ind mod map.width > 0:
      breakIfEnd(leftIndex)
      maybeAdd(leftIndex)

    if ind mod map.width + 1 < map.width:
      breakIfEnd(rightIndex)
      maybeAdd(rightIndex)



var start = getMonoTime()
let map = loadMap()
echo "Parse: ", getMonoTime() - start
start = getMonoTime()
echo map.getShortest(), " Part1: ", getMonoTime() - start
start = getMonoTime()
echo map.getShortest(true), " Part2: ", getMonoTime() - start

