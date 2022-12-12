import std/[times, monotimes, deques]
type
  Map = object
    nodes: seq[char]
    allAs: seq[int]
    width: int
    start: int
    target: int

proc loadMap(): Map =
  for line in lines("input.txt"):
    result.width = line.len
    for ch in line:
      case ch
      of 'S':
        result.nodes.add 'a'
        result.start = result.nodes.high
        result.allAs.add result.nodes.high
      of 'E':
        result.nodes.add 'z'
        result.target = result.nodes.high
      else:
        result.nodes.add ch
        if ch == 'a':
          result.allAs.add result.nodes.high

proc getShortest(map: Map, pt2: static bool = false): int =
  var
    queue = initDeque[(int, int)](30)
    visited = newSeq[bool](map.nodes.len)

  when pt2:
    for a in map.allAs:
      queue.addFirst (a, 0)
  else:
    queue.addFirst (map.start, 0)

  while queue.len > 0:
    let
      (ind, steps) = queue.popFirst
      node = map.nodes[ind]
      highestVal = succ(node)
      upIndex = ind - map.width
      downIndex = ind + map.width
      leftIndex = ind - 1
      rightIndex = ind + 1

    template checkLogic(val: int) =
      if map.nodes[val] <= highestVal:
        if not visited[val]:
          queue.addLast (val, steps + 1)
          visited[val] = true
        if val == map.target:
          return steps + 1

    if upIndex >= 0:
      checkLogic(upIndex)

    if downIndex <= map.nodes.high:
      checkLogic(downIndex)

    if ind mod map.width > 0:
      checkLogic(leftIndex)

    if ind mod map.width + 1 < map.width:
      checkLogic(rightIndex)



var start = getMonoTime()
let map = loadMap()
echo "Parse: ", getMonoTime() - start
start = getMonoTime()
echo map.getShortest(), " Part1: ", getMonoTime() - start
start = getMonoTime()
echo map.getShortest(true), " Part2: ", getMonoTime() - start

