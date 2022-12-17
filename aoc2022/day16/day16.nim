import std/[tables, strscans, parseutils, algorithm, deques, intsets, sequtils, strutils, math]

type
  Node = ref object
    name: string
    flowRate: int
    isOn: bool
    neighbours: seq[string]
    index: int # For pathfinding only

  TunnelMap = object
    nodeTable: Table[string, Node]
    nodes: seq[Node]

  Path = distinct seq[Node]

proc `$`(n: Node): string = $n[]

proc `$`(n: Path): string =
  for n in seq[Node](n):
    result.add n.name
    result.add " => "
  result.setLen result.len - 4

proc compare(a, b: Node): int =
  if a.flowRate < b.flowRate:
    -1
  elif a.flowRate == b.flowRate:
    0
  else:
    1

proc parse(s: string): TunnelMap =
  var
    name = newStringOfCap(2)
    buffer = newStringOfCap(30)

  for line in lines(s):
    var flowRate = 0
    if line.scanf("Valve $+ has flow rate=$i; tunnels lead to valves $+", name, flowRate, buffer) or
      line.scanf("Valve $+ has flow rate=$i; tunnel leads to valve $+", name, flowRate, buffer):
      let myNode = Node(name: name, flowRate: flowRate)
      result.nodeTable[name] = myNode
      result.nodes.add myNode
      var i = 0
      while i < buffer.len:
        i += buffer.parseUntil(name, ',', i) + 2 # Skip comma and space
        myNode.neighbours.add name
  result.nodes.sort(compare, Descending)
  for i, node in result.nodes:
    node.index = i


proc pathFindTo(map: TunnelMap, current, target: string): seq[Node] =
  for neighbour in map.nodeTable[current].neighbours:
    if neighbour == target:
      return @[map.nodeTable[target]]

  var
    queue = map.nodeTable[current].neighbours.mapit(@[map.nodeTable[it]]).toDeque
    visited: IntSet

  for nodes in queue:
    visited.incl nodes[0].index

  while queue.len > 0:
    let path = queue.popFirst
    for nodeName in path[^1].neighbours:
      let targetNode = map.nodeTable[nodeName]
      if nodeName == target:
        result = path
        result.add targetNode
        return

      if targetNode.index notin visited:
        var newPath = path
        newPath.add targetNode
        queue.addLast newPath
        visited.incl targetNode.index

proc solve1(map: TunnelMap): int =
  var
    time = 30
    currentNode = "AA"
    pathGeneration = 0f
    path: seq[Node]
  while time > 0:
    var opened = 0
    path.setLen(0)
    pathGeneration = 0
    for node in map.nodes:
      if not node.isOn and node.flowRate > 0:
        let
          generatedPath = pathFindTo(map, currentNode, node.name)
          flowGen = float32(node.flowRate) / pow(float32 generatedPath.len + 1, 2f32)

        if time - path.len + 1 >= 0 and pathGeneration < flowGen:
          path = generatedPath
          pathGeneration = flowGen
      else:
        inc opened



    if opened < map.nodes.len:
      for _ in 0..path.len: # Yes this is just multiplication
        for node in map.nodes:
          if node.isOn:
            result += node.flowRate
        dec time

      echo "From ", currentNode, " opened: ", path[^1].name, " through: ", Path(path)

      currentNode = path[^1].name
      path[^1].isOn = true
      if time == 0:
        echo "Worked until the end"
    else:
      echo "Done doing anything at: ", time
      for node in map.nodes:
        if node.isOn:
          result += node.flowRate * time
      time = 0


let map = parse("test.txt")
echo solve1(map)
