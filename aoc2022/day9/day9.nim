import std/[times, monotimes, sets, parseutils, enumerate]

type
  Rope[size: static int] = object
    vals: array[size, (int, int)]


proc head(rope: Rope): (int, int) = rope.vals[^1]
proc head(rope: var Rope): var (int, int) = rope.vals[^1]
proc `[]`(rope: var Rope, ind: int): var (int, int) = rope.vals[ind]
proc `[]`(rope: Rope, ind: int): (int, int) = rope.vals[ind]
proc `[]=`(rope: var Rope, ind: int, knot: (int, int)) = rope.vals[ind] = knot

proc moveRope(rope: var Rope, visited: var OrderedSet[(int, int)], offset: int, isHorz: bool) =
  let oldRope = rope
  rope.head[0] += offset * ord(isHorz)
  rope.head[1] += offset * ord(not isHorz)

  visited.incl rope.vals[0]

  for i in countDown(rope.vals.high - 1, 0):
    let
      knot1 = rope.vals[i]
      knot2 = rope.vals[i + 1]

    if 2 in [abs(knot1[0] - knot2[0]), abs(knot1[1] - knot2[1])]:
      rope.vals[i] = oldRope[i + 1]

  visited.incl rope.vals[0]


var
  smallRope = Rope[2]()
  largeRope = Rope[10]()
  smallVisited: OrderedSet[(int, int)]
  largeVisited: OrderedSet[(int, int)]

for line in lines("test.txt"):
  var amount = 0
  assert line.toOpenArray(2, line.high).parseInt(amount) > 0
  let
    chr = line[0]
    isHorz = chr in {'L', 'R'}
    offset =
      if chr in ['U', 'R']:
        1
      else:
        -1
  for _ in 0..<amount:
    smallRope.moveRope(smallVisited, offset, isHorz)
    largeRope.moveRope(largeVisited, offset, isHorz)

echo smallVisited.len
echo largeVisited.len
