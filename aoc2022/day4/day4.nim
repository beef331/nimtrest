import std/[times, monotimes, strscans]
let start = getMonoTime()

var
  overlaps = 0
  allOverlaps = 0

for line in lines("input.txt"):
  var left, right: Slice[int]
  if line.scanf("$i-$i,$i-$i", left.a, left.b, right.a, right.b):
    if (left.a in right and left.b in right) or (right.a in left and right.b in left):
      inc overlaps
    if left.a in right or left.b in right or right.a in left or right.b in left:
      inc allOverlaps

echo "Answer: Part1 - ", overlaps , " Part2 - ", allOverlaps, " | ", getMonoTime() - start
