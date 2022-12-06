import std/[times, monotimes]


proc toSet(oa: openarray[char]): set[0..26] =
  for x in oa:
    result.incl ord(x) mod 32

let input = readFile("input.txt")
var part1, part2 = 0
let start = getMonoTime()
for i in 0 .. input.high - 3:

  if part1 == 0 and input.toOpenArray(i, i + 3).toSet.len == 4:
    part1 = i + 4

  if part1 != 0 and input.toOpenArray(i, i + 13).toSet.len == 14:
    part2 = i + 14
    break

echo "Answer: Part1 - ", part1, " Part2 - ", part2, " | ", getMonoTime() - start
