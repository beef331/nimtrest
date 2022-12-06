import std/[times, monotimes, setutils]



let input = readFile("input.txt")
var part1, part2 = 0
let start = getMonoTime()
for i in 0 .. input.high - 3:
  let matchingChars = input.toOpenArray(i, i + 3).toSet

  if matchingChars.len == 4 and part1 == 0:
    part1 = i + 4

  if part1 > 0 and i + 13 < input.len:
   let matchingChars = input.toOpenArray(i, i + 13).toSet
   if matchingChars.len == 14 and part2 == 0:
     part2 = i + 14

  
echo "Answer: Part1 - ", part1, " Part2 - ", part2, " | ", getMonoTime() - start
