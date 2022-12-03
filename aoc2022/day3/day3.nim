import std/[times, monotimes, enumerate]

proc score(s: set['A'..'z']): int =
  for x in s:
    if x in 'A'..'Z':
      result += ord(x) - ord('A') + 27
    else:
      result += ord(x) - ord('a') + 1

let start = getMonoTime()
var
  sum1, sum2 = 0
  lastThree: array[3, set['A'..'z']]

for i, line in enumerate lines("input.txt"):
  var left, right: set['A'..'z'] # bleh over allocate
  for i, ch in line:
    if i < line.len div 2:
      left.incl ch
    else:
      right.incl ch
  lastThree[i mod 3] = left + right

  sum1 += score(left * right)
  if i mod 3 == 2 and i > 0:
    sum2 += score(lastThree[0] * lastThree[1] * lastThree[2])

echo "Answer: Part1 - ", sum1 , " Part2 - ", sum2, " | ", getMonoTime() - start




