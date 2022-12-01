import std/[strutils, monotimes, times, enumerate]

proc addCalories(mostCalories: var array[3, int], accumulator: var int) =
  for i, x in enumerate mostCalories.mitems:
    if x < accumulator:
      for j in i + 1 ..< mostCalories.high:
        mostCalories[j] = mostCalories[j - 1]
      x = accumulator
      break
  reset accumulator

let start = getMonoTime()
var
  mostCalories = [int.low, int.low, int.low]
  accumulator = 0
for line in lines("input.txt"):
  try:
    accumulator += line.parseInt()
  except ValueError:
    mostCalories.addCalories(accumulator)

mostCalories.addCalories(accumulator)
   
echo "Answers: ", "Part1 - ", mostCalories[0], " | Part2 - ", mostCalories[0] + mostCalories[1] + mostCalories[2], " | ", getMonoTime() - start
