import std/[strutils, os]

const
  nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

proc toInt(c: char): int = c.ord - '0'.ord

proc findDigits(line: string, searchWords = false): int =
  var 
    leftInd = line.find(Digits)
    rightInd = line.rfind(Digits)
    tens, ones: int

  if leftInd >= 0:
    tens = line[leftInd].toInt * 10 
  if rightInd >= 0:
    ones = line[rightInd].toInt

  if searchWords:
    for i, num in nums:
      var foundInd = line.find(num, last = leftInd)
      if foundInd > -1 and (foundInd < leftInd or leftInd == -1):
        tens = (i + 1) * 10
        leftInd = foundInd

      foundInd = line.rFind(num, start = rightInd)
      if foundInd > -1 and (foundInd > rightInd or rightInd == -1):
        ones = (i + 1)
        rightInd = foundInd

  tens + ones

const replaceArr =
  [
    ("one", "o1e"),
    ("two", "t2o"),
    ("three", "t3e"),
    ("four", "4"),
    ("five", "f5e"),
    ("six", "6"),
    ("seven", "7n"),
    ("eight", "e8t"),
    ("nine", "n9e")
  ]
proc partTwoSilly(line: string): int =
  var line = line
  for (replaceName, replace) in replaceArr:
    line = line.replace(replaceName, replace)
  line[line.find(Digits)].toInt() * 10 + line[line.rfind(Digits)].toInt()
  


proc solveIt(name: string): (int, int) =
  for line in lines(name):
    result[0] += line.findDigits()
    result[1] += line.partTwoSilly() #line.findDigits(true)

echo solveIt(paramStr(1))

