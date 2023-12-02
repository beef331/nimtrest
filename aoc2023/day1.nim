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
      var foundInd = line.find(num)
      if foundInd > -1 and (foundInd < leftInd or leftInd == -1):
        tens = (i + 1) * 10
        leftInd = foundInd

      foundInd = line.rFind(num)
      if foundInd > -1 and (foundInd > rightInd or rightInd == -1):
        ones = (i + 1)
        rightInd = foundInd

  tens + ones


proc solveIt(name: string): (int, int) =
  for line in lines(name):
    result[0] += line.findDigits()
    result[1] += line.findDigits(true)

echo solveIt(paramStr(1))

