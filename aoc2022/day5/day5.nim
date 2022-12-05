import std/[times, monotimes, strutils, parseutils, algorithm, strscans]


proc problem(): (string, string)=
  var
    stackA, stackB: array[9, seq[char]]
    stacksLoaded = false

  for line in lines"input.txt":
    if stacksLoaded:
      var howMany, frm, to: int
      if line.scanf("move $i from $i to $i", howMany, frm, to):
        dec frm
        dec to
        for i in 0..<howMany:
          stackA[to].add stackA[frm].pop()
        stackB[to].add stackB[frm].toOpenArray(stackB[frm].len - howMany, stackB[frm].high)
        stackB[frm].setLen(stackB[frm].len - howMany)

    else:
      if line.skipUntil(Digits) == 1:
        stacksLoaded = true
        for stack in stackA.mitems:
          stack.reverse()
        stackB = stackA
      else:
        var i = 0
        while i < line.len:
          i += line.skipUntil('[', i)
          if i < line.high:
            stackA[i div 4].add line[i + 1]
          inc i, 2

  for stack in stackA:
    result[0].add stack[^1]
  for stack in stackB:
    result[1].add stack[^1]

let
  start = getMonoTime()
  (part1, part2) = problem()
echo "Answer: Part1 - ", part1 , " Part2 - ", part2, " | ", getMonoTime() - start
