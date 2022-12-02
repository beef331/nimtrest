import std/[times, monotimes]
type
  Move = enum
    Rock
    Paper
    Scissors
  PlayState = enum
    Lose = 0
    Draw = 3
    Win = 6

const totalMoves = Move.high.ord + 1

proc getMove(c: char): Move =
  case c
  of 'A', 'X': Rock
  of 'B', 'Y': Paper
  of 'C', 'Z': Scissors
  else: raise newException(ValueError, "bleh")

proc getTargetMove(c: char, pastMove: Move): Move =
  case c
  of 'X': Move((pastMove.ord - 1 + totalMoves) mod totalMoves) # lose
  of 'Y': pastMove # draw
  of 'Z': Move((pastMove.ord + 1 + totalMoves) mod totalMoves) # win
  else: raise newException(ValueError, "bleh2")


proc playState(a, b: Move): PlayState =
  if a == b:
    Draw
  elif Move((a.ord + 1 + totalMoves) mod totalMoves) == b:
    Lose
  else:
    Win

let start = getMonoTime()

var 
  playScore = 0
  guideScore = 0

for line in lines("input.txt"):
  if line.len == 3:
    let
      playedMove = line[0].getMove()
      playerMove = line[2].getMove()
      targetMove = line[2].getTargetMove(playedMove)

    playScore += playerMove.playState(playedMove).ord + playerMove.ord + 1
    guideScore += targetMove.playState(playedMove).ord + targetMove.ord + 1
  
echo "Answer: Part1 - ", playScore, " | Part2 - ", guideScore, " | ", getMonoTime() - start


