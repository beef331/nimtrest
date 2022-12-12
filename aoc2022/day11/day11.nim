import std/[parseutils, strscans, streams, strutils, times, monotimes]
type
  Monkey = object
    items: seq[int]
    doesMultiply: bool
    modifier: int
    divVal: int
    target: array[bool, int]
    inspects: int

proc parseMonkey(fs: Stream, buffer: var string, lcd: var int): Monkey =
  doAssert fs.readLine(buffer)
  var
    i = buffer.skipUntil(Digits)
    myInt = 0
  while i < buffer.len:
    i += buffer.parseInt(myInt, i) + 2
    result.items.add myInt

  doAssert fs.readLine(buffer)
  var op: char
  if buffer.scanf("  Operation: new = old $c old", op):
    result.modifier = -1
  else:
    doAssert buffer.scanf("  Operation: new = old $c $i", op, result.modifier)
  result.doesMultiply = op == '*'

  doAssert fs.readLine(buffer)
  doAssert buffer.scanf("  Test: divisible by $i", result.divVal)

  lcd *= result.divVal

  doAssert fs.readLine(buffer)
  doAssert buffer.scanf("    If true: throw to monkey $i", result.target[true])

  doAssert fs.readLine(buffer)
  doAssert buffer.scanf("    If false: throw to monkey $i", result.target[false])

proc processMonkey(monkeys: var openarray[Monkey], monkey: var Monkey, lcd: int = -1) =
  for val in monkey.items.items:
    var newWorry = -1
    if monkey.doesMultiply:
      if monkey.modifier == -1:
        newWorry = val * val
      else:
        newWorry = val * monkey.modifier
    else:
      if monkey.modifier == -1:
        newWorry = val + val
      else:
        newWorry = val + monkey.modifier

    if lcd == -1:
      newWorry = newWorry div 3
    else:
      newWorry = newWorry mod lcd

    let target = monkey.target[newWorry mod monkey.divVal == 0]
    inc monkey.inspects
    monkeys[target].items.add newWorry

  monkey.items.setLen(0)

proc solution: (int, int) =
  let fs = newFileStream("input.txt")
  defer: fs.close()
  var
    buffer = newStringOfCap(50)
    monkeys: seq[Monkey]
    lcd = 1
  var start = getMonoTime()
  while not fs.atEnd():
    discard fs.readLine(buffer)
    if buffer.startsWith("Monkey"):
      monkeys.add parseMonkey(fs, buffer, lcd)

  echo "Parse: ", getMonoTime() - start
  var monkeysB = monkeys

  start = getMonoTime()

  for x in 0..<20:
    for monkey in monkeys.mitems:
      monkeys.processMonkey(monkey)

  var largest: array[2, int]

  for monkey in monkeys:
    if monkey.inspects > largest[0]:
      largest[1] = largest[0]
      largest[0] = monkey.inspects

    elif monkey.inspects > largest[1]:
      largest[1] = monkey.inspects

  result[0] = largest[0] * largest[1]

  echo "Pt1: ", getMonoTime() - start


  start = getMonoTime()
  for x in 0..<10000:
    for monkey in monkeysB.mitems:
      monkeysB.processMonkey(monkey, lcd)


  for monkey in monkeysB:
    if monkey.inspects > largest[0]:
      largest[1] = largest[0]
      largest[0] = monkey.inspects

    elif monkey.inspects > largest[1]:
      largest[1] = monkey.inspects
  result[1] = largest[0] * largest[1]

  echo "Pt2: ", getMonoTime() - start


echo solution()
