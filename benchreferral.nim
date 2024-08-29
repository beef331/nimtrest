import referral
import pkg/criterion
import std/random

type
  RefTree = ref object
    val: int
    next: RefTree

  ValueTree = object
    val: int
    next: CountedIndex[ValueTree]


var
  rootTree = RefTree()
  currentRef = rootTree

  arena = CountedSeqArena[ValueTree].new(123)
  rootVal = CountedIndex[ValueTree]()
  currentVal = CountedIndex[ValueTree]()

randomize(300)

for x in 0..1_000_000:
  let val = rand(10000)

  currentRef.next = RefTree(val: val)
  currentRef = currentRef.next
  if currentVal.isNil:
    currentVal = arena.new(ValueTree(val: val))
    rootVal = currentVal # We always know it's ind: 0, but meh
  else:
    let next = arena.new(ValueTree(val: val))
    currentVal[].next = next
    currentVal = next

proc countValues(): int =
  var node = rootVal
  while node != nil:
    result += node[].val
    node = node[].next


var cfg = newDefaultConfig()
benchmark cfg:
  proc countRefs(): int =
    var node = rootTree
    while node != nil:
      result += node.val
      node = node.next

  proc refBench() {.measure.} =
    blackBox countRefs()

  proc countValues(): int =
    var node = rootVal
    while node != nil:
      result += node[].val
      node = node[].next

  proc valueBench() {.measure.} =
    blackBox countValues()
