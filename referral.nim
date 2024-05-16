type
  Ref[T; Container] = object
    container: Container
    ind: int
  RefCounted[T; Container] = object
    container: Container
    ind: int

proc `=destroy`[T, Container](r: RefCounted[T, Container]) =
  mixin decCount
  if r.container != nil:
    r.container.decCount(r.ind)  
    `=destroy`(r.container)

proc `=copy`[T, Container](a: var RefCounted[T, Container], b: RefCounted[T, Container]) =
  mixin incCount
  `=destroy`(a)
  a.container = b.container
  a.ind = b.ind
  a.container.incCount(a.ind)

proc `=destroy`[T, Container](r: Ref[T, Container]) =
  mixin delete
  if r.container != nil:
    r.container.delete(r.ind)  
    `=destroy`(r.container)

proc `=copy`[T, Container](a: var Ref[T, Container], b: Ref[T, Container]) {.error.}
proc `=dup`[T, Container](a: Ref[T, Container]): Ref[T, Container] {.error.}


import std/intsets

type 
  SeqArena[T] = ref object
    data: seq[T]
    dead: IntSet

proc new[T](R: typedesc[SeqArena[T]], size = 64): R =
  R(data: newSeqOfCap[T](size))

proc nextIndex[T](arena: SeqArena[T]): int =
  result = arena.data.len
  if arena.dead.len > 0:
    for x in arena.dead:
      result = x
      arena.dead.excl x
      break

proc delete[T](arena: SeqArena[T], i: int) =
  reset arena.data[i]
  arena.dead.incl i

proc new[T](arena: SeqArena[T], val: sink T): Ref[T, SeqArena[T]] =
  let ind = arena.nextIndex()
  arena.data.setLen(max(arena.data.len, ind + 1))
  arena.data[ind] = val
  Ref[T, SeqArena[T]](
    container: arena,
    ind: ind)

type 
  CountedSeqArena[T] = ref object
    data: seq[T]
    dead: IntSet
    counts: seq[int]
  CountedIndex[T] = RefCounted[T, CountedSeqArena[T]]

proc new[T](R: typedesc[CountedSeqArena[T]], size = 64): R =
  R(data: newSeqOfCap[T](size), counts: newSeq[int](size))

proc nextIndex[T](arena: CountedSeqArena[T]): int =
  result = arena.data.len
  if arena.dead.len > 0:
    for x in arena.dead:
      result = x
      arena.dead.excl x
      break

proc decCount[T](arena: CountedSeqArena[T], i: int) =
  dec arena.counts[i]
  if arena.counts[i] == 0:
    reset arena.data[i]
    arena.dead.incl i

proc incCount[T](arena: CountedSeqArena[T], i: int) =
  inc arena.counts[i]

proc new[T](arena: CountedSeqArena[T], val: sink T): CountedIndex[T] =
  let ind = arena.nextIndex()
  arena.data.setLen(max(arena.data.len, ind + 1))
  arena.counts.setLen(arena.data.len)
  arena.counts[ind] = 1
  arena.data[ind] = val
  RefCounted[T, CountedSeqArena[T]](
    container: arena,
    ind: ind
    )

proc `[]`[T](arena: CountedSeqArena[T], ind: CountedIndex[T]): var T =
  assert arena.counts[ind.ind] > 0
  arena.data[ind.ind]

proc `[]=`[T](arena: CountedSeqArena[T], ind: CountedIndex[T], val: sink T) =
  assert arena.counts[ind.ind] > 0
  arena.data[ind.ind] = val

when isMainModule:
  var 
    myArena = SeqArena[int].new(32)
    myData: seq[typeof(myArena.new(0))]

  for x in 0..31:
    myData.add myArena.new(10)
    echo myData[^1].ind

  for x in 0..31:
   echo myArena.new(10).ind

  var countedArena = CountedSeqArena[int].new(32)
  let 
    data = countedArena.new(10)
    otherData = data

  countedArena[otherData] = 400
  echo countedArena[otherData]
  echo countedArena[data]
