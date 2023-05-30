import std/[sets, isolation, typetraits, tables]
import assume/typeit

when defined(gcOrc): # Shamelessly stolen from nim-works/arc
  const
    rcIncrement = 0b10000 # so that lowest 4 bits are not touched
    rcMask = 0b1111
    rcShift = 4           # shift by rcShift to get the reference counter
else:
  const
    rcIncrement = 0b1000 # so that lowest 3 bits are not touched
    rcMask = 0b111
    rcShift = 3          # shift by rcShift to get the reference counter

template realCount(n: int): int = n shr rcShift

type
  RefHeader = object
    rc: int
    when defined(gcOrc):
      rootIdx: int
    when defined(nimArcDebug) or defined(nimArcIds):
      refId: int

  Cell = ptr RefHeader
  RefBase[T] = concept r
    r.distinctBase() is T
    T is ref
    r isnot ref
  Ref = ref or RefBase

converter toRef[T](r: RefBase[T]): T = T(r)
template head(p: Ref): Cell =
  cast[Cell](cast[int](p) -% sizeof(RefHeader))

template rc(p: Ref): int = realCount (addr(head(p)[].rc)[])

proc copyUnisolated[T: ref](r: T, remap: var Table[pointer, pointer]): T
proc copyUnisolated[T: RefBase](r: T, remap: var Table[pointer, pointer]): T =
  T copyUnisolated(r.distinctBase, remap)

proc copyToIsolated[T: ref](dst: var T, src: T, remap: var Table[pointer, pointer])
proc copyToIsolated[T: RefBase](dst: var T, src: T, remap: var Table[pointer, pointer]) =
  copyUnisolated(dst.distinctBase, src.distinctBase, remap)


proc copyToIsolated[T: ref](dst: var T, src: T, remap: var Table[pointer, pointer]) =
  if src != nil:
    for dest, source in fields(dst[], src[]):
      when dest is Ref:
        if source.distinctbase != nil and source.rc > 0:
          dest = copyUnisolated(source, remap)

proc copyUnisolated[T: ref](r: T, remap: var Table[pointer, pointer]): T =
  if r != nil and cast[pointer](r) notin remap:
    result = new T
    result[] = r[]
    remap[cast[pointer](r)] = cast[pointer](result)
    result.copyToIsolated(r, remap)
  else:
    result = r

proc isolatedCopy*[T: Ref](r: sink T): Isolated[T] =
  var remapped: Table[pointer, pointer]
  if r.isNil:
    isolate(T(nil))
  else:
    var res = new T
    res[] = r[]
    for field in res[].fields:
      when field is Ref:
        field = field.copyUnisolated(remapped)
    unsafeIsolate(res)





