import std/math

type
  Payload[T] = ptr UncheckedArray[T]

  AlignedSeq*[T] = object
    len: int
    cap: int
    alignment: int
    data: Payload[T]


proc alignedAlloc(align, size: uint): pointer {.importc:"aligned_alloc", cdecl, header: "<stdlib.h>".}
proc c_free(p: pointer) {.importc: "free", header: "<stdlib.h>", cdecl.}

proc `=destroy`[T](aseq: var AlignedSeq[T]) =
  for i in 0..<aseq.len:
    reset aseq.data[i]
  if aseq.data != nil:
    c_free(aseq.data)
    reset aseq.data
  aseq.len = 0

proc `=copy`[T](a: var AlignedSeq[T], b: AlignedSeq[T]) {.error.}
proc `=dup`[T](a: AlignedSeq[T]): AlignedSeq[T] {.error.}

proc payloadSize[T](alignment, len: int): int =
  lcm(len * sizeof(T), alignment)

proc allocPayload[T](alignment, len: int): Payload[T] =
  when defined(debugAseq):
    echo "Allocating: ", (Alignment: alignment, Len: len, Payload: payloadSize[T](alignment, len))
  # arm limits to multiples of sizeof(pointer) and must be atleast 1 pointer width
  let actualAlignment = max(alignment, sizeof(pointer))
  result = cast[Payload[T]](alignedAlloc(uint actualAlignment, uint payloadSize[T](alignment, len)))
  assert result != nil

proc init*[T](t: typedesc[AlignedSeq[T]], alignment: int, len: int, zeroMemory: bool = true): t =
  result = t(
    len: len,
    alignment: alignment,
    data: allocPayload[T](alignment, len)
  )

  if zeroMemory:
    result.data.zeroMem(uint payloadSize[T](alignment, len))
  result.cap = len

proc init*[T](t: typedesc[AlignedSeq[T]], alignment: int, cap: int = 64, zeroMemory: bool = true): t =
  result = t(
    len: 0,
    alignment: alignment,
    data: allocPayload[T](alignment, cap)
  )

  if zeroMemory:
    result.data.zeroMem(uint payloadSize[T](alignment, cap))
  result.cap = cap


proc setLen*[T](aseq: var AlignedSeq[T], len: int) =
  when defined(debugAseq):
    echo "Set len: ", len, " from ", aseq.len
  if len == aseq.len:
    return
  elif len <= aseq.cap:
    for i in len..<aseq.len:
      reset aseq.data[i]
    aseq.len = len
  else:
    let
      newSize =
        if aseq.data == nil:
          len
        else:
          aseq.cap div 2 * 3
      startLen = aseq.len
      oldData = aseq.data

    aseq.data = allocPayload[T](aseq.alignment, newSize)

    if startLen > 0:
      copyMem(aseq.data, oldData, payloadSize[T](aseq.alignment, startLen))
      zeroMem(aseq.data[startLen].addr, newSize - startLen - 1 * sizeof(T))
    else:
      zeroMem(aseq.data[startLen].addr, newSize * sizeof(T))

    if oldData != nil:
      cfree(oldData)
    aseq.cap = newSize
    aseq.len = len
  assert aseq.len == len

proc add*[T](aseq: var AlignedSeq[T], val: sink T) =
  if aseq.len == aseq.cap:
    let
      newSize = aseq.cap div 2 * 3
      oldData = aseq.data
      oldCap = aseq.cap
    aseq.data = allocPayload[T](aseq.alignment, newSize)
    copyMem(aseq.data, oldData, payloadSize[T](aseq.alignment, oldCap))
    cfree(oldData)
    aseq.cap = newSize

  aseq.data.data[aseq.len - 1] = val
  inc aseq.len

template indexCheck[T](s: AlignedSeq[T], ind: int) =
  when compileOption("boundChecks"):
    if ind notin 0..<aseq.len:
      {.line: instantiationInfo(-1).}:
        raise (ref IndexDefect)(msg: "Index " & $ind & " out of range 0..<" & $aseq.len & ".")

proc `[]`*[T](aseq: AlignedSeq[T], ind: int): lent T =
  aseq.indexCheck(ind)
  aseq.data[ind]

proc `[]`*[T](aseq: var AlignedSeq[T], ind: int): var T =
  aseq.indexCheck(ind)
  aseq.data[ind]

proc `[]=`*[T](aseq: var AlignedSeq[T], ind: int, val: sink T) =
  aseq.indexCheck(ind)
  aseq.data[ind] = val

proc len*[T](aseq: AlignedSeq[T]): int = aseq.len
proc high*[T](aseq: AlignedSeq[T]): int = aseq.len - 1
proc alignment*[T](aseq: AlignedSeq[T]): int = aseq.alignment

proc delete*[T](aseq: var AlignedSeq[T], ind: int) =
  aseq.indexCheck(ind)
  for i in countDown(aseq.high, ind):
    if i > 0:
      aseq[i - 1] = aseq[i]
  dec aseq.len

proc delete*[T](aseq: var AlignedSeq[T], rng: Slice[int]) =
  when defined(debugAseq):
    echo "Deleting ", $rng , " from seq with length:  " , aseq.len

  for i in rng:
    aseq.indexCheck(i)
    aseq[i] = aseq[rng.b + rng.a - i]
  dec aseq.len, rng.len

  when defined(debugAseq):
    echo "Len is now: ", aseq.len

iterator items*[T](aseq: AlignedSeq[T]): lent T =
  for i in 0..<aseq.len:
    yield aseq.data.data[i]

iterator mitems*[T](aseq: AlignedSeq[T]): var T =
  for i in 0..<aseq.len:
    yield aseq.data.data[i]

iterator pairs*[T](aseq: AlignedSeq[T]): (int, lent T) =
  for i in 0..<aseq.len:
    yield (i, aseq.data.data[i])

iterator mpairs*[T](aseq: AlignedSeq[T]): (int, var T) =
  for i in 0..<aseq.len:
    yield aseq.data.data[i]
