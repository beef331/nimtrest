type
  # Need a single convention, inline seems the best?
  # It increases the chance of inlined allocations, reducing overhead(hopefully this is 0 cost then)
  AllocProc = proc(size: int): pointer {.inline.}
  DeallocProc = proc(p: pointer) {.inline.}
  JoinedInternal[T] {.pure.} = object
    refCount: int
    data: T
  JoinedCount[T]  = ptr JoinedInternal[T]
  SeperateCount[T] {.pure.} = object
    refCount: ref int
    data: ptr T
  Counts[T] = SeperateCount[T] or JoinedCount[T]
  RemoteRef[T; allocProc: static AllocProc; freeProc: static DeallocProc] = object
    data: T
  JoinedRef*[T; allocProc: static AllocProc; freeProc: static DeallocProc] = RemoteRef[JoinedCount[T], allocProc, freeProc]
  SeperateRef*[T; allocProc: static AllocProc; freeProc: static DeallocProc] = RemoteRef[SeperateCount[T], allocProc, freeProc]

template getData(r: RemoteRef): auto =
  when r.data is JoinedCount:
    r.data[].data
  elif r.data is SeperateCount:
    r.data.data[]

template getPtr(r: RemoteRef): auto =
 when r.data is JoinedCount:
   r.data
 elif r.data is SeperateCount:
   r.data.data

template hasData(r: RemoteRef): auto =
  when r.data is JoinedCount:
    r.data != nil
  elif r.data is SeperateCount:
    r.data.data != nil

template count(r: RemoteRef): auto =
  when r.data is JoinedCount:
    r.data.refCount
  elif r.data is SeperateCount:
    r.data.refCount[]

proc `=destroy`[T, aP, fP](r: var RemoteRef[T, aP, fP]) =
  if r.hasData:
    dec r.count
    if r.count <= 0:
      r.freeProc(cast[pointer](r.getPtr))

proc `=copy`[T, aP, fP](r: var RemoteRef[T, aP, fP], rOld: RemoteRef[T, aP, fP]) =
  r = rOld
  if r.hasData:
    inc r.count

proc `=trace`[T, aP, fP](r: var RemoteRef[T, aP, fP], env: pointer) =
  if r.hasData != nil:
    `=trace`(r.getData, env)


proc `[]`*[T, aP, fP](r: RemoteRef[Counts[T], aP, fP]): var T = r.getData
proc `[]=`*[T, aP, fP](r: RemoteRef[Counts[T], aP, fP], val: T) = (r.getData) = val

template `[]`*[T,  aP, fP](r: RemoteRef[Counts[T], aP, fP], key: auto): auto = r.getData[key]
template `[]=`*(r: RemoteRef, key, val: auto) = r.getData[key] = val

{.experimental: "dotOperators".}

template `.`*(r: RemoteRef, field: untyped): auto = r[].field
template `.=`*(r: RemoteRef, field, val: untyped): auto = r.getData.field = val

proc new*[T, AllocProc, FreeProc](Y: typedesc[RemoteRef[Counts[T], AllocProc, FreeProc]], val: T): Y =
  when result.data is JoinedCount:
    (result.getPtr) = cast[JoinedCount[T]](result.allocProc(sizeof(JoinedInternal[T])))
  elif result.data is SeperateCount:
    (result.getPtr) = cast[ptr T](result.allocProc(sizeof(T)))
    new result.data.refCount
  (result.getData) = val
  inc result.count

proc new*[T, AllocProc, FreeProc](Y: typedesc[RemoteRef[Counts[T], AllocProc, FreeProc]]): Y =
  when result.data is JoinedCount:
    (result.getPtr) = cast[JoinedCount[T]](result.allocProc(sizeof(JoinedInternal[T])))
  elif result.data is SeperateCount:
    (result.getPtr) = cast[ptr T](result.allocProc(sizeof(T)))
    new result.data.refCount
  (result.getData) = default(typeof(result.getData))
  inc result.count

when isMainModule:
  proc spiAlloc(size: int): pointer {.inline.} =
    alloc(size)
  proc spiFree(p: pointer) {.inline.} =
    dealloc(p)

  type
    SpiRef*[T] = SeperateRef[T, spiAlloc, spiFree]
    OtherRef*[T] = JoinedRef[T, spiAlloc, spiFree]
    SpiCharray = SpiRef[array[4, char]]
    OtherCharray = OtherRef[array[4, char]]

  block:
    var res = SpiCharray.new(['A', 'B', 'C', '\0'])
    res[0] = 'D'
    proc assign(a: var char, b: char) = a = b
    res[0].assign('d')
    echo res[]
    res[] = ['A', 'B', 'C', 'D']
    res = SpiCharray.new()

    type MyObj = object
      x, y: int
    var test = SpiRef[MyObj].new(MyObj(x: 100, y: 200))
    test.x = 300
    echo test.x
    echo test.y

  block:
    var res = OtherCharray.new(['A', 'B', 'C', '\0'])
    res[0] = 'D'
    proc assign(a: var char, b: char) = a = b
    res[0].assign('d')
    echo res[]
    res[] = ['A', 'B', 'C', 'D']
    res = OtherCharray.new()

    type MyObj = object
      x, y: int
    var test = OtherRef[MyObj].new(MyObj(x: 100, y: 200))
    test.x = 300
    echo test.x
    echo test.y
