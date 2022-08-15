type
  # Need a single convention, inline seems the best?
  # It increases the chance of inlined allocations, reducing overhead(hopefully this is 0 cost then)
  AllocProc = proc(size: int): pointer {.inline.}
  DeallocProc = proc(p: pointer) {.inline.}
  HeapData[T] {.pure.} = object
    refCount: int
    data: T
  RemoteRef[T; allocProc: static AllocProc; freeProc: static DeallocProc] = object
    heapData: ptr HeapData[T]

template refCount(remoteRef: RemoteRef): int =
  remoteRef.heapData.refCount

template data(remoteRef: RemoteRef): auto =
  remoteRef.heapData.data


proc `=destroy`[T, aP, fP](r: var RemoteRef[T, aP, fP]) =
  if r.heapData != nil:
    dec r.refCount
    if r.refCount <= 0:
      r.freeProc(cast[pointer](r.heapData))

proc `=copy`[T, aP, fP](r: var RemoteRef[T, aP, fP], rOld: RemoteRef[T, aP, fP]) =
  r = rOld
  if r.heapData != nil:
    inc r.refCount

proc `=trace`[T, aP, fP](r: var RemoteRef[T, aP, fP], env: pointer) =
  if r.heapData != nil:
    `=trace`(r.heapData[], env)

proc `[]`*[T,X,Y](r: RemoteRef[T, X, Y]): var T = r.data
proc `[]=`*[T,X,Y](r: RemoteRef[T, X, Y], val: T) = (r.data) = val

template `[]`*[T,X,Y](r: RemoteRef[T, X, Y], key: auto): auto = r.heapData.data[key]
template `[]=`*(r: RemoteRef, key, val: auto) = r.heapData.data[key] = val

{.experimental: "dotOperators".}

template `.`*(r: RemoteRef, field: untyped): auto = r[].field
template `.=`*(r: RemoteRef, field, val: untyped): auto = r.data.field = val

proc new*[T, AllocProc, FreeProc](Y: typedesc[RemoteRef[T, AllocProc, FreeProc]], val: T): Y =
  result.heapData = cast[ptr HeapData[T]](result.allocProc(sizeof(HeapData[T])))
  result.heapData.data = val
  inc result.refCount

proc new*[T, AllocProc, FreeProc](Y: typedesc[RemoteRef[T, AllocProc, FreeProc]]): Y =
  result.heapData = cast[ptr HeapData[T]](result.allocProc(sizeof(HeapData[T])))
  result.heapData.data = default(T)
  inc result.refCount

when isMainModule:
  proc spiAlloc(size: int): pointer {.inline.} =
    alloc(size)
  proc spiFree(p: pointer) {.inline.} =
    dealloc(p)

  type
    SpiRef*[T] = RemoteRef[T, spiAlloc, spiFree]
    SpiCharray = SpiRef[array[4, char]]


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
