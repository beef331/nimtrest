type
  # Need a single convention, inline seems the best?
  # It increases the chance of inlined allocations, reducing overhead(hopefully this is 0 cost then)
  AllocProc = proc(size: int): pointer {.inline.}
  DeallocProc = proc(p: pointer) {.inline.}

  JoinedInternal[T] {.pure.} = object
    ## This type exists for the cases where you want a ref count next to the data.
    refCount: int
    data: T

  JoinedCount[T]  = ptr JoinedInternal[T]

  SeperateCount[T] {.pure.} = object
    ## This type exists for the cases where you want the refcount to be held elsewhere.
    refCount: ref int
    data: ptr T

  Counts[T] = SeperateCount[T] or JoinedCount[T]

  RemoteRef[T; allocProc: static AllocProc; freeProc: static DeallocProc] = object
    ## The actual type that holds the statically typed custom allocated types.
    ## This likely could be a distinct but that seems a bit more tedious to deal with
    data: T

  ## Exposed aliases that should be used, in theory should be easy to add any other forms of refcount locations
  JoinedRef*[T; allocProc: static AllocProc; freeProc: static DeallocProc] = RemoteRef[JoinedCount[T], allocProc, freeProc]
  SeperateRef*[T; allocProc: static AllocProc; freeProc: static DeallocProc] = RemoteRef[SeperateCount[T], allocProc, freeProc]

template theData[T, aP, fP](r: RemoteRef[Counts[T], aP, fP]): var T =
  ## Returns the actual type data of the `RemoteRef`
  when r.data is JoinedCount:
    r.data[].data
  elif r.data is SeperateCount:
    r.data.data[]

template thePtr[Y; T: JoinedCount[Y], aP, fP](r: RemoteRef[T, aP, fP]): auto =
  ## Gets the `ptr` to the data of a `JoinedCount` specialised `RemoteRef`
  r.data

template thePtr[Y; T: SeperateCount[Y], aP, fP](r: RemoteRef[T, aP, fP]): auto =
  ## Gets the `ptr` to the data of a `SeperatedCount` specialised `RemoteRef`
  r.data.data

template count(r: RemoteRef): auto =
  when r.data is JoinedCount:
    r.data.refCount
  elif r.data is SeperateCount:
    r.data.refCount[]

proc `=destroy`[T, aP, fP](r: var RemoteRef[T, aP, fP]) =
  # Should destructors assert `r.thePtr != nil` or that `r.count > 0`?
  if r.thePtr != nil:
    dec r.count
    if r.count <= 0:
      r.freeProc(cast[pointer](r.thePtr))

proc `=copy`[T, aP, fP](r: var RemoteRef[T, aP, fP], rOld: RemoteRef[T, aP, fP]) =
  r = rOld
  if r.thePtr != nil:
    inc r.count

proc `=trace`[T, aP, fP](r: var RemoteRef[T, aP, fP], env: pointer) =
  if r.thePtr != nil:
    `=trace`(r.theData, env)


proc `[]`*[T, aP, fP](r: RemoteRef[Counts[T], aP, fP]): var T =
  ## Dereference the data to replicate `ref` and `ptr`
  r.theData
proc `[]=`*[T, aP, fP](r: RemoteRef[Counts[T], aP, fP], val: T) =
  ## Dereference and assign the data to replicate `ref` and `ptr`
  (r.theData) = val

template `[]`*[T,  aP, fP](r: RemoteRef[Counts[T], aP, fP], key: auto): auto =
  ## Index dereference enabling usage of collections seemlessly without `r[][key]`
  r.theData[key]
template `[]=`*(r: RemoteRef, key, val: auto) =
  ## Index assignment enabling usage of collections seemlessly without `r[][key] = val`
  r.theData[key] = val

{.experimental: "dotOperators".}

template `.`*(r: RemoteRef, field: untyped): auto =
  ## Automatic field dereference, useful for object fields and getters
  r[].field
template `.=`*(r: RemoteRef, field, val: untyped): auto =
  ## Automatic field assignment, useful for object fields and setters
  r.theData.field = val

proc new*[T, AllocProc, FreeProc](Y: typedesc[RemoteRef[Counts[T], AllocProc, FreeProc]], val: T): Y =
  ## Allocates a `T` with allocators and sets value to `val`.
  when result.data is JoinedCount:
    (result.thePtr) = cast[JoinedCount[T]](result.allocProc(sizeof(JoinedInternal[T])))
  elif result.data is SeperateCount:
    (result.thePtr) = cast[ptr T](result.allocProc(sizeof(T)))
    new result.data.refCount
  (result.theData) = val
  inc result.count

proc create*[T, AllocProc, FreeProc](Y: typedesc[RemoteRef[Counts[T], AllocProc, FreeProc]], size: int): Y =
  ## Allocates a pointer to T with allocators.
  ## With `JoinedRef` it takes  `size` + `sizeof(JoinedCount[T])`.
  ## With `SeperatedRef` it takes  `size` + `sizeof(T)`.
  when result.data is JoinedCount:
    (result.thePtr) = cast[JoinedCount[T]](result.allocProc(size + sizeof(JoinedCount[T])))
  elif result.data is SeperateCount:
    (result.thePtr) = cast[ptr T](result.allocProc(sizeof(T) + size))
    new result.data.refCount
  inc result.count

proc new*[T, AllocProc, FreeProc](Y: typedesc[RemoteRef[Counts[T], AllocProc, FreeProc]]): Y =
  ## Allocates a 0'init version of `T` with the type allocators.
  new(Y, default(T))

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

  type
    MySeqInternal[T] = object
      len: int
      data: UncheckedArray[T]
    SpiSeq[T] = JoinedRef[MySeqInternal[T], spiAlloc, spiFree]


  proc len*[T](spiSeq: SpiSeq[T]): int = spiSeq[].len
  proc high*[T](spiSeq: SpiSeq[T]): int = spiSeq.len - 1

  proc newSpiSeq*[T](size: int): SpiSeq[T] =
    result = create(SpiSeq[T], size)
    result.len = size


  proc `[]`*[T](spiSeq: SpiSeq[T], ind: int): T =
    when compileOption("checks"):
      assert ind in spiSeq.len
    spiSeq[].data[ind]

  proc `[]=`*[T](spiSeq: SpiSeq[T], ind: int, val: T) =
    when compileOption("checks"):
      assert ind in spiSeq.len
    spiSeq[].data[ind] = val

  iterator mitems*[T](spiSeq: SpiSeq[T]): var T =
    for i in 0..<spiSeq.len:
      yield spiSeq[i]

  iterator items*(spiSeq: SpiSeq): auto =
    for i in 0..<spiSeq.len:
      yield spiSeq[i]

  iterator pairs*[T](spiSeq: SpiSeq[T]): (int, T) =
    for i in 0..<spiSeq.len:
      yield (i, spiSeq[i])

  proc `$`*(spiSeq: SpiSeq): string =
    result = "["
    for ind, val in spiSeq:
      result.add $val
      if ind < spiSeq.high:
        result.add ", "
    result.add ']'


  var mySeq = newSpiSeq[byte](100)
  assert mySeq.high == 99
  assert mySeq.len == 100
  for x in mySeq:
    assert x == 0
  mySeq[0] = 30
  mySeq[1] = 50
  assert mySeq[0] == 30
  assert mySeq[1] == 50


