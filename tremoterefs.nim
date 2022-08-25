import remoterefs
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


proc `[]`*[T](spiSeq: SpiSeq[T], ind: int): var T =
  when compileOption("checks"):
    assert ind in 0..spiSeq.high
  spiSeq[].data[ind]

proc `[]=`*[T](spiSeq: SpiSeq[T], ind: int, val: T) =
  when compileOption("checks"):
    assert ind in 0..spiSeq.high
  spiSeq[].data[ind] = val

template forMIt*(spiSeq: SpiSeq, body: untyped) =
  for i in 0..spiSeq.high:
    template it: auto {.inject.} = spiSeq[i]
    body

template forIt*(spiSeq: SpiSeq, body: untyped) =
  for i in 0..spiSeq.high:
    let it {.inject.} = spiSeq[i]
    body

iterator pairs*[T](spiSeq: SpiSeq[T]): (int, T) =
  for i in 0..spiSeq.high:
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

mySeq.forIt:
  assert it == 0

mySeq.forMit:
  it = 255

mySeq.forIt:
  assert it == 255

mySeq[0] = 30
mySeq[1] = 50
assert mySeq[0] == 30
assert mySeq[1] == 50

echo cast[int](addr(mySeq[2]))
echo cast[int](addr mySeq[3])
