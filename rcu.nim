import std/[atomics, os]
type
  RcuNodeImpl*[T] = object
    data: T
    parent, next: RcuNode[T]

  RcuNode*[T] = ptr RcuNodeImpl[T]

  RcuList*[T; Reclaimable: static bool] = object
    root, tail: RcuNode[T]
    finished*: Atomic[int]
    reclaimQueue: seq[RcuNode[T]]

proc `=wasMoved`[T, Y](rcu: var RcuList[T, Y])
proc `=destroy`[T, Y](rcu: RcuList[T, Y])

proc `=wasMoved`[T, Y](rcu: var RcuList[T, Y]) =
  rcu.reclaimQueue.setLen(0)
  rcu.root = nil
  rcu.tail = nil

proc `=destroy`[T, Y](rcu: RcuList[T, Y]) =
  var node = rcu.root
  while node != nil:
    let next = node.next
    `=destroy`(node.data)
    deallocShared(node)   
    node = next
  `=destroy`(rcu.reclaimQueue)

proc `=copy`[T, Y](dest: var RcuList[T,  Y], src: RcuList[T, Y]) {.error: "Copying RcuList should not happen, there are no ref counts so will result in double frees.".}
proc `=dup`[T, Y](dest: RcuList[T,  Y]): RcuList[T, Y] {.error: "Copying RcuList should not happen, there are no ref counts so will result in double frees.".}

proc new*[T](_: typedesc[RcuNode], data: T, parent: RcuNode[T] = nil, next: RcuNode[T] = nil): RcuNode[T] = 
  result = cast[RcuNode[T]](createShared(typeof RcuNodeImpl[T]))
  result.data = data
  result.parent = parent
  result.next = next

proc addAtEnd*[T, Y](rcu: var RcuList[T, Y], data: T): RcuNode[T] {.discardable.} =
  if rcu.tail == nil:
    rcu.tail = RcuNode.new(data)
    rcu.root = rcu.tail
  else:
    rcu.tail.next = RcuNode.new(data, rcu.tail)
    rcu.tail = rcu.tail.next
  rcu.tail

proc addAtStart*[T, Y](rcu: var RcuList[T, Y], data: T): RcuNode[T] {.discardable.} =
  rcu.root = RcuNode.new(data, nil, rcu.root)
  rcu.root

proc insert*[T, Y](rcu: var RcuList[T, Y], data: T, parent: RcuNode[T]): RcuNode[T] {.discardable.} =
  let next = parent.next
  parent.next = RcuNode.new(data, parent, parent.next)
  parent.next

proc remove*[T, Y](rcu: var RcuList[T, Y], toRemove: RcuNode[T]) =
  rcu.reclaimQueue.add toRemove
  if toRemove.parent == nil:
    rcu.root = toRemove.next
    rcu.root.parent = nil
  else:
    toRemove.parent.next = toRemove.next
    if toRemove.next != nil:
      toRemove.next.parent = toRemove.parent

proc waitUntilFinished*[T](rcuList: var RcuList[T, false]): var RcuList[T, true] =
  while rcuList.finished.load <= 0:
    discard
  result = cast[ptr RcuList[T, true]](rcuList.addr)[] # Sadly cannot just do `RcuList[T, true]`

proc reclaim*[T](rcuList: var RcuList[T, true]): var RcuList[T, true] =
  for x in rcuList.reclaimQueue:
    `=destroy`(x.data)
    deallocShared(x)
  rcuList.reclaimQueue.setLen(0)
  result = rcuList

proc threadProcess[T, Y](input: (int, ptr RcuList[T, Y])) =
  discard existsOrCreateDir("output")
  let file = open("output" / $input[0], fmWrite)
  var node = input[1].root
  while node != nil:
    file.writeLine(node.data)
    node = node.next
  input[1].finished.atomicDec
  close(file)
  echo "Finished: ", input[0]


proc main() =
  var 
    myList = RcuList[int, false]()
    toRemove: seq[RcuNode[int]]

  for x in 0..1000:
    let node = myList.addAtEnd(x)
    if x mod 10 == 0:
      toRemove.add node

  var threads: array[8, Thread[(int, ptr RcuList[int, false])]]
  myList.finished.store(threads.len)
  
  for i, x in threads.mpairs:
    x.createThread(threadProcess[int, false], (i, myList.addr))

  for x in toRemove:
    myList.remove(x)
        

  discard myList.waitUntilFinished().reclaim()

main()


