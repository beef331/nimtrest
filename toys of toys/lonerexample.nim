import std / [json, isolation]
import ../loner
import threading / channels

type
  MyRef = distinct Test
  Test = ref object
    data: string
    r: MyRef
  Tree = ref object
    left, right: Test

var chan = newChan[Tree]()
var thr: Thread[void]

proc worker() {.thread.} =
  var x: Tree
  chan.recv(x) # somebody should fix this API...
  echo "received ", x.left.data, " ", x.right.data

createThread thr, worker
let hello = Test(data: "Hello", r: MyRef(Test(data: "hmmm")))
var meh = hello
chan.send isolatedCopy(Tree(left: hello, right: Test(data: "world")))
joinThread thr
echo meh[]
