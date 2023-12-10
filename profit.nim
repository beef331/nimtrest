include system/timers
import std/[tables, sets, strutils, exitprocs, strformat, hashes, deques, sequtils, algorithm, locks, sets]

type
  MyCstring = distinct cstring
  MyStackEntry = (MyCstring, MyCstring, uint16)
  GraphEntry = object
    children: array[30, MyStackEntry]
    len: uint8
    value: MyStackEntry
    samples: int

proc hash(cstr: MyCstring): Hash = hash(cast[uint](cstr))
proc `==`(a, b: MyCstring): bool = cast[pointer](a) == cast[pointer](b)
proc `$`(cstr: MyCstring): string = $cstring(cstr)

var
  sampleLock: Lock
  sampleTable {.guard: sampleLock.} = initTable[MyStackEntry, GraphEntry]()
  totalSamples: int

initLock sampleLock

proc mySortFunc(a, b: MyStackEntry): int =
  {.locks: [sampleLock].}:
    if a in sampleTable and b in sampleTable:
      cmp(sampleTable[a].samples, sampleTable[b].samples)
    else:
      1
proc samplePercentage(root: GraphEntry): float64 =
  var
    queue = @[root.value]
    sum = 0
    counted: HashSet[MyStackEntry]
  {.locks:[sampleLock].}:
    while queue.len > 0:
      let entry = queue.pop()
      withValue sampleTable, entry, data:
        if entry notin counted:
          sum += data.samples
          counted.incl entry
        for entry in data.children.toOpenArray(0, data.len.int - 1).sorted(mySortFunc):
          queue.add entry
  sum / totalSamples * 100

proc printTree(entry: GraphEntry) {.raises: [].} =
  var msg = ""
  withLock sampleLock: # We grab the lock if we're printing
    var queue = @[(entry.value, 0)].toDeque
    while queue.len > 0:
      let (entry, indent) = queue.popFirst()
      if entry in sampleTable:
        try:
          let data = sampleTable[entry]
          msg.add indent(fmt "{data.value[0]}() {data.value[1]}:{data.value[2]}; {data.samples} times({data.samplePercentage}%).\n", indent)
          for entry in data.children.toOpenArray(0, data.len.int - 1).sorted(mySortFunc):
            queue.addFirst (entry, indent + 2)
        except KeyError as e:
          echo e.msg
          return
        except ValueError as e:
          echo e.msg
          return
  echo msg

const header =  """
<svg version="1.1" width="1280" height="720" xmlns="http://www.w3.org/2000/svg">
"""


proc writeXml(entry: GraphEntry) {.raises: [].} =
  var file =
    try:
      let res = open("profit.svg", fmWrite)
      res.writeLine header
      res

    except CatchableError as e:
      echo "Failed to open file: ", e.msg
      return

  defer:
    {.cast(raises: []).}:
      file.writeLine "</svg>"
    file.close()
  withLock sampleLock: # We grab the lock if we're printing
    var queue = [(entry.value, 1280f, 720f)].toDeque
    var count = 0
    while queue.len > 0:
      let (entry, w, h) = queue.popFirst()
      if entry in sampleTable:
        try:
          let data = sampleTable[entry]
          try:
            let percent = data.samplePercentage
            file.writeLine fmt"""<text y = "{count * 10}" w = "{int(w * percent)}"> {data.value[0]}({data.samples} samples {percent:0.2f}%) </text>"""
          except Exception as e:
            echo "Failed to write to file:", e.msg
            return
          for entry in data.children.toOpenArray(0, data.len.int - 1).sorted(mySortFunc):
            queue.addLast (entry, w, h)
        except KeyError as e:
          echo e.msg
          return
        except ValueError as e:
          echo e.msg
          return
      inc count

  

{.push profiler: off.}
import std/[stackframes]

var 
  root: GraphEntry
  rootSet = false


addExitProc proc() {.noconv.} =
  when defined(profit.print):
    root.printTree()
  else:
    root.writeXml()
  

proc hook(st: StackTrace) {.nimcall, raises:[].} =
  if tryAcquire(sampleLock): # We only write if we can get lock, skips samples, but faster
    defer: release sampleLock
    {.cast(raises:[]), locks:[sampleLock].}:
      var presentFrame = getFrame().prev
      if presentFrame == nil: return
      var pFrame = (MyCstring presentFrame.procname, MyCstring presentFrame.filename, uint16 presentFrame.line)
      if sampleTable.hasKeyOrPut(pframe, GraphEntry(value: pFrame, samples: 1)):
        inc sampleTable[pFrame].samples

      let startFrame = presentFrame
      inc totalSamples
      while presentFrame != nil: 
        let 
          lastFrame = presentFrame.prev
          pFrame = (MyCstring presentFrame.procname, MyCstring presentFrame.filename, uint16 presentFrame.line)

        if lastFrame != nil:
          let lFrame = (MyCstring lastFrame.procname, MyCstring lastFrame.filename, uint16 lastFrame.line)
          withValue sampleTable, lFrame, val:
            if val.len.int < val.children.len and pFrame notin val.children.toOpenArray(0, val.len.int - 1):
              val.children[val.len] = pFrame
              inc val.len
          do:
            var entry = GraphEntry(value: lFrame, samples: 0, len: 1)
            entry.children[0] = pFrame
            sampleTable[lFrame] = entry 
        elif not rootSet and pFrame in sampleTable:
          root = sampleTable[pFrame]
          rootSet = true
        presentFrame = lastFrame


var ticks: int

proc reqHook(): bool {.nimcall.} =
  if ticks == 0:
    result = true
    ticks = 300
  dec ticks


{.pop.}
profilingRequestedHook = reqHook
profilerHook = hook

import std/json
discard parseJson(readFile("5MB.txt"))

