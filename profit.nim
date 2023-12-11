include system/timers
import std/[tables, sets, strutils, exitprocs, strformat, hashes, deques, sequtils, algorithm, locks, sets]

type
  MyCstring = distinct cstring
  MyStackEntry = (MyCstring, MyCstring, uint16)
  GraphEntry = object
    children: array[64, MyStackEntry]
    len: int8
    value: MyStackEntry
    samples: int32

proc hash(cstr: MyCstring): Hash = hash(cast[uint](cstr))
proc `==`(a, b: MyCstring): bool = cast[pointer](a) == cast[pointer](b)
proc `$`(cstr: MyCstring): string = $cstring(cstr)

var
  sampleLock: Lock
  sampleTable {.guard: sampleLock.} = initTable[MyStackEntry, GraphEntry]()
  totalSamples: int

iterator items(graphEntry: GraphEntry): MyStackEntry =
  for val in graphEntry.children.toOpenArray(0, graphEntry.len.int - 1):
    yield val

initLock sampleLock

proc mySortFunc(a, b: MyStackEntry): int {.raises: [].}=
  {.locks: [sampleLock].}:
    try:
      if a in sampleTable and b in sampleTable:
        cmp(sampleTable[a].samples, sampleTable[b].samples)
      else:
        0
    except:
      0

proc samplePercentage(root: GraphEntry): (int, float) {.raises: [].} =
  var
    queue {.global.}: seq[MyStackEntry] # Reuse, reduce, recycle
    sum = 0
  queue.setLen(0)
  queue.add root.value
  {.locks:[sampleLock].}:

    while queue.len > 0:
      let entry = queue.pop()
      withValue sampleTable, entry, data:
        sum += data.samples
        for entry in data[].items:
            queue.add entry
  (sum, sum / totalSamples)

proc printTree(entry: GraphEntry) {.raises: [].} =
  var msg = ""
  withLock sampleLock: # We grab the lock if we're printing
    var queue = @[(entry.value, 0)].toDeque
    while queue.len > 0:
      let (entry, indent) = queue.popFirst()
      if entry in sampleTable:
        try:
          let data = sampleTable[entry]
          msg.add indent(fmt "{data.value[0]}() {data.value[1]}:{data.value[2]}; {data.samples} times({data.samplePercentage()[1]}%).\n", indent)
          for entry in data.children.toOpenArray(0, data.len.int - 1).sorted(mySortFunc):
            queue.addFirst (entry, indent + 2)
        except KeyError as e:
          echo e.msg
          return
        except ValueError as e:
          echo e.msg
          return
  echo msg

const header =  staticRead("profit.template")

proc hotness(f: float32): (uint8, uint8, uint8) =
  (255, uint8((255 * (1 - f))), 0)

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
  {.locks: [sampleLock].}:
    var queue = [(entry.value, 1180f, 1f, 10f, 626f - 50f)].toDeque
    var count = 0
    while queue.len > 0:
      let (entry, w, percentOfParent, x, y) = queue.popFirst()
      withvalue sampleTable, entry, data:
        let (procSamples, percent) = data[].samplePercentage()
        try:
          let info = fmt"""{data.value[0]}[{data.value[1]}:{data.value[2]}]"""
          file.writeLine fmt"""
<g class="func_g" onmouseover="s('{data.value[0]} ({procSamples} samples, {percentOfParent * 100:0.2f}%)')" onmouseout="c()" onclick="zoom(this)">
<title>{info} ({procSamples} samples, {percentOfParent * 100:0.2f}%)</title>
<rect x="{x}" y="{y}" width="{w}" height="15.0" fill="rgb{hotness(percentOfParent)}" rx="2" ry="2"></rect>
<text text-anchor="" x="{x}" y="{y.float32 + 12}" width="{w}" font-size="12" font-family="Verdana" fill="rgb(0,0,0)">{info}</text>
</g>"""
        except Exception as e:
          echo "Failed to write to file:", e.msg
          return
        var offset = 0f
        var sum = 0
        data.children.toOpenArray(0, data.len.int - 1).sort(mySortFunc)
        for entry in data[].items:
          withValue sampleTable, entry, data: 
            let
              (samples, _) = data[].samplePercentage()
              percentOfParent = float32(samples / procSamples)
              width = float32 max(w * percentOfParent, 1)
            sum += samples
            queue.addLast (entry, width, percentOfParent, x + offset, y - 15)
            offset += width
        if sum > procSamples:
          echo sum, " ", procSamples

      inc count

  

{.push profiler: off.}
import std/[stackframes]

var root = (MyCstring cstring"all", MyCstring cstring"", 0u16)
{.locks:[sampleLock].}:
  sampleTable[root] = GraphEntry(value: root)



addExitProc proc() {.noconv.} =
  withLock sampleLock:
    when defined(profit.print):
      root.printTree()
    else:
      sampleTable[root].writeXml()
  

proc hook(st: StackTrace) {.nimcall, raises:[].} =
  if tryAcquire(sampleLock): # We only write if we can get lock, skips samples, but faster
    defer: release sampleLock
    {.cast(raises:[]), locks:[sampleLock].}:
      var presentFrame = getFrame().prev
      if presentFrame == nil: return
      var pFrame = (MyCstring presentFrame.procname, MyCstring presentFrame.filename, uint16 presentFrame.line)
      if sampleTable.hasKeyOrPut(pframe, GraphEntry(value: pFrame, samples: 1)):
        inc sampleTable[pFrame].samples

      inc totalSamples
      while presentFrame != nil: 
        let 
          lastFrame = presentFrame.prev
          pFrame = (MyCstring presentFrame.procname, MyCstring presentFrame.filename, uint16 presentFrame.line)

        if lastFrame != nil:
          let lFrame = (MyCstring lastFrame.procname, MyCstring lastFrame.filename, uint16 lastFrame.line)
          withValue sampleTable, lFrame, val:
            if val.len.int < val.children.len and pFrame notin val.children.toOpenArray(0, val.len.int - 1):
              val.children[int val.len] = pFrame
              inc val.len
          do:
            var entry = GraphEntry(value: lFrame, samples: 0, len: 1)
            entry.children[0] = pFrame
            sampleTable[lFrame] = entry 
        else:
          withValue sampleTable, root, val:
            if val.len.int < val.children.len and pFrame notin val.children.toOpenArray(0, val.len.int - 1):
              val.children[int val.len] = pFrame
              inc val.len
        presentFrame = lastFrame

var 
  ticks: int
  tickDelay = 30

proc setTickDelay*(delay: int) =
  ## Sets the tick delay, this is a procedure so you can set the sampler rate from CLI or anywhere else(ENV var)
  tickDelay = delay

proc reqHook(): bool {.nimcall.} =
  if ticks == 0:
    result = true
    ticks = tickDelay
  dec ticks


{.pop.}
profilingRequestedHook = reqHook
profilerHook = hook

import std/[json, os]
setTickDelay(parseInt(paramStr(1)))
discard parseJson(readFile("5MB.txt"))

