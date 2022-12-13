import std/[parseutils, enumerate, streams, strutils]

type
  Comparison = enum Less, Equal, Greater
  PacketKind = enum Entry, List
  Packet = object
    case kind: PacketKind
    of Entry:
      val: int
    of List:
      entries: seq[Packet]

proc packetEntry(val: int) : Packet = Packet(kind: Entry, val: val)
proc packetList(val: Packet): Packet = Packet(kind: List, entries: @[val])
proc packetList(): Packet = Packet(kind: List)

proc `$`(packet: Packet): string =
  case packet.kind
  of Entry:
    result = $packet.val
  of List:
    result = "["
    for i, entr in packet.entries.pairs:
      result.add $entr
      if i < packet.entries.high:
        result.add ", "
    result.add "]"

proc compare(a, b: Packet): Comparison =
  case a.kind
  of Entry:
    case b.kind
    of Entry:
      if a.val < b.val:
        result = Less
      elif a.val == b.val:
        result = Equal
      else:
        result = Greater
    of List:
      result = packetList(a).compare(b)
  of List:
    case b.kind
    of Entry:
      result = a.compare packetList(b)
    of List:
      let
        aLen = a.entries.len
        bLen = b.entries.len
      if bLen == 0 and aLen > 0:
        result = Greater
      elif aLen == 0 and bLen > 0:
        result = Less
      else:
        for i in 0..<min(aLen, bLen):
          result = a.entries[i].compare(b.entries[i])
          if result in {Greater, Less}:
            break

        if result == Equal:
          if aLen > bLen:
            result = Greater
          elif aLen < bLen:
            result = Less


  #echo a, " is ", result, " ", b

proc parsePacket(line: string, pos: var int): Packet =
  result = packetList()
  var val: int
  while pos < line.len:
    case line[pos]
    of '[':
      inc pos
      result.entries.add parsePacket(line, pos)
    of ']':
      inc pos
      return
    of Digits:
      pos += line.parseInt(val, pos)
      result.entries.add packetEntry(val)
    else:
      inc pos

proc parseInput(): seq[array[2, Packet]] =
  let fs = newFileStream("test.txt")
  defer: fs.close()
  var buffer = newStringOfCap(80)
  while not fs.atEnd():
    var
      i = 1
      arr: array[2, Packet]
    doAssert fs.readLine(buffer)
    arr[0] = buffer.parsePacket(i)
    i = 1
    doAssert fs.readLine(buffer)
    arr[1] = buffer.parsePacket(i)
    if fs.peekLine(buffer):
      discard fs.readLine(buffer)
    result.add arr



let packets = parseInput()
var val = 0

for i, x in packets:
  #echo "\n", x[0], " vs. ", x[1]
  if x[0].compare(x[1]) != Greater:
    echo i + 1
    val += i + 1

echo val
