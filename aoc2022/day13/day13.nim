import std/[parseutils, enumerate, streams, strutils, sugar, algorithm]

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
            return

        if aLen > bLen:
          result = Greater
        elif aLen < bLen:
          result = Less
        else:
          result = Equal

proc myCompare(a, b: Packet): int = ord(compare(a, b)) - 1

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

proc parseInput(): seq[Packet] =
  let fs = newFileStream("input.txt")
  defer: fs.close()
  var buffer = newStringOfCap(80)
  while not fs.atEnd():
    var i = 1
    doAssert fs.readLine(buffer)
    result.add buffer.parsePacket(i)
    i = 1
    doAssert fs.readLine(buffer)
    result.add buffer.parsePacket(i)
    if fs.peekLine(buffer):
      discard fs.readLine(buffer)

import std/[times, monotimes]

var start = getMonoTime()
let packets = parseInput()


let
  sixPacket = packetList(packetEntry(6))
  twoPacket = packetList(packetEntry(2))

var pt2Packets = packets
pt2Packets.add sixPacket
pt2Packets.add twoPacket
pt2Packets.sort(myCompare)

echo "Parsing: ", getMonoTime() - start


start = getMonoTime()

var pt1, pt2 = 0
for i in countup(0, packets.high, 2):
  #echo "\n", x[0], " vs. ", x[1]
  if packets[i].compare(packets[i + 1]) != Greater:
    pt1 += i div 2 + 1

echo "Pt1: ", getMonoTime() - start

start = getMonoTime()

for i, x in pt2Packets:
  if x.compare(twoPacket) == Equal:
    pt2 = i + 1
  elif x.compare(sixPacket) == Equal:
    pt2 *= i + 1

echo "Pt2: ", getMonoTime() - start


echo pt1, " ", pt2
