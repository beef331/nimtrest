import std/[macros, genasts, strformat]
type
  AllowedVersion = (range or enum or bool)
  VersionedData* = concept v, type V
    V.Version is static
    V.Version is AllowedVersion

proc migrateData*[T: VersionedData](verDat: T): auto =
  mixin migrate
  when verDat.Version != typeof(verDat.Version).high:
    migrateData(migrate(verDat))
  else:
    verDat

iterator eachVal(n: NimNode): NimNode =
 case n.getType.typeKind
 of ntyEnum:
   for i, n in n.getTypeImpl:
     if i > 0:
       yield n
 of ntyBool:
   yield newLit false
   yield newLit true
 of ntyRange:
   let impl = n.getTypeImpl
   for x in impl[^1][1].intVal .. impl[^1][2].intVal:
     yield newCall(impl[^1][1].getType, newLit x)
 else:
   error(fmt"Expected 'range' or 'bool' or 'enum' for '{n.repr}', but got '{n.getType}'", n)

macro forEachVersion*(theRange: AllowedVersion, theStatement: untyped): untyped =
  result = nnkCaseStmt.newTree(theRange)
  for entry in theRange.eachVal:
    let newTree = newStmtList()
    newTree.add:
      genast(entry, theStatement):
        const it {.inject.} = entry
        theStatement
    result.add nnkOfBranch.newTree(entry, newTree)


import std/streams
type
  Version = enum
    ver00
    ver01
    ver02
  MyData*[Version: static Version] = object
    when Version == ver00:
      a: int
      b: string
    elif Version in {ver01, ver02}:
      age: int
      name: string
      when Version == ver02:
        talent: string

  SaveData = MyData[Version.high]

proc migrate(data: MyData[ver00]): MyData[ver01] =
  result.age = data.a
  result.name = data.b

proc migrate(data: MyData[ver01]): MyData[ver02] =
  result.age = data.age
  result.name = data.name
  result.talent = "None"

proc read(stream: Stream, T: typedesc): T =
  stream.read(result)

proc saveData*(myData: SaveData, stream: Stream) =
  stream.write(ord(Version.high))
  stream.write(myData) # Streams write pointers for string/seqs this is just example

proc loadData*(stream: Stream): SaveData =
  var id: Version
  stream.read(id)
  id.forEachVersion:
    stream.read(MyData[it]).migrateData

let
  personData = MyData[ver00](a: 32, b: "John")
  saveV2 = MyData[ver01](name: "liar", age: 600)
  saveV3 = MyData[ver02](name: "jimmy", age: 32, talent: "jumping")

var ss = newStringStream()
ss.write(ver00)
ss.write(personData)
ss.write(ver01)
ss.write(saveV2)
ss.setPosition(0)

echo loadData(ss)
echo loadData(ss)
echo migrateData(personData)
echo migrateData(saveV2)
echo migrateData(saveV3)

true.forEachVersion:
  discard

range[0u8..3u8](3).forEachVersion:
  discard
