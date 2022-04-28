import std/[macros, genasts, streams]

proc getVersionParam(node: NimNode): NimNode =
  let node =
    if node.kind != nnkTypeDef:
      node.getImpl
    elif node.kind == nnkBracket and node[0].eqIdent"typedesc":
      node[1].getTypeImpl
    else:
      node
  if node[1].kind == nnkGenericParams:
    # We assume it has a single generic param and that's what we want
    result = node[1][0]

macro migrateData*(data: typed): untyped =
  var
    typeImpl = data.getTypeInst
    descrim = typeImpl[^1]
  if typeImpl.kind == nnkBracketExpr:
    # Get base type from the type instantiation
    typeImpl = typeImpl[0].getImpl

  if descrim == nil:
    error("Cannot migrate data on type without version", data)

  let
    eImpl = descrim.getTypeImpl
    eCount = eImpl.len - 2

  result = nnkWhenStmt.newTree()
  for i, enmVal in eImpl[1..^1]:
    result.add nnkElifBranch.newTree(infix(descrim, "==", enmVal), data)
    for migrate in i..<eCount:
      result[^1][^1] = newCall("migrate", result[^1][^1])

proc replace(node, toReplace, with: NimNode) =
  for i, n in node:
    if n.kind == nnkIdent and n.eqIdent toReplace:
      node[i] = with
    else:
      n.replace(toReplace, with)

macro unpackToData(target: typedesc, descrim: enum, loadCall: untyped): untyped =
  ## Generates:
    #var res: `target`
    #case `descrim`
    #of firstEnumVal:
      #let data = loadCall(type[firstEnumVal])
      #re  s = migrateData(data, firstEnumVal)
    #of secondEnumVal:
      #let data = loadCall(type[secondEnumVal])
      #res = migrateData(data, secondEnumVal)
  let
    eImpl = descrim.getTypeImpl
    resSym = genSym(nskVar, "res")
    theTypeDef = target.getType[^1].getImpl
    versionParam = theTypeDef.getVersionParam()
  if versionParam == nil:
    error("Cannot unpack data of a type without version", target)
  var typeName = theTypeDef[0]

  if typeName.kind == nnkPragmaExpr:
    typeName = typeName[0]
  let typeInst = nnkBracketExpr.newTree(typeName)

  result = nnkCaseStmt.newTree(descrim)
  for i, enmVal in eImpl[1..^1]:
    let
      typ = typeInst.copyNimTree()
      loadCall = loadCall.copyNimTree()
    typ.add enmVal
    loadCall.replace(ident"Data", typ)
    result.add nnkOfBranch.newTree(enmVal, newCall(bindsym"migrateData", loadCall))
    result[^1][^1] = nnkAsgn.newTree(resSym, result[^1][^1])
  result = genAst(body = result, res = resSym, target):
    var res: target
    body
    res

type
  Version = enum
    ver00
    ver01
    ver02
  MyData*[Ver: static Version] = object
    when Ver == ver00:
      a: int
      b: string
    elif Ver in {ver01, ver02}:
      age: int
      name: string
      when Ver == ver02:
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

proc loadData*(T: typedesc, stream: Stream): T =
  mixin migrate
  var id: Version
  stream.read(id)
  unpackToData(T, id, stream.read(Data))

let
  personData = MyData[ver00](a: 32, b: "John")
  saveV2 = MyData[ver01](name: "liar", age: 600)

var ss = newStringStream()
ss.write(ver00)
ss.write(personData)
ss.write(ver01)
ss.write(saveV2)
ss.setPosition(0)

echo SaveData.loadData(ss)
echo SaveData.loadData(ss)
echo migrateData(personData)

