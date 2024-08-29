import std/[macros, macrocache, genasts, typetraits, sets]

const 
  rowTable = CacheTable"rowTable" # store concept names
  rowedTable = CacheTable"rowedTable" # store concept names

type ImplementedHooks = enum # Used to keep track of which hooks are implemented, and their order
  Destructor


type RowedTable = distinct NimNode

proc vtable(rowedTable: RowedTable): NimNode = NimNode(rowedTable)[1]
proc theTuple(rowedTable: RowedTable): NimNode = NimNode(rowedTable)[0]
proc hasTypes(rowedTable: RowedTable): bool = NimNode(rowedTable).len != 2

iterator types(rowedTable: RowedTable): NimNode =
  for i in 2..<NimNode(rowedTable).len:
    yield NimNode(rowedTable)[i]

iterator fields(rowedTable: RowedTable): (NimNode, NimNode) =
  # Yields fields and their types
  for defs in rowedTable.theTuple:
    for name in defs[0..^3]:
      yield (name, defs[^2])

proc fieldCount(rowedTable: RowedTable): int =
  result = ImplementedHooks.high.ord + 1
  for _ in rowedTable.fields:
      inc result

proc vtableProcOffsetTyp(rowedTable: RowedTable, field: NimNode): (int, NimNode) =
  result[0] = ImplementedHooks.high.ord + 1
  for name, typ in rowedTable.fields:
    if name.eqIdent field:
      return (result[0], typ)
    inc result[0]

proc maybeAddRowedIfNotFound(rowed, typ: NimNode): bool =
  # Attempts to add typ to the rowed.
  # Returns true if added
  let rowRepr = rowed.repr
  if rowRepr notin rowedTable:
    error("Somehow attempted to add to a non generated rowed", rowed)
  else:
    for x in RowedTable(rowedTable[rowRepr]).types:
      if x.sameType(typ):
        return false
    rowedTable[rowRepr].add typ
  true
 
proc typeId(rowed, typ: NimNode): int =
  let rowRepr = rowed.repr  
  if rowRepr notin rowedTable:
    error("Somehow attempted to add to a non generated rowed", rowed)
  else:
    for x in RowedTable(rowedTable[rowRepr]).types:
      if x.sameType(typ):
        return 
      inc result


type NamedTuple = concept type T
  isNamedTuple(T)

proc hasIdent(n: NimNode): bool =
  if n.kind == nnkIdent:
    return
  for x in n:
    result = result or hasIdent(x)
    if result:
      break

proc isGenericTuple(n: NimNode): bool =
  result = false
  for idef in n:
    if idef[^2].hasIdent:
      return true

macro skipAliases(t: typed): untyped =
  result = t
  while result.kind == nnkSym:
    result = result.getImpl[^1]

macro rowImpl(body: typedesc[NamedTuple]): untyped =
  let 
    body =
      if body.kind == nnkSym:
        body.getImpl[^1]
      else:
        body
    bodyRepr = body.repr
    rowName = genSym(nskType, "Row")
    varName = ident"rowVal"

  if bodyRepr in rowTable and not body.isGenericTuple():
    result = rowTable[bodyRepr]
  else:
    rowTable[bodyRepr] = rowName
    let typBody = newStmtList()
    var refinementType = body.copyNimtree

    for i in countDown(refinementType.len - 1, 0): # Remove the last field
      if refinementType[i].len > 3:
        refinementType[i].del(refinementType[i].len - 3)
      else:
        refinementType.del(i)

      break

    if refinementType.len > 0:
      typBody.add varName.infix("is", newCall("row", refinementType))

    for idef in body:
      for name in idef[0..^3]:
        typBody.add infix(nnkDotExpr.newTree(varName, name), "is", idef[^2])

    result = genast(rowName, varName, body = typBody):
      type rowName = concept varname
        body
      rowName
  echo result.repr

template row*(typ: typedesc[NamedTuple]): untyped =
  rowImpl skipAliases(typ)

type Rowed[T: static string] = object
  id: int
  data: pointer

macro rowedImpl(body: typedesc[NamedTuple]): untyped =
  let
    bodyRepr = body.repr
    rowName = genSym(nskType, "Row")
  if bodyRepr in rowedTable and not body.isGenericTuple():
    result = nnkBracketExpr.newTree(bindSym"Rowed", newLit bodyRepr)
  else:
    let vtable = genSym(nskVar, bodyRepr & "vTable")
    rowedTable[bodyRepr] = newStmtList(body, vtable)
    result = nnkBracketExpr.newTree(bindSym"Rowed", newLit bodyRepr)

template rowed*(typ: typedesc[NamedTuple]): untyped =
  rowedImpl(skipAliases(typ))

macro rowedImpl(theRow: typedesc, val: typed): untyped =
  let 
    rowRepr = theRow.repr
    typId = theRow.typeId(val)
    rowTyp = nnkBracketExpr.newTree(bindSym"Rowed", newLit rowRepr)
  result = newStmtList:
    genast(val, rowTyp, theId = newLit typId):
      let res = rowTyp(id: theId, data: create(typeof(val)))
      copyMem(res.data, val.addr, sizeof(val))
      res
  var insertOffset = 0
  let
    rowData = RowedTable rowedTable[rowRepr]
    fieldCount = rowData.fieldCount
  if not rowData.hasTypes:
    result.insert 0:
      genast(table = rowData.vtable):
        var table {.global.} = newSeq[pointer]() # we use pointer cause we have hooks and fields of unknown types here
    inc insertOffset

    result.insert insertOffset:
      genast(rowTyp, fieldCount, vtable = rowData.vtable):
        proc `=destroy`(r: rowTyp) =
          if r.data != nil:
            cast[proc(r: rowTyp) {.nimcall.}](vtable[r.id * fieldCount])(r)
            dealloc(r.data)
    inc insertOffset

  if maybeAddRowedIfNotFound(theRow, val):
    result.insert insertOffset:
      genast(rowTyp, typ = val.getTypeInst, vTable = rowdata.vtable, fCount = newLit fieldCount):
        vTable.add cast[pointer](proc(r: rowTyp) {.nimcall.} =
          `=destroy`(cast[ptr typ](r.data)[])
        )
    inc insertOffset

    for field, fieldTyp in rowData.fields:
      result.insert insertOffset:
        genast(rowTyp, field, fieldTyp, typ = val.getTypeInst, vtable = rowData.vtable):
          vtable.add cast[pointer](proc(r: rowTyp): var fieldTyp =
            cast[ptr typ](r.data).field
          )

{.experimental: "dotOperators".}

macro `.`*[T](rowed: Rowed[T], name: untyped): untyped =
  let 
    rowData = RowedTable rowedTable[$T]
    (offset, typ) = rowData.vtableProcOffsetTyp(name)

  result = genast(rowed, offset, typ, vtable = rowData.vtable, procCount = rowData.fieldCount):
    let val = cast[proc(r: typeof(rowed)): var typ {.nimcall.}](vtable[offset + (rowed.id * procCount)])(rowed)
    val

macro `.`*[T](rowed: var Rowed[T], name: untyped): untyped =
  let 
    rowData = RowedTable rowedTable[$T]
    (offset, typ) = rowData.vtableProcOffsetTyp(name)

  result = genast(rowed, offset, typ, vtable = rowData.vtable, procCount = rowData.fieldCount):
    cast[proc(r: typeof(rowed)): var typ {.nimcall.}](vtable[offset + (rowed.id * procCount)])(rowed)


macro `.=`*[T](rowed: var Rowed[T], name: untyped, val: typed): untyped =
  result = genast(rowed, name, val):
    (rowed.name) = val


template rowed*(typ: typedesc[NamedTuple], val: typed): untyped =
  var temp = default(rowed(skipAliases(typ)))
  let otherTemp =
    when compiles(move(val)):
      move(val)
    else:
      val
  temp = rowedImpl(skipAliases(typ), otherTemp)
  temp

macro join*(toJoin: varargs[typed], body: untyped): untyped =
  var toAddFields: seq[NimNode]

  for joiner in toJoin:
    case joiner.kind
    of nnkSym:
      var impl = joiner.getImpl()
      if impl == nil:
        error("No AST found, did you declare this type in the same block?", joiner)

      if impl[^1].kind == nnkRefTy:
        impl = impl[^1]

      case impl[^1].kind
      of nnkObjectTy:
        for field in impl[^1][^1]:
          case field.kind
          of nnkRecCase:
            error("This macro does not handle variant objects.", joiner)
          of nnkIdentDefs:
            toAddFields.add field
          else:
            error("Unexpected AST for joiner macro: ", field)

      of nnkTupleTy:
        for field in impl[^1]:
          toAddFields.add field

      else:
        error("Expected object or tuple definition.", joiner)
    of nnkTupleTy:
      echo joiner.repr
    else:
      error("Expected tuple or non generic object", joiner)

  result = body
  let obj = 
    if result[^1].kind == nnkRefTy:
      result[^1][^1]
    else:
      result[^1]
  if obj[^1].kind != nnkRecList:
    obj[^1] = nnkRecList.newTree(toAddFields)
  else:
    for i, x in toAddFields:
      obj[^1].insert i, x

proc doThing(r: row tuple[x, y: int]) = echo r
proc doThing(r: row tuple[x: int]) = echo r
proc otherThing(r: row tuple[x: int]) = echo "Hmm"

doThing (x: 100, y: 200)
doThing (x: 100, )


#[
type 
  MyRow[T] = row tuple[x, y: T] # Due to macro evaluation order this alias is required (it injects a `T`)

proc tryGeneric[T](r: MyRow[T], a: T) =
  echo r.x, " ", r.y, " ", a

doThing (x: 100, y: 200)
doThing (x: 100, )


type 
  MyTuple = tuple[x: int]
  MyObj = ref object
    y: int

type
  MyType {.join(MyTuple, MyObj).} = object
    z: int

doThing MyType(x: 3, y: 40, z: 100)

tryGeneric MyType(x: 3, y: 40, z: 100), 300

tryGeneric (x: "hmm", y: "huh"), "wazahhh"

var a = rowed(MyTuple, MyType(x: 300))
assert a.x == 300
a.x = 400
assert a.x == 400
a = rowed(MyTuple, (x: 100))
assert a.x == 100
a.x = 300
assert a.x == 300
otherThing a
]#
