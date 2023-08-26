import std/[macros, macrocache, genasts, typetraits, sets]

const 
  rowTable = CacheTable"rowTable" # store concept names

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

macro row(body: typedesc[NamedTuple]): untyped =
  let 
    bodyRepr = body.repr
    rowName = genSym(nskType, "Row")
    varName = ident"row"

  if bodyRepr in rowTable and not body.isGenericTuple():
    result = rowTable[bodyRepr]
  else:
    rowTable[body.repr] = rowName
    let typBody = newStmtList()

    for idef in body:
      for name in idef[0..^3]:
        typBody.add infix(nnkDotExpr.newTree(varName, name), "is", idef[^2])

    result = genast(rowName, varName, body = typBody):
      type rowName = concept varname
        body
      rowName

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

type 
  MyRow[T] = row tuple[x, y: T] # Due to macro evaluation order this alias is required (it injects a `T`)


proc tryGeneric[T](r: MyRow[T], a: T) =
  echo r.x, " ", r.y, " ", a

doThing (x: 100, y: 200)


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

