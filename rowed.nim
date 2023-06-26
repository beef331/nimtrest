import std/[macros, macrocache, algorithm, strutils]

template isRowed*{.pragma.}

proc isRowType(t: typedesc): bool = t.hasCustomPragma(isRowed)

type Rowed* = concept type R
  isRowType(R)

const rowTable = CacheTable"RowTable"

proc sortedDef(tup: NimNode): (NimNode, int) =
  ## Returns the sorted def and the count of generic parameters
  var fieldNames: seq[NimNode]
  for def in tup:
    for i in 0 .. def.len - 3:
      fieldNames.add nnkTupleConstr.newTree(def[i], def[^2])

  result[0] = newStmtList(fieldNames.sortedByIt($it[0]))


macro row*(p: typed): untyped =
  var (def, generics) = default (NimNode, int)
  let newProc = p.copyNimTree()
  for i, param in p.params.pairs:
    if i > 0 and param[^2].kind == nnkTupleTy:
      (def, generics) = sortedDef(param[^2])
      break # we got our first row. replace tuple with emitted concept

  let 
    name = $p[0]
  newProc[0] = genSym(nskProc, name)

  if name notin rowTable:
    rowTable[name] = newStmtList()
  rowTable[name].add nnkTupleConstr.newTree(def, newProc)
  result = newProc
  echo result.repr


proc doThing(a: tuple[x, y: int]) {.row.} = echo "dual"
proc doThing(a: tuple[x, y, z: int]) {.row.} = echo "tri"


type MyType {.isRowed.} = object
  x, y: int

static: assert MyType is Rowed
