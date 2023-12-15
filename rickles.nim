import std/[macros, macrocache, genasts]

const procTable = CacheSeq"MockedTable" # (Name, [(Types, PointerProc),...], originalProc)

macro addToTable(name, orig: typed) =
  procTable.add newStmtList(name, nnkBracketExpr.newNimNode(), orig)

macro isInstantiatable(name: typedesc): untyped =
  let name = name.getType()[^1]
  for entry in procTable:
    if name == entry[0]:
      return newLit true

  newLit false

type Instantiatable = concept type I
  I.isInstantiatable()

proc getTableData(typ: NimNode): NimNode =
  let typ = typ.getTypeInst()
  for x in procTable:
    if typ == x[0]:
      return x
  assert false

proc getSym(types, table: NimNode): NimNode =
  for entry in table[1]:
    if types.len + 1 == entry.len:
      var matches = true
      for i in 0..< types.len:
        if not types[i].sameType entry[i]:
          matches = false
          break
      if matches:
        return entry[^1]


macro `[]`*(name: Instantiatable, args: varargs[typed]): untyped =
  let 
    tableData = getTableData(name)
    sym = getSym(args, tableData)

  if sym.kind == nnkNilLit:
    let 
      name = genSym(nskVar, "dummyProc")
      newEntry = newStmtList()
    args.copyChildrenTo(newEntry)
    newEntry.add name

    tableData[1].add newEntry
  
    let inst = nnkBracketExpr.newTree(tableData[2][0])
    args.copyChildrenTo(inst)

    result = genast(name, inst):
      var name {.global.} = inst
      name
  else:
    result = sym
  echo result.repr

proc desym(node: NimNode) =
  for i, n in node:
    case n.kind
    of nnkSym:
      node[i] = ident($n)
    else:
      desym(n)


macro `[]=`*(name: Instantiatable, args: varargs[typed], newProc: untyped): untyped =
  let 
    tableData = getTableData(name)
    sym = getSym(args, tableData)

  if sym.kind == nnkNilLit:
    let 
      name = genSym(nskVar, "dummyProc")
      newEntry = newStmtList()
    args.copyChildrenTo(newEntry)
    newEntry.add name

    tableData[1].add newEntry
    result = genast(name, newProc):
      var name {.global.} = newProc
  else:
    result = nnkAsgn.newTree(sym, newProc) 

macro mockit*(p: typed): untyped =
  let name =
    if p[0].kind == nnkPostfix:
      postfix(ident($p[0][0]), "*")
    else:
      ident $p[0]

  if p[2].kind != nnkGenericParams:
    result = newVarStmt(name, p[0])
  else:
    result = genast(name, p):
      type SomeType = distinct pointer
      var name = SomeType(nil)
      addToTable SomeType, p 


proc doThing*() {.mockit.} = echo "World"

doThing()
doThing = proc() = echo "Hello"

proc doStuff[T]() {.mockit.} = echo "Unimplemented: ", typeof(T)

static:
  echo doStuff.typeof().isInstantiatable
  assert doStuff is Instantiatable


doStuff[int]()
doStuff[int] = proc() = debugecho "What?!"
doStuff[int]()
doStuff[float]()
doStuff[float] = proc() = echo "Floaty"
doStuff[int]()
doStuff[float]()
