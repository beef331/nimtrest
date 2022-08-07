import std/[macros, macrocache, genasts, enumerate]

type Interface[Constraint: static int] = object
  typeId: int
  vtable: ptr seq[pointer]
  data: RootRef

const
  conceptVTable = CacheSeq"ConceptVTable"

proc replace(n, toReplace, with: NimNode) =
  for i, x in n:
    case x.kind
    of nnkSym, nnkIdent:
      if x.eqIdent toReplace:
        n[i] = with
    else:
      x.replace(toReplace, with)

proc desym(n: NimNode) =
  for i, x in n:
    if x.kind == nnkSym:
      n[i] = ident $x
    else:
      desym(x)

proc getProcs(cncpt: NimNode): NimNode

proc recurseInfix(n: NimNode): NimNode =
  ## Goes of `A and B` concepts
  if not n[0].eqIdent"and":
    error("Goface only presently works with 'and' union concepts", n)
  result = newStmtList()
  for x in 1..<n.len:
    if n[x].kind == nnkInfix:
      recurseInfix(n[x]).copyChildrenTo result
    else:
      getProcs(n[x]).copyChildrenTo result

proc getProcs(cncpt: NimNode): NimNode =
  let impl =
    if cncpt.len > 0:
      cncpt[0].getImpl.copyNimTree
    else:
      cncpt.getImpl
  for i, x in impl[1]:
    impl.replace(x, cncpt[i + 1])
  case impl[^1].kind
  of nnkInfix:
    result = impl[^1].recurseInfix()
  of nnkBracketExpr, nnkSym:
    result = impl[^1].getProcs()
  else:
    result = impl[^1][^1]

proc getProcs(id: int): NimNode = conceptVtable[id][0].getProcs()
proc getVtableName(id: int): NimNode = conceptVtable[id][0][1]


proc addToVtable(typ, cncpt: NimNode): bool =
  ## Adds to vtable and returns the true if it does add
  result = true
  for x in conceptVTable:
    if x[0] == cncpt:
      for y in x:
        if typ == y:
          return
      x.add typ
      return true

proc getVtableEntry(typ, cncpt: NimNode): (bool, NimNode) =
  ## Returns whether we need to emit the vtable and the name of it
  for x in conceptVTable:
    if x[0] == cncpt:
      return (false, x)
  let name =  genSym(nskVar, cncpt.repr & "vtable")
  result = (true, newStmtList(cncpt, name))
  conceptVtable.add result[1]

proc conceptTypeId(typ, cncpt: NimNode): (int, int) =
  ## Yields the type id and cncpt id for a given pair
  result = (-1, -1)
  for i, x in enumerate conceptVtable:
    if x[0] == cncpt:
      for j, y in x:
        if y == typ:
          return (i, j - 2)

proc instantiateProcType(t, prcDef: NimNode): NimNode =
  result = nnkProcTy.newTree()
  result.add prcDef.params.copyNimTree
  result.add nnkPragma.newTree(ident"nimcall")
  for i, typ in t[1..^1]:
    result.replace prcDef[2][i], typ

proc compatible(a, b: NimNode): bool =
  if a == b:
    result = true
  elif a[0] == b[0] and a.len == b.len:
    for i in 0..<a.len:
      if a[i].typeKind != ntyGenericParam and not a[i].sameType b[i]:
        return false
      else:
        result = true


proc makeDefaultCall(pTyp, name: NimNode): NimNode =
  result = newCall(name)
  for defs in pTyp[0][1..^1]:
    for _ in 1 .. defs.len - 2:
      if defs[^2][0].eqIdent bindSym("openarray"):
        result.add nnkBracket.newNimNode()
      else:
        result.add newCall("default", newCall("typeof", defs[^2]))

macro procSymFromCall(p: typed): untyped = p[0]

iterator generatedArgs(typ, procdef: NimNode): NimNode =
  ## What the generic args are in our to made call
  let copiedProc = procdef.copyNimTree
  copiedProc.replace ident"Self", typ
  for i, defs in copiedProc.params:
    if i > 0:
      for j, param in defs[0..^3]:
        if defs[^2][0].sameType getType(openarray):
          yield nnkBracket.newTree
        else:
          yield newCall("default", newCall("typeof", defs[^2]))

macro toInterface(val: RootRef, t: typedesc): untyped =
  result = newStmtList()
  let
    (emitVtable, vtable) = val.getTypeInst.getVtableEntry(t)
    vtableName = vtable[1]
    needToAddProcs = val.getTypeInst.addToVtable(t)
    (conceptId, typeId) = val.getTypeInst.conceptTypeId(t)

  if needToAddProcs:
    let procs = t.getProcs()

    for pid, prc in enumerate procs:
      let myCall = newCall(ident $prc[0])
      for arg in generatedArgs(val.getTypeInst, prc):
        myCall.add arg

      result.add:
        genast(vtableName, myCall, typ = val.getTypeInst, pid = newLit(pid), typeId = newLit(typeId), procCount = newLit(procs.len)):
          if vtableName.len <= (typeId * procCount + pid):
            vtableName.add procSymFromCall(myCall)



  result.add:
    genast(tid = newLit(typeId), cid = newLit(conceptId), val, vtableName):
      Interface[cid](typeId: tid, vtable: vtableName.addr, data: RootRef(val))

  if emitVtable: # Emit the vtable in global memory if we do need to emit one
    result = genAst(res = result, vtableName):
      block:
        var vtableName {.global.}: seq[pointer]
        res

template emitConverter*(theType, theConcept: typedesc): untyped =
  converter asInterface(theVal: theType): auto = theVal.toInterface(theConcept)

{.experimental: "dotOperators".}
macro `.()`*(inFace: Interface, procName: untyped, args: varargs[typed]): untyped =
  let
    id = inFace.getTypeInst[1].intVal.int
    procs = id.getProcs()
  for ind, prc in enumerate procs:
    if prc[0].eqIdent procName: # Should check that the parameters can match, but meh safety schmaftey for now
      let copied = nnkProcTy.newTree()
      copied.add prc.params.copyNimTree
      copied.add nnkPragma.newTree(ident"nimcall")
      copied.replace(ident"Self", ident"RootRef")
      result = genast(inFace, procTyp = copied, ind = newLit(ind), procCount = newLit(procs.len)):
        cast[procTyp](inFace.vtable[inFace.typeId * procCount + ind])(inFace.data)
      for arg in args:
        result.add arg
      result = newStmtList(result)
      break

import std/options


type
  OptInt = Option[int]
  Reader*[T] = concept
    proc read(self: Self, buffer: openArray[T]): Optint # New concepts have bugs with generic types apparently
  Writer*[T] = concept
    proc write(self: Self, buffer: openArray[T], num: ref int)
  ReaderWriter[T] = Reader[T] and Writer[T]

type MyStream[T] = ref object of RootRef

proc read[T](self: MyStream[T], buffer: openArray[T]): Option[int] = echo typeof(self), " Reader"
proc write[T](self: MyStream[T], buffer: openArray[T], num: ref int) = echo typeof(self), " Writer"


converter asInterface(theVal: MyStream[char]): auto = theVal.toInterface(ReaderWriter[char])

type MyOtherStream[T] = ref object of RootRef

proc read[T](self: MyOtherStream[T], buffer: openArray[T]): Option[int] = echo typeof(self), " Reader"
proc write[T](self: MyOtherStream[T], buffer: openArray[T], num: ref int) = echo typeof(self), " Writer"

emitConverter MyOtherStream[char], ReaderWriter[char]

for x in [MyStream[char]().toInterface(ReaderWriter[char]), MyOtherStream[char](), MyStream[char]()]:
  discard x.read("")
  x.write("hello", nil)

