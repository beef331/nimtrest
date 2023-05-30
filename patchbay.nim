import std/[macros, genasts, sugar]
import micros

proc dynDispatchImpl(procs, els, myProc: NimNode): NimNode =
  let
    myProc = routineNode myProc
    argSeq = collect:
      for def in myProc.params:
        for name in def.names:
          ident $name

    theIf = ifStmt()

  for routine in routineSym(procs).routines:
    if sameType(routine.returnType, myProc.returnType):
      var
        cond: NimNode
        compileCond: NimNode
        call = newCall(NimNode(routine)[0])
        paramCount = 0
      for def in routine.params:
        let typ = newCall("typeof", def.typ)
        for name in def.names:
          let thisCond = block:
            let theCond = newCall("convertTo", argSeq[paramCount], typ)
            if compileCond.kind == nnkNilLit:
              compileCond = newCall("compiles", theCond)
            else:
              compileCond = infix(compileCond, "and", newCall("compiles", theCond))

          if cond.kind == nnkNilLit:
            cond = thisCond
          else:
            cond = infix(cond, "and", thisCond)
          call.add newCall("convertTo", argSeq[paramCount], typ)
          inc paramCount
      if paramCount == argSeq.len:
        theIf.add elifBranch(cond, newCall(bindsym"callCheck", compileCond, call))

  theIf.add elseBranch(els)
  result = NimNode(theIf)

macro dynDispatch*(name, theProc: typed) =
  echo theProc.isa RoutineNode
  result = copyNimTree(theProc)
  echo result[^1].treeRepr
  result[^1] = dynDispatchImpl(name, theProc[^1], theProc)
  result[^1] = newCall(bindSym"expandMacros", result[^1])
  echo result.repr

type Num[T: static int] = distinct int

proc convertTo[T](i: int, _: typedesc[Num[T]]): (bool, Num[T]) =
  if i == T:
    (true, Num[T](i))
  else:
    (false, Num[T](0))

proc factorial(num: Num[0]): int = 1
proc factorial(val: auto): int {.dynDispatch: factorial.} =
  result = val * factorial(val - 1)

echo factorial 7

