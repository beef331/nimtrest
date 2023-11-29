import std/[macros, genasts]

proc validateParams(n: NimNode) =
  var foundNamed = false
  for i, x in n:
    if x.kind == nnkExprEqExpr:
      foundNamed = true
    if foundNamed and x.kind != nnkExprEqExpr:
      error("Provided positional parameter after named parameter.", x)


macro tumeric*(theProc: proc, args: varargs[untyped]): untyped =
  if theProc.kind != nnkSym:
    error("Expected a non overloaded procedure", theProc)

  args.validateParams()

  let prcImpl = theProc.getTypeInst()
  var 
    theCall = newCall(theProc)
    params = nnkFormalParams.newTree(prcImpl[0][0])
    positionalIndex = 0
  
  for def in prcImpl[0][1..^1]:
    var found = false
    for i, arg in args:
      case arg.kind
      of nnkExprEqExpr:
        if def[0].eqIdent arg[0]:
          found = true
          theCall.add arg
          break
      else:
        discard
    if not found:
      if positionalIndex < args.len and args[positionalIndex].kind != nnkExprEqExpr:
        theCall.add args[positionalIndex]
        found = true
        inc positionalIndex
      else:
        let copiedName = ident($(def[0]))
        params.add newIdentDefs(copiedName, def[^2])
        theCall.add copiedName

  result = genast():
    proc() = discard

  result.params = params
  result.body = theCall

proc addThem(a, b: int): int = a + b
proc doThing[T](a, b: T): T = a + b

let
  a = addThem.tumeric(10) 
  b = addThem.tumeric(b = 10)
  c = addThem.tumeric(b = 30, a = 20)
  d = doThing[int].tumeric(b = 300)
  e = doThing[float].tumeric(a = 300, b = 400)
assert a(20) == 30
assert b(40) == 50
assert c() == 50
assert d(300) == 600
assert e() == 700

