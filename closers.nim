import std/[macros, genasts]

proc byref*[T: not (ref or ptr)](name: var T): ptr T = name.addr
proc bycopy*[T](name: sink T): T {.inline.} = T

type
  Closure[Env; Prc: proc] = object
    prc: Prc
    env: Env

type CaptureKind = enum
  ByRef
  ByCopy
  Move

proc makeVar(n: NimNode): NimNode =
  if n.getType().typekind != ntyVar:
    nnkVarTy.newTree(n.getType())
  else:
    n.getType()

proc stackClosureImpl(theProc, args: NimNode): NimNode =
  var params: seq[(NimNode, CaptureKind)]

  for x in args[0..^1]:
    if x.kind notin nnkCallKinds + {nnkSym}:
      error("Expected a 'move' call, 'byref' call, or a variable symbol.", x)
    case x.kind
    of nnkSym:
      params.add (x, ByCopy)
    else:
      let kind =
        if x[0].eqIdent"byref":
          ByRef
        elif x[0].eqIdent"bycopy":
          ByCopy
        elif x[0].eqIdent"move" or x[0].eqIdent"ensureMove":
          Move
        else:
          error("Expected 'move', 'byref', 'bycopy', `ensureMove`, or a variable symbol.", x)
          Move


      if params.len > 2: # Redundant but bleh
        error("Expected single parameter.", x)

      params.add (x[1], kind)

  result = theProc.copyNimTree()
  let returnType = theProc.params[0]

  let envTuple = nnkTupleConstr.newTree()

  for i, x in params:
    let
      sym = x[0]
      (name, typ) =
        if sym.kind == nnkHiddenDeref:
          (ident $sym[0], makeVar sym)
        else:
          (ident $sym, makeVar sym)
    result.params.add newIdentDefs(name, typ)
    envTuple.add nnkExprColonExpr.newTree(name, args[i])


  let
    innerPrc = result
    caller = newProc()

  caller.params = theProc.params.copyNimTree
  caller.params.add newIdentDefs(ident"env", nnkVarTy.newTree newCall("typeof", envTuple))
  caller[^1].add newCall(innerPrc)
  caller[4] = nnkPragma.newTree(ident"nimcall")

  for i, params in theProc.params:
    if i > 0:
      for def in params[0..^3]:
        caller[^1][^1].add def


  for i, param in params:
    caller[^1][0].add nnkBracketExpr.newTree(ident"env", newLit i)
    if param[1] == ByRef:
      caller[^1][0][^1] = nnkBracketExpr.newTree(caller[^1][0][^1])


  result = genast(clos = bindSym"Closure", caller, envTuple):
    clos[typeof(envTuple), typeof(caller)](env: envTuple, prc: caller)

macro stackClosure*(args: varargs[typed], theProc: untyped): untyped =
  stackClosureImpl(theProc, args)


macro stackClosureTyp*(theProc: untyped): untyped =
  result = theProc
  result[^1].add ident"nimcall"
  result[0].add newIdentDefs(ident"_", nnkVarTy.newTree(ident"auto"))

macro invoke*[Env, Prc](clos: var Closure[Env, Prc], params: varargs[typed]): untyped =
  result = newCall(nnkDotExpr.newTree(clos, ident"prc"))
  params.copyChildrenTo(result)

  result.add nnkDotExpr.newTree(clos, ident"env")

proc doThing(clos: var Closure[auto, proc(_, _: int) {.stackClosureTyp.}])=
  clos.invoke(10, 20)

proc main(i: var int): auto =
  var s = @[0, 1, 2]
  result = proc(x: int, y: int) {.stackClosure(byref(i), ensureMove(s)).} =
    s.add i
    inc i
    echo x, " ", y, " ", i, " ", s

var i = 0
var clos = main(i)
clos.doThing()
clos.doThing()
clos.doThing()
echo i

