import std/[macros, genasts]


macro expandProc(args, retT, env: typed): untyped =
  result = nnkProcTy.newTree(nnkFormalParams.newTree(), nnkPragma.newTree(ident"nimcall"))
  result[0].add retT.getTypeInst[1]
  result.params.add newIdentDefs(ident"params", args.getTypeInst[1])
  result.params.add newIdentDefs(ident"env", env.getTypeInst[1])

proc byref[T: not (ref or ptr)](name: var T): ptr T = name.addr
proc bycopy[T](name: sink T): T {.inline.} = T

type Closure[Env; Args; ReturnType] = object
  prc: expandProc(Args, ReturnType, Env)
  env: Env

type CaptureKind = enum
  ByRef
  ByCopy
  Move

macro stackClosure(parameters: typedesc[tuple or void], returnType: typedesc, args: varargs[typed], body: untyped): untyped =
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

  result = newProc(body = body)
  result.params[0] = returnType

  let envTuple = nnkTupleConstr.newTree()

  for i, x in params:
    let
      sym = x[0]
      (name, typ) =
        if sym.kind == nnkHiddenDeref:
          (ident $sym[0], nnkVarTy.newTree(sym.getType()))
        else:
          (ident $sym, sym.getType())
    result.params.add newIdentDefs(name, typ)
    envTuple.add nnkExprColonExpr.newTree(name, args[i])


  let
    innerPrc = result
    caller = newProc()

  caller.params[0] = returnType
  caller.params.add newIdentDefs(ident"params", parameters)
  caller.params.add newIdentDefs(ident"env", newCall("typeof", envTuple))
  caller[^1].add newCall(innerPrc)
  for param in parameters:
    caller[^1][0].add param[0]

  for i, param in params:
    caller[^1][0].add nnkBracketExpr.newTree(ident"env", newLit i)
    case param[1]
    of ByRef:
      caller[^1][0][^1] = nnkBracketExpr.newTree(caller[^1][0][^1])
    else:
      discard




  result = genast(clos = bindSym"Closure", envTuple, parameters, returnType, caller):
    clos[typeof(envTuple), parameters, returnType](env: envTuple, prc: caller)

macro invoke[Params: not void, Ret, Env](clos: Closure[Env, Params, Ret], params: Params): untyped =
  result = newCall(nnkDotExpr.newTree(clos, ident"prc"))
  for param in params:
    result.add param

  result.add nnkDotExpr.newTree(clos, ident"env")


macro invoke[Ret, Env](clos: Closure[Env, void, Ret]): untyped =
  result = newCall(nnkDotExpr.newTree(clos, ident"prc"))
  result.add nnkDotExpr.newTree(clos, ident"env")

proc main(i: var int) =
  var s = @[0, 1, 2]
  let clos = stackClosure(void, string, byref(i), ensureMove s):
    inc i
    $i

  echo clos.invoke()
  echo clos.invoke()
  echo clos.invoke()



var i = 0
main(i)
echo i

