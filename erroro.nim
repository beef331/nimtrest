import std/[macros, enumerate, genasts]
import micros


type
  JmpBuf {.importc: "jmp_buf", header: "<setjmp.h>".} = object

proc setjmp(ctx: var JmpBuf): cint {.importc, header: "<setjmp.h>", cdecl.}
proc longjmp(ctx: JmpBuf, ret = 1) {.importc, header: "<setjmp.h>", cdecl.}

type
  ErroroError* = object of CatchableError
  ErrorBase*  {.packed.} = object of RootObj
    msg*: cstring
  Error*[T] {.packed.} = object of ErrorBase
    val*: T


var
  errorBuffer: pointer
  errorTypeInfo: pointer # Ugh type information
  jmpBuf: JmpBuf
  errorBufferSize: int
  errorHandler: proc() {.nimcall.}

proc alloca(size: uint): pointer {.importc, cdecl.}

converter unpack*[T](error: Error[T]): T =
  if error.msg.len != 0:
    if errorBuffer.isNil or errorHandler.isNil or errorBufferSize < sizeof(error):
      raise (ref ErroroError)(msg: $typeof(error) & ": " & error.msg)
    else:
      moveMem(errorBuffer, error.addr, sizeof(error))
      errorTypeInfo = error.getTypeInfo()
      wasMoved(error.addr[])
      errorHandler()

  error.val

proc findAllErrors(code: NimNode, errs: var seq[NimNode]) =
  for n in code:
    if n.kind in nnkCallKinds:
      let prc = n[0].getImpl()
      let objTyp = prc.params[0]
      if objTyp.kind == nnkBracketExpr and objTyp[0].sameType Error.getType():
        errs.add objTyp
      else:
        if objTyp.isa ObjectDef:
          let obj = objectDef(objTyp)
          for parTyp in obj.inheritObjs():
            discard # TODO: Actually handle this here

      for x in n[1..^1]:
        findAllErrors(x, errs)

    findAllErrors(n, errs)

macro triImpl(body: typed, excepts: typed, names: untyped, bodies: untyped): untyped =
  var size = 0
  let
    errorIf = ifStmt()
    errorBase = newStmtList()
  for i, excpt in excepts:
    size = max(excpt.getSize(), size)
    let dummy = genSym(nskVar, "ErrorTyp")
    errorBase.add:
      genast(excpt, dummy):
        var dummy: excpt
    errorIf.add:
      elifBranch:
        genast(dummy):
          dummy.getTypeInfo() == errorTypeInfo
      do:
        genast(excpt, name = names[i], body = bodies[i]):
          let name = cast[ptr excpt](errorBuffer)[]
          body

  result = genast(size, errorBase, errorIf = NimNode(errorIf), body):
    try:
      var arr {.noinit.}: array[size, byte]
      errorBuffer = arr.addr
      errorBufferSize = size
      errorHandler = proc() {.nimcall.} =
        longjmp(jmpBuf)
      if jmpBuf.setjmp() != 1:
        body
      else:
        errorBase
        errorIf
    finally:
      errorBuffer = nil
      errorBufferSize = 0
      errorHandler = nil
  echo result.repr
macro tri*(code: typed, branches: varargs[untyped]): untyped =
  var
    excepts = nnkBracket.newTree()
    names = nnkBracket.newTree()
    bodies = nnkBracket.newTree()

  for branch in branches:
    case branch[0].kind
    of nnkInfix:
      excepts.add branch[0][1]
      names.add branch[0][2]
      bodies.add branch[^1]
    else:
      excepts.add branch[0][0]
      names.add genSym(nskLet, "Error")
      bodies.add branch[^1]
  result = newCall(bindsym"triImpl", code)
  result.add excepts
  result.add names
  result.add bodies

proc `raise`*[Val; T: Error[Val]](error: var T, msg: string) =
  error.msg = msg

when isMainModule:
  import benchy

  proc doThingErr(s: string): Error[int] =
    if s.len == 1:
      result.raise("Incorrect string")
    else:
      result = Error[int](val: 10)

  proc doThingExcp(s: string): int =
    if s.len == 1:
      raise (ref ValueError)(msg: "Incorrect string")
    else:
      10

  timeit "Basic good erroro", 10:
    for x in 0..3000000:
      tri:
        let x = doThingErr("1")
      except Error[File] as e:
        echo "here"

  timeit "Basic good exception", 10:
    for x in 0..3000000:
      try:
        let x =  doThingExcp("")
      except ValueError as e:
        discard

  timeit "Basic bad erroro", 10:
    for x in 0..3000000:
      tri:
        let x = doThingErr("1")
      except Error[File] as e:
        discard


  timeit "Basic bad exception", 10:
    for x in 0..3000000:
      try:
        let x = doThingExcp("1")
      except ValueError as e:
        discard


