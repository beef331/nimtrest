import std/[macros, genasts, typetraits, enumerate]
import micros

proc genBracketBorrow(isMut: bool, borrowFor, statement: NimNode): NimNode =
  let dBase = bindSym"distinctBase"
  result =
      if statement.kind == nnkAsgn:
        genast():
          proc `[]=`() {.inline.} = discard
      else:
        genast():
          proc `[]`() {.inline.} = discard
  let
    call = newCall:
      if statement.kind == nnkAsgn:
        "[]="
      else:
        "[]"
    bracketExpr =
      if statement.kind == nnkAsgn: # Converts `nnkAsgn(nkBracket(a, b), args..)` into `stmtList(a, b, args..)`
        let newStmt = newStmtList()
        statement[0].copyChildrenTo(newStmt)
        newStmt.add statement[1..<statement.len]
        newStmt

      else:
        statement

  for i, x in bracketExpr:
    let
      argName = x.skipAddrs
      isSelf = argName.eqIdent"self" or argName.eqIdent"mSelf"
    var
      typ =
        if isSelf:
          if argName.symKind == nskVar:
            nnkVarTy.newTree(borrowFor)
          else:
            borrowFor
        else:
          x.getType()

    if argName.eqIdent"mSelf" and typ.kind != nnkVarTy:
      typ = nnkVarTy.newTree(typ)

    let
      name = ident("arg" & $i)
      theIdent = IdentDef(newIdentDefs(name, typ))
    call.add:
      if isSelf:
        newCall(dBase, name)
      else:
        name
    RoutineNode(result).addParam theIdent
    if statement.kind != nnkAsgn:
      var retT = getType(statement)
      if isMut:
        retT = nnkVarTy.newTree(retT)
      RoutineNode(result).returnType = retT



  result[^1] = call



proc genCallBorrow(borrowFor: NimNode, isMut: bool, statement: NimNode): NimNode =
  let
    dBase = bindSym"distinctBase"
    call = nnkCall.newTree(statement[0])
  result =
      case statement[0].symKind
      of nskIterator:
        genast(name = ident $statement[0]):
          iterator name() = discard
      else:
        genast(name = ident $statement[0]):
          proc name() {.inline.} = discard


  for symCount, routine in enumerate routineSym(statement[0]).routines:
    if symCount > 0:
      error("Somehow got an overloaded symbol", routine.NimNode)
    routineNode(result).returnType = routine.returnType
    for i, params in enumerate routine.params:
      let params = IdentDef(NimNode(params).copyNimTree)

      for j, name in enumerate params.names:
        let
          argName = statement[j + 1 + i].skipAddrs
          isSelf = argName.eqIdent"self" or argName.eqIdent"mSelf"
        proc paramType: NimNode =
          if isSelf:
            borrowFor
          else:
            if statement[j + 1 + i].kind == nnkConv: # Conversions should be skipped for their type?
              statement[j + 1 + i][0]
            else:
              statement[j + 1 + i].getType()

        var typ =
          case params.typ.kind
          of nnkVarTy:
            var pType = paramType()
            if pType.kind == nnkBracketExpr and pType[0].eqIdent"var":
              pType = pType[^1]
            nnkVarTy.newTree(pType)
          of nnkCommand:
            if params.typ[0].eqIdent"sink":
              nnkCommand.newTree(ident"sink", paramType())
            else:
              paramType()
          else:
            paramType()
        if argName.eqIdent"mSelf" and typ.kind != nnkVarTy: # Allow us to do `mSelf` for overriding
          typ = nnkVarTy.newTree(typ)
        let theIdent = identDef(newIdentDefs(NimNode(name), typ))
        RoutineNode(result).addParam theIdent

        call.add:
          if isSelf:
            newCall(dBase, NimNode(name))
          else:
            NimNode(name)
    if statement[0].symKind == nskIterator:
      result[^1] = genast(call):
        for x in call:
          yield x
    else:
      result[^1] = call

macro borrower(borrowFor: typed, isMut: static bool, statement: typed): untyped =
  ## `borrowFor` is the type used for replacements on `self` or `mSelf`
  ## `isMut` indicates whether the result is `mut` where it's ambiguous `[]` for instance.
  ## `statement` is the expression which will be lent from
  let statement = statement[^1][^1]
  case statement.kind
  of nnkCall:
    result = genCallBorrow(borrowFor, isMut, statement)

  of nnkAsgn:
    if statement[0].kind == nnkBracketExpr:
      result = genBracketBorrow(isMut, borrowFor, statement)

  of nnkBracketExpr:
    result = genBracketBorrow(isMut, borrowFor, statement)

  of nnkForStmt:
    result = genCallBorrow(borrowFor, isMut, statement[^2])
  else:
    discard
  desym(result)
  echo result.repr

macro lendProcs*(borrowFor: typedesc[distinct], body: untyped): untyped =
  result = newStmtList()
  for stmt in body:
    let
      (isMut, stmt) =
        if stmt.kind == nnkCall and stmt[0].eqIdent"mut":
          (true, stmt[1])
        else:
          (false, stmt)
    result.add:
      genast(stmt, borrowFor, borrow = bindSym"borrower", dBase = bindSym"distinctBase", isMut = newLit(isMut)):
        template letBorrow() =
          borrow(borrowFor, isMut):
            block:
              let self {.inject.} = default(dBase(borrowFor))
              stmt

        template varBorrow() =
          borrow(borrowFor, isMut):
            block:
              var self {.inject.} = default(dBase(borrowFor))
              stmt

        template mSelfBorrow() =
          borrow(borrowFor, isMut):
            block:
              var mSelf {.inject.} = default(dBase(borrowFor))
              stmt
        when compiles(mSelfBorrow()):
          mSelfBorrow()
        else:
          when compiles(letBorrow()):
            letBorrow()

          elif compiles(varBorrow()):
            varBorrow()

          else:
            {.error: "cannot borrow '" & astToStr(stmt) & "'.".}


import std/strutils

type NotString = distinct string

lendProcs(NotString):
  self.contains('a')
  self.contains("")
  self.add("helllo")
  `$`(self)
  `[]`(self, 0)
  `[]`(self, ^1)
  mut: `[]`(mSelf, 0)
  `[]=`(mSelf, 0, 'a')
  `[]=`(mSelf, ^1, 'a')
  `==`(self, self)

  for _ in self.items(): discard
  for _ in self.mitems(): discard
  for x, y in self.pairs(): discard


var notString = NotString("")
notString.add "Hello"
notString.add " World!"
assert notString == NotString("Hello World!")
notString[^1] = 'a'
assert notString[^1] == 'a'
notString[0] = 'b'
assert notString[0] == 'b'
assert "World" in notString
assert 'b' in notString

for ch in notString.mitems:
  ch = '0'

assert notString == NotString("000000000000")
