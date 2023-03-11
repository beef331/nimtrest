import std/[macros, genasts, strutils, macrocache, decls, sets]
import pkg/micros


const adtTable = CacheTable"FungusTable"

type FungusConvDefect = object of Defect

macro subscribeAdt(name: typed, enumFields: untyped, typeNames: untyped) =
  adtTable[name.signatureHash] = newStmtList(name, enumFields, typenames)

macro adtEnum*(name, body: untyped): untyped =
  var typeNames, enumFields, addons: seq[NimNode]

  let caseDef = caseStmt(NimName ident"kind")
  for entry in body:
    case entry.kind
    of nnkIdent:
      typeNames.add entry
      let enumName = ident($entry & "Kind")
      enumFields.add NimNode enumField(enumName)
      caseDef.add ofBranch(enumName, newNilLit())

    of nnkCall, nnkCommand:
      if entry.len != 2 or (entry[1].kind != nnkStmtList and entry[1][0].kind != nnkTupleTy):
        error("Invalid entry expected `name: tuple[...].")
      typeNames.add entry[0]
      let
        enumName = ident($entry[0] & "Kind")
        dataName = NimName(ident(entry[0].repr & "Data"))
        typ = entry[0]
      enumFields.add NimNode enumField(enumName)
      caseDef.add ofBranch(enumName, NimNode identDef(dataName, typ = entry[1]))
      addons.add:
        genAst(
          name,
          enumName,
          dataName = NimNode(dataName),
          typ,
          tupl = entry[1],
          procName = ident("to" & $entry[0])
        ):
          converter procName(arg: sink tupl): typ = typ name(kind: enumName, dataName: arg)
          converter toTuple(arg: typ): lent tupl = name(arg).dataName
          converter toTuple(arg: var typ): var tupl = name(arg).dataName

          converter `to name`(arg: typ): name = name(arg)
          converter `to name`(arg: var typ): var name = name(arg)

          proc to(val: name, _: typedesc[typ]): lent typ =
            if val.kind != enumName:
              raise (ref FungusConvDefect)(msg: "Cannot convert '$#' to '$#'." % [$val.kind, $enumName])
            typ name(val)

          proc to(val: var name, _: typedesc[typ]): var typ =
            if val.kind != enumName:
              raise (ref FungusConvDefect)(msg: "Cannot convert '$#' to '$#'." % [$val.kind, $enumName])
            typ name(val)

          proc init(_: typedesc[typ], tup: tupl): typ = tup

      for iDef in entry[1][0]:
        let fieldTyp = iDef[^2]
        for field in iDef[0..^3]:
          addons.add:
            genast(field, typ, fieldTyp, name, dataName = NimNode(dataName)):
              proc field(val: typ): lent fieldTyp = name(val).dataName.field
              proc field(val: var typ): var fieldTyp = name(val).dataName.field
              proc `field=`(val: var typ, newVal: fieldTyp) = name(val).dataName.field = newVal


    else:
      error("Invalid entry, expected either an 'name' or 'name: tuple[...]'.", entry)

  let enumName = ident $name & "Kind"
  result = newStmtList(NimNode enumDef(NimName enumName, enumFields))
  NimNode(caseDef)[0] = NimNode identDef(NimName NimNode(caseDef)[0], NimNode enumName)
  let
    objDef = objectDef(NimName name)
    recCase = nnkRecCase.newTree()
  NimNode(caseDef).copyChildrenTo(recCase)
  objDef.recList = nnkRecList.newTree recCase
  result[0].add NimNode objDef

  for i, typeName in typeNames:
    result.add:
      genast(name, typeName, field = enumFields[i]):
        type typeName = distinct name

  result.add addons
  result.add newCall(bindSym"subscribeAdt", name,
    nnkBracket.newTree(enumFields),
    nnkBracket.newTree(typeNames)
  )

proc getKind(data, toLookFor: NimNode): NimNode =
  for i, name in data[2]:
    if name.eqIdent(toLookFor):
      return data[1][i]
  error("Invalid `kind`.", toLookFor)

macro match*(val: typed, branches: varargs[untyped]): untyped =
  result = nnkIfStmt.newTree()
  let
    adtData = adtTable[val.getTypeInst.signatureHash]
    valIsNotMut = val.kind != nnkSym or val.symKind != nskVar

  var implemented: HashSet[string]

  for branch in branches:
    if branch.kind in {nnkElse, nnkElifBranch}:
      result.add branch
    else:
      case branch[0].kind
      of nnkInfix: # We're doing a named match
        if branch[0][0].kind != nnkIdent or not branch[0][0].eqIdent"as":
          error("Invaid operation expected 'as'.", branch[0][0])

        let kind = getKind(adtData, branch[0][1])
        case branch[0][^1].kind
        of nnkIdent: # emit a `let`
          let injection = branch[^1].copyNimTree
          injection.insert 0, newLetStmt(branch[0][^1], newCall("to", val, branch[0][1]))
          result.add nnkElifBranch.newTree(
            infix(nnkDotExpr.newTree(val, ident"kind"), "==", kind),
            injection)

        of nnkCall, nnkCommand: # Check if it's 'mut', and `val` is mut, emit `var name {.byaddr.} = val`...?
          if not branch[0][^1][0].eqIdent"mut":
            error("Can only make a 'mut' call.", branch[0][^1][0])

          if valIsNotMut:
            error("Can only make a 'mut' reference to a mutable variable.", val)

          let injection = branch[^1].copyNimTree
          injection.insert 0:
            genAst(val, byaddr = bindSym"byaddr", name = branch[0][^1][1], destType = branch[0][1]):
              var name {.byaddr.} = to(val, destType)
          result.add nnkElifBranch.newTree(
            infix(nnkDotExpr.newTree(val, ident"kind"), "==", kind),
            injection)


        of nnkTupleConstr: # same as a call check if each a param is a `mut` if so emit a `byAddr` per field, also perhaps should check field count
          let injection = branch[^1].copyNimTree
          for i, x in branch[0][^1]:
            case x.kind
            of nnkCall, nnkCommand:
              if not x[0].eqIdent"mut":
                error("Invalid call inside match.", x)

              if valIsNotMut:
                error("Can only make a 'mut' reference to a mutable variable.", val)

              injection.insert 0:
                genast(val, name = x[1], index = newLit(i), destType = branch[0][1], byAddr = bindSym"byaddr"):
                  var name {.byaddr.} = to(val, destType).toTuple[index]


            of nnkIdent:
              if not x.eqIdent"_":
                injection.insert 0:
                  genast(val, name = x, index = newLit(i), destType = branch[0][1]):
                    let name = to(val, destType).toTuple[index]

            else:
              error("Invalid capture statement.", x)
          result.add nnkElifBranch.newTree(
            infix(nnkDotExpr.newTree(val, ident"kind"), "==", kind),
            injection)

        else:
          error("Invalid alias statement", branch[0][^1])
        implemented.incl branch[0][1].repr

      of nnkIdent: # Just a kind match
        let kind = getKind(adtData, branch[0])
        result.add nnkElifBranch.newTree(
          infix(nnkDotExpr.newTree(val, ident"kind"), "==", kind),
          branch[^1])
        implemented.incl branch[0].repr
      else: error("Invalid branch not dong a match.", branch)

  if result[^1].kind != nnkElse:
    var unimplemented: HashSet[string]
    for kind in adtData[^1]:
      let theRepr = kind.repr
      if theRepr notin implemented:
        unimplemented.incl theRepr
    if unimplemented.len > 0:
      error("Unhandled type branch for: " & $unimplemented)


adtEnum(Shape):
  None
  Circle: tuple[x, y, r: int]
  Rectangle: tuple[x, y, w, h: int]
  Line: tuple[x1, y1, x2, y2: int]


var a = Shape Circle.init (x: 10, y: 100, r: 100)
a.to(Circle).r = 300
echo a.to(Circle).toTuple
a = Shape Line.init (0, 0, 1, 1)

match a:
of Circle as mut circ:
  circ.r = 1000
  echo circ.toTuple
of Rectangle as rect:
  echo rect.toTuple
of Line as (mut x1, _, x2, _):
  inc x1
  echo a
else: discard

#[
  if (let (isCirc, circ) = a.match(Circle); isCirc):
    echo "hmm"
]#
