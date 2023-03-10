import std/[macros, genasts, strutils]
import pkg/micros

type FungusConvDefect = object of Defect

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
      if entry.len != 2:
        error("Invalid entry expected `name: tuple[...].")
      typeNames.add entry[0]
      let
        enumName = ident($entry[0] & "Kind")
        dataName = NimName(ident(entry[0].repr & "Data"))
        typ = entry[0]
      enumFields.add NimNode enumField(enumName)
      caseDef.add ofBranch(enumName, NimNode identDef(dataName, typ = entry[1]))
      addons.add:
        genAst(name, enumName, dataName = NimNode(dataName), typ, tupl = entry[1], procName = ident("to" & $entry[0])):
          converter procName(arg: sink tupl): typ= typ name(kind: enumName, dataName: arg)
          converter toTuple(arg: typ): lent tupl = name(arg).dataName
          converter toTuple(arg: var typ): var tupl = name(arg).dataName
          proc to(val: name, _: typedesc[typ]): lent typ =
            if val.kind != enumName:
              raise (ref FungusConvDefect)(msg: "Cannot convert '$#' to '$#'." % [$val.kind, $enumName])
            typ name(val)
          proc to(val: var name, _: typedesc[typ]): typ =
            if val.kind != enumName:
              raise (ref FungusConvDefect)(msg: "Cannot convert '$#' to '$#'." % [$val.kind, $enumName])
            typ name(val)

      for iDef in entry[1][0]:
        let fieldTyp = iDef[^2]
        for field in iDef[0..^3]:
          addons.add:
            genast(field, typ, fieldTyp, name, dataName = NimNode(dataName)):
              proc field(val: typ): lent fieldTyp = name(val).dataName.field
              proc field(val: var typ): var fieldTyp = name(val).dataName.field
              proc field(val: var typ, newVal: fieldTyp) = name(val).dataName.field = newVal


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

# TODO: Add a copy of staticcases for primitive matching


adtEnum(Shape):
  None
  Circle: tuple[r, x, y: int]
  Rectangle: tuple[w, h, x, y: int]
  Line: tuple[x1, y1, x2, y2: int]


var a = Shape (r: 100, x: 10, y: 100).toCircle()
echo a.to(Circle).r
echo a.to(Circle)



