import std/[macros, genasts, strutils]

proc extractProcName(s: string): string =
  let splitNames = s.split(".")
  if splitNames.len > 0:
    splitNames[^1]
  else:
    "" # Maybe raise exception

macro makeImportPath(s: typed): untyped =
  if s.kind == nnkSym:
    var
      owner = s.owner
      names = @[$s]
    while owner != nil:
      if $owner != "unknown":
        names.add $owner
      owner = owner.owner
    var name = ""
    for i in countdown(names.high, 0):
      name.add names[i]
      if i > 0:
        name.add "."
    result = newLit name
  else:
    result = newLit $s

macro mock*(name: static string, defaultOrType: typed) =
  let
    procName = name.extractProcName()
  case defaultOrType.getType.typeKind
  of ntyProc:
    result = genast(myProc = ident(procName & "Mock"), callName = ident procName, path = newLit name, defaultOrType):
      var myProc* = defaultOrType
      when myProc is (proc() {.nimcall.}):
        template mockedProc*{callName()}(): untyped =
          {.noRewrite.}:
            when makeImportPath(callName) == path:
              myProc()
            else:
              callName()
      else:
        template mockedProc*{callName(args)}(args: varargs[typed]): untyped =
          {.noRewrite.}:
            when makeImportPath(callName) == path:
              myProc(args)
            else:
              callName(args)
  of ntyTypeDesc:
    result = genast(myProc = ident(procName & "Mock"), callName = ident procName, path = newLit name, defaultOrType):
      var myProc*: defaultOrType
      when myProc is (proc() {.nimcall.}):
        template mockedProc*{callName()}(): untyped =
          {.noRewrite.}:
            when makeImportPath(callName) == path:
              myProc()
            else:
              callName()
      else:
        template mockedProc*{callName(args)}(args: varargs[typed]): untyped =
          {.noRewrite.}:
            when makeImportPath(callName) == path:
              myProc(args)
            else:
              callName(args)
  else:
    error("Is not a 'typedesc' or 'proc'", defaultOrType)
