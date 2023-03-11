import std/[macros, options, parseutils, strutils, strformat, strbasics, sets]
import assume/typeit

template shortName*(s: char) {.pragma.}
template longName*(s: string) {.pragma.}
template absolutePos*(i: int) {.pragma.}
template parser*(s: proc(s: openarray[char]): Option[auto]) {.pragma.}
template helpMessage*(s: string) {.pragma.}
template toggle*(){.pragma.}
template remainder*(){.pragma.}

type
  CliError = object of CatchableError
  OptionKind = enum
    Unlabelled
    LongOpt
    ShortOpt
  ParsedOpt = tuple[kind: OptionKind, value: Slice[int]]

template toOa(s: openarray[char], slice: Slice[int]): auto = s.toOpenArray(slice.a, slice.b)

proc asString(oa: openArray[char]): string =
  if oa.len > 0:
    result.add oa


proc cliParse(str: openArray[char], result: var string) =
  result = str.asString()

proc cliParse(str: openArray[char], result: var Option[string])=
  result = some(str.asString())

iterator args(
  oa: openArray[char],
  seperators = {':', '='},
  filter = {OptionKind.low .. OptionKind.high}
  ): ParsedOpt =
  var i = oa.skipWhile(WhiteSpace)
  while i < oa.len:
    case oa[i]
    of '-':
      if i + 1 < oa.len and oa[i + 1] == '-':
        i += 2
        let start = i
        i += oa.toOpenArray(i, oa.high).skipUntil(WhiteSpace) - 1
        yield (LongOpt, start..i)
        inc i
      else:
        inc i
        let start = i
        i += oa.toOpenArray(i, oa.high).skipUntil(WhiteSpace) - 1
        yield (ShortOpt, start..i)
        inc i

    else:
      let nd = min(oa.high, oa.toOpenArray(i, oa.high).skipUntil(WhiteSpace) - 1)
      yield (Unlabelled, i .. i + nd)
      i += nd + 1
    i += oa.toOpenArray(i, oa.high).skipWhile(WhiteSpace)

iterator nameRanges(oa: openarray[char], opt: ParsedOpt, seperator = {':', '='}): Slice[int] =
  case opt.kind
  of ShortOpt, LongOpt:
    let namePos = oa.skipUntil(seperator) - 1
    yield 0..namePos
  else:
    yield 0..oa.high


proc matchesNames(needle: openarray[char], haystack: openArray[string]): bool =
  for name in haystack:
    if name.len == 1 and needle.len == 1:
      if needle == name:
        return true
    else:
      if needle == name:
        return true

proc fromCli*[t]( _: typedesc[t], cmdLine: openarray[char]): t =
  mixin cliParse, toggleParam
  var
    usedRange: seq[Slice[int]]
    absCount = 1
  for arg in cmdLine.args():
    block assignArg:
      typeIt(result, {}):
        const
          hasLongName = it.hasCustomPragma(longName)
          hasShortName = it.hasCustomPragma(shortName)
          hasToggle = it.hasCustomPragma(toggle)
          hasAbs = it.hasCustomPragma(absolutePos)

        when (hasLongName or hasShortName or hasToggle) and hasAbs:
          {.error: "Cannot give a name or make a toggle of '" & astToStr(it).split('.')[^1] & "'." .}

        const
            names =
              when hasLongName and hasShortName:
                [it.getCustomPragmaVal(longName), $it.getCustomPragmaVal(shortName)]
              elif hasLongName:
                [it.getCustomPragmaVal(longName)]
              elif hasShortName:
                [$it.getCustomPragmaVal(shortName)]
              else:
                [astToStr(it).split(".")[^1]]

        case arg.kind
        of ShortOpt, LongOpt:
          for name in nameRanges(cmdLine.toOpenArray(arg.value.a, arg.value.b), arg):
            if cmdLine.toOpenArray(name.a + arg.value.a, name.b + arg.value.a).matchesNames(names):
              {.cast(uncheckedAssign).}:
                when hasToggle:
                  toggleParam(it)
                else:
                  cliParse(cmdLine.toOpenArray(name.b + 2 + arg.value.a, arg.value.b), it)
              break assignArg

        of Unlabelled:
          when hasAbs:
            const absVal = it.getCustomPragmaVal(absolutePos)
            if absVal == absCount:
              {.cast(uncheckedAssign).}:
                cliParse(cmdLine.toOpenArray(arg.value.a, arg.value.b), it)
              inc absCount
              break assignArg


proc printHelpMessage(T: typedesc) =
  var
    res: T
    printed: HashSet[string]

  typeIt(res, {titAllFields, titDeclaredOrder}):
    var msg: string
    const
      hasLongName = it.hasCustomPragma(longName)
      hasShortName = it.hasCustomPragma(shortName)
      isToggle = it.hasCustomPragma(toggle)

    when hasLongName:
      msg.add "--"
      msg.add it.getCustomPragmaVal(longName)
      when not isToggle:
        msg.add ":"
        msg.add $typeof(it)
      msg.add " "

    when hasShortName:
      msg.add "-"
      msg.add it.getCustomPragmaVal(shortName)
      when not isToggle:
        msg.add ":"
        msg.add $typeof(it)
      msg.add " "

    when not (hasShortName or hasLongName):
      msg.add astToStr(it).split(".")[^1]
      msg.add " "

    when it.hasCustomPragma(helpMessage):
      msg.add " | "
      msg.add it.getCustomPragmaVal(helpMessage)
      msg.add " "

    if msg notin printed:
      echo msg
      printed.incl msg


type BuildOptions = enum
  init, build, setup, help, version

proc cliParse(oa: openArray[char], result: var BuildOptions) =
  for opt in BuildOptions:
    if $opt == oa:
      result = opt
      break

proc toggleParam(b: var bool) = b = true


type
  MyOptions = object
    case command {.absolutePos: 1, helpMessage: "What command to run, this can be 'init', 'setup', 'build', 'version', 'help'." .}: BuildOptions
    of init:
      name {.absolutePos: 2, helpMessage: "Name of project.".}: string
      initSdk {.longName: "sdk", shortname: 's', helpMessage: "Sdk path to use for this project".}: string
      nimbase {.helpMessage: "Location of nimbase.h to use."}: string
    of setup:
      setupSdk {.longName: "sdk", shortname: 's', helpMessage: "Sdk path to use for this project".}: string
    of build:
      mainProgram {.absolutePos: 2 helpMessage: "Program to compile".}: string
      output {.helpMessage: "output directory".}: string
    else: discard
    doThing {.toggle, shortName: 'd', helpMessage: "Whether one should doThing.".}: bool

MyOptions.printHelpMessage()


echo MyOptions.fromCli """build --output:hello hello.nim"""
echo MyOptions.fromCli """init --nimbase=hello test --sdk:hmmm/huh """
echo MyOptions.fromCli """setup --nimbase=hello test -s:hmm -d """
