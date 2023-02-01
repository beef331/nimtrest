import std/[macros, options, parseutils, strutils, strformat, strbasics]
import assume/typeit

template shortName*(s: char) {.pragma.}
template longName*(s: string) {.pragma.}
template absolutePos*(i: int) {.pragma.}
template parser*(s: proc(s: openarray[char]): Option[auto]) {.pragma.}
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
                  echo astToStr(it), " assigned to ",  cmdLine.toOpenArray(name.b + 2 + arg.value.a, arg.value.b)
                  cliParse(cmdLine.toOpenArray(name.b + 2 + arg.value.a, arg.value.b), it)
              break assignArg

        of Unlabelled:
          when hasAbs:
            const absVal = it.getCustomPragmaVal(absolutePos)
            if absVal == absCount:
              {.cast(uncheckedAssign).}:
                echo astToStr(it), " assigned to ",  cmdLine.toOpenArray(arg.value.a, arg.value.b)
                cliParse(cmdLine.toOpenArray(arg.value.a, arg.value.b), it)
              inc absCount
              break assignArg

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
    case command {.absolutePos: 1.}: BuildOptions
    of init:
      name {.absolutePos: 2.}: string
      initSdk {.longName: "sdk", shortname: 's'.}: string
      nimbase: string
    of setup:
      setupSdk {.longName: "sdk", shortname: 's'.}: string
    of build:
      mainProgram {.absolutePos: 2.}: string
      output: string
    else: discard
    doThing {.toggle, shortName: 'd'.}: bool


echo MyOptions.fromCli """build --output:hello hello.nim"""
echo MyOptions.fromCli """init --nimbase=hello test --sdk:hmmm/huh """
echo MyOptions.fromCli """setup --nimbase=hello test -s:hmm -d """
