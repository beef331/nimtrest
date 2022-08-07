import std/[macros, options, parseutils, strutils, strformat, strbasics]
import assume/typeit

type EventType = (string, proc(){.nimcall.})

template shortName*(s: char) {.pragma.}
template longName*(s: string) {.pragma.}
template absolute*(i: int) {.pragma.}
template parser*(s: proc(s: string): Option[auto]) {.pragma.}
template eventFlags*(procs: openarray[EventType]) {.pragma.}
template remainder*(){.pragma.}

type CliError = object of CatchableError

template toOa(s: string, slice: Slice[int]): auto = s.toOpenArray(slice.a, slice.b)
proc asString(oa: openArray[char]): string =
  result.add oa

proc isStartOfAbsolute(s: openArray[char]): bool = s.len > 0 and s[0] != '-'

proc getAbsolute(s: string, index: int): Slice[int] =
  result.a = 0
  result.b = s.skipUntil(' ', result.a) - 1
  var found =
      if s.toOa(result).isStartOfAbsolute():
        1
      else:
        0
  while result.a < s.len and found < index:
    result.a += s.skipUntil(' ', result.a) + 1
    result.b += s.skipUntil(' ', result.a) + 1
    if s.toOa(result).isStartOfAbsolute():
      inc found

proc getLongRange(input, argName: string): Slice[int] =
  result.a = input.skipUntil('-') + 1
  while result.a + 1 < input.len:
    if input[result.a] != '-':
      result.a += input.skipUntil('-', result.a + 1)
    else:
      let skipped = input.skip(argName, result.a + 1)
      if skipped == 0:
        result.a += input.skipUntil('-', result.a + 1) + 1
      else:
        if result.a + skipped + 1 < input.len and input[result.a + skipped + 1] in {':', '='}:
          result.a += skipped + 2
          result.b = result.a + input.skipUntil(' ', result.a) - 1
          break

proc getShortRange(input: string, argName: char): Slice[int] =
  result.a = input.skipUntil(argName)
  while result.a < input.len:
    if result.a == 0 or input[result.a - 1] != '-' or
      input.toOpenArray(result.a - 2,  result.a - 1) != " -":
        result.a += input.skipUntil(argName, result.a + 1) + 1
    else:
      let offset =
        if input[result.a + 1] in {':', '='}:
          2
        else:
          1
      result.b = input.skipUntil(' ', result.a + offset) - 1
      break


proc cliParse(str: openArray[char], _: typedesc[string]): Option[string] =
  result = some(str.asString())

proc callbackRange(input, name: string): Slice[int]=
  result.a = input.find(name)
  if result.a >= 0 and (result.a + name.len == input.len or input[result.a + name.len] == ' '):
    result.b = result.a + name.len

proc hasRemainder(t: typedesc): bool =
  typeIt(default(t), {titAllFields}):
    when it.hasCustomPragma(remainder):
      return true

proc unusedRange(s: string, used: seq[Slice[int]]) =
  var
    vals = used
    totalUsed = vals.pop
  while vals.len > 0:
    for i, val in vals:
      let
        start = min(totalUsed.b, val.b)
        target = max(totalUsed.a, val.a)
        skipped = skipWhile(s, WhiteSpace, min(totalUsed.b, val.b) + 1)
      if skipped + start == target - 1:
        totalUsed.a = start
        totalUsed.b = max(totalUsed.b, val.b)
        vals.del(i)
        continue


proc fromCli*[t](s: string, _: typedesc[t]): t =
  mixin cliParse
  var usedRange: seq[Slice[int]]

  when t.hasCustomPragma(eventFlags):
    const callbackTable = t.getCustomPragmaVal(eventFlags)
    for name, prc in callbackTable.items:
      let callRange = s.callbackRange(name)
      if callRange.a >= 0:
        usedRange.add callrange
        prc()

  template commandParse(t: typed, s: openArray[char]) =
    template noOptSkippedError = raise newException(CliError, "Non optional argument skipped " & astToStr(t).split(".")[^1])
    mixin cliParse
    when t.hasCustomPragma(parser):
      when t is Option:
        t = t.getCustomPragmaVal(parser)[0](s)
      else:
        let val = t.getCustomPragmaVal(parser)[0](s)
        if val.isNone:
          noOptSkippedError()
        t = val.get
    else:
      when t is Option:
        t = cliParse(s, typeof(t.get))
      else:
        let val = cliParse(s, typeof(t))
        if val.isNone:
          noOptSkippedError()
        t = val.get

  typeit(result, {}):
    template rangeCheck(rng: Slice[int]) =
      when it isnot Option:
        if rng.len == 0:
          raise newException(CliError, "Non optional argument skipped " & astToStr(it).split(".")[^1])

    {.cast(uncheckedAssign).}:
      when it.hasCustomPragma(absolute):
        let
          ind = it.getCustomPragmaVal(absolute)
          absRange = s.getAbsolute(ind)
        usedRange.add absRange
        commandParse(it, s.toOA(absRange))
      else:
        when it.hasCustomPragma(longName):
          const myLongName = it.getCustomPragmaVal(longName)
          var myLongRange = s.getLongRange(myLongName)
          rangeCheck(myLongRange)
          if myLongRange.len > 0:
            commandParse(it, s.toOa(myLongRange))
            myLongRange.a -= 2 ## Go back to `--` for usage checking
            usedRange.add myLongRange

        when it.hasCustomPragma(shortName):
          const myShortName = it.getCustomPragmaVal(shortName)
          var myShortRange = s.getShortRange(myShortName)
          when it.hasCustomPragma(longName):
            if myLongRange.len > 0 and myShortRange.len > 0:
              raise newException(CliError, fmt"Duplicate arguments passed '{myLongName}' and '{myShortName}' are the same argument")
          rangeCheck(myShortRange)
          if myShortRange.len > 0:
            commandParse(it, s.toOa(myShortRange))
            myShortRange.a -= 1
            usedRange.add myShortRange

        when not it.hasCustomPragma(longName) and not it.hasCustomPragma(shortName):
          const name = astToStr(it).split('.')[^1]
          when name.len == 1:
            var myShortRange = s.getShortRange(name[0])
            rangeCheck(myShortRange)
            if shortRange.len > 0:
              commandParse(it, myShortRange)
              myShortRange.a -= 1
              usedRange.add myShortRange
          else:
            var myLongRange = s.getLongRange(name)
            rangeCheck(myLongRange)
            if myLongRange.len > 0:
              commandParse(it, s.toOa(myLongRange))
              myLongRange.a -= 2
              usedRange.add myLongRange

  ##s.unusedRange(usedRange)

type BuildOptions = enum
  init, build, setup

proc cliParse(s: openArray[char], _: typedesc[BuildOptions]): Option[BuildOptions] =
  result = none(BuildOptions)
  for opt in BuildOptions:
    if $opt == s:
      result = some(opt)
      break

proc showVersion =
  echo "0.0.0"

proc showHelp =
  echo "Helllp me"


type
  MyOptions {.eventFlags([("version", showVersion), ("help", showHelp)]).} = object
    case command {.absolute: 1.}: BuildOptions
    of init:
      name {.absolute: 2.}: string
      initSdk {.longName: "sdk", shortname: 's'.}: Option[string]
      nimbase: Option[string]
    of setup:
      setupSdk {.longName: "sdk", shortname: 's'.}: Option[string]
    of build:
      mainProgram {.absolute: 2.}: string
      output: Option[string]


echo """build --output:hello hello.nim""".fromCli(MyOptions)
echo """init --nimbase=hello test --sdk:hmmm/huh """.fromCli(MyOptions)
echo """setup --nimbase=hello test -s:hmm""".fromCli(MyOptions)
try:
  echo """version""".fromCli(MyOptions)
except: discard
try:
  echo """help""".fromCli(MyOptions)
except: discard

