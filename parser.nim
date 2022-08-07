import std/[macros, options, parseutils, strutils, strformat]
import assume/typeit

type EventType = (string, proc(){.nimcall.})

template shortName*(s: char) {.pragma.}
template longName*(s: string) {.pragma.}
template absolute*(i: int) {.pragma.}
template parser*(s: proc(s: string): Option[auto]) {.pragma.}
template eventFlags*(procs: openarray[EventType]) {.pragma.}
template remainder*(){.pragma.}

type CliError = object of CatchableError


proc isStartOfAbsolute(s: string): bool = s.len > 0 and s[0] != '-'

proc getAbsolute(s: string, index: int): string =
  var
    i = s.parseUntil(result, ' ') + 1# Capture first option
    found =
      if result.isStartOfAbsolute():
        1
      else:
        0
  while i < s.len and found < index:
    i += s.parseUntil(result, ' ', i) + 1
    if result.isStartOfAbsolute():
      inc found

proc getLongName(input, argName: string): string =
  var i = input.skipUntil('-') + 1
  while i + 1 < input.len:
    if input[i] != '-':
      i += input.skipUntil('-', i + 1)
    else:
      let skipped = input.skip(argName, i + 1)
      if skipped == 0:
        i += input.skipUntil('-', i + 1) + 1
      else:
        if i + skipped + 1 < input.len and input[i + skipped + 1] in {':', '='}:
          discard input.parseUntil(result, ' ', i + skipped + 2)
          break

proc getShortName(input: string, argName: char): string =
  var i = input.skipUntil(argName)
  while i < input.len:
    if i == 0 or input[i - 1] != '-' or
      input.toOpenArray(i - 2, i - 1) != " -":
        i += input.skipUntil(argName, i + 1) + 1
    else:
      let offset =
        if input[i + 1] in {':', '='}:
          2
        else:
          1
      discard input.parseUntil(result, ' ', i + offset)
      break


proc cliParse(str: string, _: typedesc[string]): Option[string] = some(str)

proc hasCallback(input, name: string): bool =
  let ind = input.find(name)
  ind >= 0 and (ind + name.len == input.len or input[ind + name.len] == ' ')

proc hasRemainder(t: typedesc): bool =
  typeIt(default(t), {titAllFields}):
    when it.hasCustomPragma(remainder):
      return true

proc fromCli*[t](s: string, _: typedesc[t]): t =
  mixin cliParse

  when t.hasCustomPragma(eventFlags):
    const callbackTable = t.getCustomPragmaVal(eventFlags)
    for name, prc in callbackTable.items:
      if s.hasCallback(name):
        prc()

  template commandParse(t: typed, s: string) =
    mixin cliParse
    when t.hasCustomPragma(parser):
      when t is Option:
        t = t.getCustomPragmaVal(parser)[0](s)
      else:
        let val = t.getCustomPragmaVal(parser)[0](s)
        if val.isNone:
          raise newException(CliError, "Non optional argument skipped " & astToStr(t).split[^1])
        t = val.get
    else:
      when t is Option:
        t = cliParse(s, typeof(t.get))
      else:
        let val = cliParse(s, typeof(t))
        if val.isNone:
          raise newException(CliError, "Non optional argument skipped " & astToStr(t).split[^1])
        t = val.get

  typeit(result, {}):
    {.cast(uncheckedAssign).}:
      when it.hasCustomPragma(absolute):
        let
          ind = it.getCustomPragmaVal(absolute)
          myStr = s.getAbsolute(ind)
        commandParse(it, myStr)
      else:
        when it.hasCustomPragma(longName):
          const myLongName = it.getCustomPragmaVal(longName)
          let myLongStr = s.getLongName(myLongName)
          commandParse(it, myLongStr)

        when it.hasCustomPragma(shortName):
          const myShortName = it.getCustomPragmaVal(shortName)
          let myShortStr = s.getShortName(myShortName)
          when it.hasCustomPragma(longName):
            if myLongStr.len != 0 and myShortStr.len != 0:
              raise newException(CliError, fmt"Duplicate arguments passed '{myLongName}' and '{myShortName}' are the same argument")
          commandParse(it, myShortStr)

        when not it.hasCustomPragma(longName) and not it.hasCustomPragma(shortName):
          const name = astToStr(it).split('.')[^1]
          when name.len == 1:
            commandParse(it, s.getShortName(name[0]))
          else:
            commandParse(it, s.getLongName(name))



import std/strutils

type BuildOptions = enum
  init, build, setup

proc cliParse(s: string, _: typedesc[BuildOptions]): Option[BuildOptions] =
  try:
    some(s.parseEnum[: BuildOptions]())
  except:
    none(BuildOptions)

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
    secondaryOptions {.remainder.}: string


echo """build --output:hello hello.nim""".fromCli(MyOptions)
echo """init --nimbase=hello test --sdk:hmmm/huh """.fromCli(MyOptions)
echo """setup --nimbase=hello test -s:hmm""".fromCli(MyOptions)
try:
  echo """version""".fromCli(MyOptions)
except: discard
try:
  echo """help""".fromCli(MyOptions)
except: discard

