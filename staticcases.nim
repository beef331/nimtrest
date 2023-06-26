import micros
import std/macros

macro typeCase*(branches: varargs[untyped]): untyped =
  let
    whenSt = whenStmt()
    toCheck = branches[0]
  var foundElifBranches = 0
  for i, branch in branches:
    if i > 0:
      case branch.kind
      of nnkOfBranch:
        for j in 0 ..< branch.len - 1:
          whenSt.add elifBranch(newCall("is", toCheck, branch[j]), branch[^1])
      of nnkElifBranch:
        if foundElifBranches > 0:
          error("Too many elif branches specified", branch)
        NimNode(whenSt).add branch
        inc foundElifBranches
      of nnkElse:
        NimNode(whenSt).add branch
      else:
        error("Unexpected flow control", branch)
  result = NimNode(whenSt)

macro staticCase*(branches: varargs[untyped]): untyped =
  let
    whenSt = whenStmt()
    toCheck = branches[0]
  var foundElifBranches = 0
  for i, branch in branches:
    if i > 0:
      case branch.kind
      of nnkOfBranch:
        for j in 0 ..< branch.len - 1:
         case branch[j].kind
         of nnkCurly:
          whenSt.add elifBranch(newCall("contains", branch[j], toCheck), branch[^1])
         of nnkInfix:
           if branch[j][0].eqIdent "..":
             whenSt.add elifBranch(newCall("contains", branch[j], toCheck), branch[^1])
           else:
             whenSt.add elifBranch(newCall("==", toCheck, branch[j]), branch[^1])
         else:
          whenSt.add elifBranch(newCall("==", toCheck, branch[j]), branch[^1])
      of nnkElifBranch:
        if foundElifBranches > 0:
          error("Too many elif branches specified", branch)
        NimNode(whenSt).add branch
        inc foundElifBranches
      of nnkElse:
        NimNode(whenSt).add branch
      else:
        error("Unexpected flow control", branch)
  result = NimNode(whenSt)

var a = "hello"

typeCase typeof(a):
of int:
  echo "Hello"
of string:
  echo "world"

const b = "hello"

staticCase b:
of "hello":
  import std/strutils
of "huh":
  import std/sequtils

var c = strutils.allCharsInSet

staticCase 10:
of 0..30:
  static: echo "Hello"
of 3, 4, 5:
  static: echo "Hmm"

template getType(i: static int): auto =
  staticCase i:
  of 8:
    uint8
  of 16:
    uint16
  of 32:
    uint32
  of 64:
    uint64
  else:
   {.error: "Whatchu want".}
static: 
  assert getType(8) is uint8
  assert getType(16) is uint16
  assert getType(32) is uint32
  assert getType(64) is uint64
  assert not compiles(getType(1))
var d = default getType(8)
