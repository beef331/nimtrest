proc doThing() = echo "world"

proc doOtherThing() = doThing()

doOtherThing()
doThingMock = proc() = echo "hello world"
doOtherThing()
doThingMock = proc() = echo "Hmm"
doOtherThing()

import std/random

var s = ""
for x in 0..10:
  s.add rand('a'..'z')
echo s
