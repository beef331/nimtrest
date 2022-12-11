import std/[strscans]

type
  Vm = object
    cycle: int
    accumulator: int
    screenBuffer: string

proc draw*(vm: var Vm) =
  if vm.screenBuffer.len in vm.accumulator - 1 .. vm.accumulator + 1:
    vm.screenBuffer.add '#'
  else:
    vm.screenBuffer.add '.'

  if vm.screenBuffer.len == 40:
    echo vm.screenBuffer
    vm.screenBuffer.setLen(0)

proc doReturnVal(vm: Vm): bool = (vm.cycle - 20) mod 40 == 0 and (vm.cycle - 20) div 40 in 0..5

proc inc(vm: var Vm, val: int): int =
  inc vm.cycle
  vm.draw()
  if vm.doReturnVal():
    result = vm.cycle * vm.accumulator
  inc vm.cycle
  if vm.doReturnVal():
    result = vm.cycle * vm.accumulator

  vm.draw()
  inc vm.accumulator, val


proc noop*(vm: var Vm): int =
  inc vm.cycle
  vm.draw()
  if vm.doReturnVal():
    result = vm.cycle * vm.accumulator

proc getSum(): int =
  var vm = Vm(cycle: 0, accumulator: 1)
  for line in lines("input.txt"):
    var amount = 0
    if line == "noop":
      result += vm.noop()
    elif line.scanf("addx $i", amount):
      result += vm.inc(amount)
import std/[times, monotimes]
let start = getMonoTime()
echo getSum(), " ", getMonoTime() - start
