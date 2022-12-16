import std/[strscans, sets]

type
  Sensor = object
    x, y: int
    dist: int

  Beacon = object
    x, y: int

  World = object
    sensors: seq[Sensor]
    minX: int
    maxX: int
    beacons: HashSet[Beacon]

proc parseInput(input: string): World =
  for line in lines(input):
    let (success, sensX, sensY, beacX, beacY) = line.scanTuple("Sensor at x=$i, y=$i: closest beacon is at x=$i, y=$i")
    if success:
      result.minX = min(min(sensX, result.minX), beacX)
      result.maxX = max(max(sensX, result.maxX), beacX)
      let dist = abs(beacX - sensX) + abs(beacY - sensY)
      result.sensors.add Sensor(x: sensX, y: sensY, dist: dist)
      result.beacons.incl Beacon(x: beacX, y: beacY)

proc within(sensor: Sensor, x, y: int): bool =
  abs(sensor.x - x) + abs(sensor.y - y) <= sensor.dist

proc canMoveTo(world: World, x: var int, y: int): bool =
  var
    closestDist = int.high
    closestSensor: Sensor
  for sensor in world.sensors:
    let dist = abs(sensor.x - x) + abs(sensor.y - y)
    if dist < closestDist:
      closestSensor = sensor
      closestDist = dist
    if sensor.within(x, y):
      result = true
  if result:
    x += abs(closestSensor.dist - closestDist) + 1

proc solve1(world: World, y: int): int =
  for x in world.minX .. world.maxX:
    for sensor in world.sensors:
      if sensor.within(x, y) and Beacon(x: x, y: y) notin world.beacons:
        inc result
        break

proc solve2(world: World, rng: int): int =
  var x, y = 0
  while world.canMoveTo(x, y):
    if x >= rng:
      x = 0
      inc y

  result = x * 4000000 + y

import std/[times, monotimes]
var start = getMonoTime()
let data = parseInput("input.txt")
echo "Parse: ", getMonoTime() - start
start = getMonoTime()
echo "Pt1: ", solve1(data, 2000000), " ", getMonoTime() - start
start = getMonoTime()
echo "Pt2: ", solve2(data, 4000000), " ", getMonoTime() - start

