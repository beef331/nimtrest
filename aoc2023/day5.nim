import std/[os, strutils, parseutils, tables, enumerate, strscans]

type 
  Maps = enum
    Seeds
    SeedsToSoil = "seed-to-soil"
    SeedsToFertilizer = "soil-to-fertilizer"
    FertilizerToWater = "fertilizer-to-water"
    WaterToLight = "water-to-light"
    LightToTemperature = "light-to-temperature"
    TemperatureToHumidity = "temperature-to-humidity"
    HumidityToLocation = "humidity-to-location"
  SeedMap = array[Maps.SeedsToSoil..Maps.high, seq[RangeMap]]
  RangeMap = object
    source, dest, offset: int

proc contains(rng: RangeMap, i: int): bool = i in rng.source .. rng.source + rng.offset


proc mapRange(val: int, map: seq[RangeMap]): int =
  for mapVal in map:
    if val in mapVal:
      return mapVal.dest + (val - mapVal.source)
  val

proc mapRange(seed: int, maps: SeedMap): int =
  result = seed.mapRange(maps[SeedsToSoil])
  for map in SeedsToSoil.succ .. Seeds.high:
    result = result.mapRange(maps[map])

proc solveIt(name: string): (int, int) =
  var 
    seeds: seq[int]
    maps: SeedMap 
    state = Seeds

  for lineNum, line in enumerate lines(name):
    if lineNum == 0:
      let start = line.skipUntil(Digits)
      var i = start
      while i < line.len:
        var val: int
        i += line.toOpenArray(i, line.high).parseInt(val) + 1
        seeds.add val
    else:
      if line.len == 0:
        discard
      elif line[0] in Letters:
        let nd = line.find(' ')
        state = parseEnum[Maps](line[0 .. nd - 1])
      else:
        var map: RangeMap 
        if line.scanf("$i $i $i", map.dest, map.source, map.offset):
          maps[state].add map

  result = (int.high, int.high)
  for seed in seeds:
    result[0] = min(seed.mapRange(maps), result[0])

  #Brute force ahead 
  for i in countUp(0, seeds.high, 2):
    for seed in seeds[i] .. seeds[i] + seeds[i + 1]:
      result[1] = min(seed.mapRange(maps), result[1])

echo solveIt(paramStr(1))
