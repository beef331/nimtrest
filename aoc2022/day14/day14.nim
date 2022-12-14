import std/[parseutils, enumerate, intsets, times, monotimes]

const width = 700 # Hey it works here for pt2
type
  ParticleKind = enum Air, Sand, Rock
  Particle = object
    kind: ParticleKind
  ParticleSim = object
    lowestRock: int
    data: array[width * 200, Particle]
    lastParticle: int


proc addRocks(sim: var ParticleSim, frm, to: (int, int)) =
  for x in min(frm[0], to[0]) .. max(frm[0], to[0]):
    for y in min(frm[1], to[1]) .. max(frm[1], to[1]):
      sim.data[x + y * width] = Particle(kind: Rock)
      sim.lowestRock = max(y, sim.lowestRock)

proc parseInput(): ParticleSim =
  for line in lines"input.txt":
    var
      i = 0
      frm, to: (int, int)
    i += line.parseint(frm[0]) + 1
    i += line.parseint(frm[1], i) + 4
    while i < line.len:
      i += line.parseint(to[0], i) + 1
      i += line.parseint(to[1], i) + 4
      result.addRocks(frm, to)
      frm = to

proc fullySimmed(sim: ParticleSim): bool =
  let y = sim.lastParticle div width
  if y > sim.lowestRock + 1:
      return true

proc canStep(sim: ParticleSim, pos: int): bool =
  if pos + width > sim.data.high:
    result = false
  else:
    for nextPos in [pos + width, pos + width - 1, pos + width + 1]:
      if sim.data[nextPos].kind == Air:
        return true

proc step(sim: var ParticleSim) =
  var sandPos = 500
  sim.data[sandPos] = Particle(kind: Sand)

  while sim.canStep(sandPos):
    for nextPos in [sandPos + width, sandPos + width - 1, sandPos + width + 1]:
      if sim.data[nextPos].kind == Air:
        swap(sim.data[nextPos], sim.data[sandPos])
        sandPos = nextPos
        sim.lastParticle = sandPos
        break

proc simulate(sim: var ParticleSim): int =
  result = -1
  while not sim.fullySimmed():
    inc result
    if sim.data[500].kind == Sand:
      break
    sim.step()

var
  sim1 = parseInput()
  sim2 = sim1

var start = getMonoTime()
echo "Pt1: ", sim1.simulate(), " ", getMonoTime() - start
start = getMonoTime()
for i in 0..<width:
  sim2.data[(sim2.lowestRock + 2) * width + i] = Particle(kind: Rock)
echo "Pt2: ", sim2.simulate(), " ", getMonoTime() - start
