import ../yeacs
import std / lenientops

let
  width = 360.0
  height = 360.0

type
  Size = object of Component
    r: float

  Position = object of Component
    x: float
    y: float

  Velocity = object of Component
    dx: float
    dy: float

  Bounce = object of Component

  Gravity = object of Component
    ddx: float
    ddy: float

  Drag = object of Component
    coefficient: float

  Draw = object of Component

func moveSystem(archetypes: openarray[ArcheTypeBase]) =
  for (pos, vel) in archetypes.foreach (Position, Velocity):
    let oldPosition = pos

    pos.y += vel.dy
    pos.x += vel.dx

    when defined(debugMove):
      debugEcho "Moved " & ecs.inspect(entity) & " from ", oldPosition, " to ", po

proc bounceOnWalls(archetypes: openarray[ArcheTypeBase]) =
  for (pos, vel, _, size) in archetypes.foreach (Position, Velocity, Bounce, Size):
    if pos.x > width - size.r or pos.x < size.r:
      vel.dx = -vel.dx
    if pos.y > height - size.r or pos.y < size.r:
      vel.dy = -vel.dy

proc fall(archetypes: openarray[ArcheTypeBase]) =
  for (pos, vel, gravity, size) in archetypes.foreach (Position, Velocity, Gravity, Size):
    # apply gravity only if not falling
    if not (pos.y > height - size.r or pos.y < size.r):
      vel.dy += gravity.ddy
    if not (pos.x > width - size.r or pos.x < size.r):
      vel.dx += gravity.ddx

proc airResistance(archetypes: openarray[ArcheTypeBase]) =
  for (pos, vel, drag, _) in archetypes.foreach (Position, Velocity, Drag, Size):
    if abs(vel.dy) > 0:
      vel.dy = vel.dy*drag.coefficient
    if abs(vel.dx) > 0:
      vel.dx = vel.dx*drag.coefficient


proc drawBall(archetypes: openarray[ArcheTypeBase]) =
  for (pos, _, size) in archetypes.foreach (Position, Draw, Size):
    ##ellipse(position.x, position.y, 2*size.r)

var
  normalArch = makeArchetype (Size, Position, Velocity)
  bounceArch = makeArchetype (Size, Position, Velocity, Bounce)
  gravityArch = makeArchetype (Size, Position, Velocity, Bounce, Gravity)
  dragArch = makeArchetype (Size, Position, Velocity, Bounce, Gravity, Drag)

proc setup() {. exportc .} =
  ##createCanvas(width, height)

proc draw() {. exportc .} =
  ##background(0)
  template makeArr: auto = [ArcheTypeBase normalArch, bounceArch, gravityArch, dragArch]
  moveSystem(makeArr)
  bounceOnWalls(makeArr)
  fall(makeArr)
  airResistance(makeArr)
  drawBall(makeArr)

var ballId = 1

proc newBall(r, x, y, dx, dy, ddx, ddy: float; bounce, gravity, drag = false, coeff = 1.0) =
  if bounce and not (gravity or drag):
    bounceArch.add (Size(r: r), Position(x: x, y: y), Velocity(dx: dx, dy: dy), Bounce())
  elif gravity and not drag:
    gravityArch.add (Size(r: r), Position(x: x, y: y), Velocity(dx: dx, dy: dy), Bounce(), Gravity(ddx: ddx, ddy: ddy))
  elif drag:
    dragArch.add (Size(r: r), Position(x: x, y: y), Velocity(dx: dx, dy: dy), Bounce(), Gravity(ddx: ddx, ddy: ddy), Drag(coefficient: coeff))
  else:
    normalArch.add (Size(r: r), Position(x: x, y: y), Velocity(dx: dx, dy: dy))

newBall(15.0, 50.0, 180.0, -2.0, 1.0, 0.0, 0.0, true, false)
newBall(10.0, 10.0, 60.0, 4.0, 0.0, 0.0, -1.0, true, true)
newBall(7.0, 180.0, 180.0, 5.0, 2.0, 0.3, 0.5, true, true)
newBall(20.0, 240.0, 40.0, -15.0, 0.0, 0.0, 0.5, true, true, true, 0.99)

