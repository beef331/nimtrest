import ../yeacs
import std / lenientops
import drawim

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

func moveSystem(world: var World) =
  for (pos, vel) in world.foreach (Position, Velocity):
    let oldPosition = pos

    pos.y += vel.dy
    pos.x += vel.dx

    when defined(debugMove):
      debugEcho "Moved " & ecs.inspect(entity) & " from ", oldPosition, " to ", po

proc bounceOnWalls(world: var World) =
  for (pos, vel, _, size) in world.foreach (Position, Velocity, Bounce, Size):
    if pos.x >= width - size.r or pos.x < size.r:
      vel.dx = -vel.dx
    if pos.y >= height - size.r or pos.y < size.r:
      vel.dy = -vel.dy

proc fall(world: var World) =
  for (pos, vel, gravity, size) in world.foreach (Position, Velocity, Gravity, Size):
    # apply gravity only if not falling
    if not (pos.y > height - size.r or pos.y < size.r):
      vel.dy += gravity.ddy
    if not (pos.x > width - size.r or pos.x < size.r):
      vel.dx += gravity.ddx

proc airResistance(world: var World) =
  for (pos, vel, drag, _) in world.foreach (Position, Velocity, Drag, Size):
    if abs(vel.dy) > 0:
      vel.dy = vel.dy*drag.coefficient
    if abs(vel.dx) > 0:
      vel.dx = vel.dx*drag.coefficient


proc drawBall(world: var World) =
  for (pos, _, size) in world.foreach (Position, Draw, Size):
    ellipseFill(pos.x, pos.y, size.r, size.r)

var world = World()


proc draw() =
  background(0)
  fill(255, 255, 255)
  bounceOnWalls(world)
  fall(world)
  airResistance(world)
  moveSystem(world)
  drawBall(world)

proc newBall(r, x, y, dx, dy, ddx, ddy: float; bounce, gravity, drag = false, coeff = 1.0) =
  var ent = world.addEntity (Size(r: r), Position(x: x, y: y), Velocity(dx: dx, dy: dy), Draw())
  if bounce:
    world.addComponent(ent, Bounce())
  if gravity:
    world.addComponent(ent,  Gravity(ddx: ddx, ddy: ddy))
  if drag:
    world.addComponent(ent,  Drag(coefficient: coeff))



newBall(15.0, 50.0, 180.0, -2.0, 1.0, 0.0, 0.0, true, false)
newBall(10.0, 10.0, 0, 4.0, 0.0, 0.0, -1.0, true, true)
newBall(7.0, 180.0, 180.0, 5.0, 2.0, 0.3, 0.5, true, true)
newBall(20.0, 240.0, 40.0, -15.0, 0.0, 0.0, 0.5, true, true, true, 0.99)

for arch in world.archetypes:
  echo arch.len


run(int width, int height, draw, name = "ecs")
