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

proc moveSystem(world: var World) =
  for (pos, vel, size) in world.foreach (Position, Velocity, Size):
    let oldPosition = pos

    pos.y += vel.dy
    pos.x += vel.dx

    pos.x = clamp(pos.x, size.r, width - size.r)
    pos.y = clamp(pos.y, size.r, height - size.r)

    when defined(debugMove):
      debugEcho "Moved " & ecs.inspect(entity) & " from ", oldPosition, " to ", po

proc bounceOnWalls(world: var World) =
  for (pos, vel, _, size) in world.foreach (Position, Velocity, Bounce, Size):
    if pos.x >= width - size.r or pos.x <= size.r:
      vel.dx *= -1
    if pos.y >= height - size.r or pos.y <= size.r:
      vel.dy *= -1

proc fall(world: var World) =
  var fallQuery {.global.}: QueryIndex[(Position, Velocity, Gravity, Size)]
  for (pos, vel, gravity, size) in world.query(fallQuery):
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
  var drawQuery {.global.}: QueryIndex[(Position, Draw, Size)]
  for (pos, _, size) in world.query(drawQuery):
    circleFill(pos.x, pos.y, size.r)


var world = World()


proc draw() =
  background(0)
  fill(255, 255, 255)
  bounceOnWalls(world)
  fall(world)
  airResistance(world)
  moveSystem(world)
  drawBall(world)

proc newBall(r, x, y, dx, dy: float; bounce: bool, ddx, ddy = 0d; coeff = 1.0) =
  var ent = world.addEntity (Size(r: r), Position(x: x, y: y), Velocity(dx: dx, dy: dy), Draw())
  echo "Add entity"
  if bounce:
    echo "Add Bounce"
    world.addComponent(ent, Bounce())

  if ddx != 0 or ddy != 0:
    echo "Add Gravity"
    world.addComponent(ent,  Gravity(ddx: ddx, ddy: -ddy))

  if coeff < 1:
    echo "Add Drag"
    world.addComponent(ent,  Drag(coefficient: coeff))



newBall(15.0, width / 2, height / 2, 1, 0.4, false, 0.3, 0.5)
newBall(10.0, 20, 0, 4.0, 0.0, true, 0, 1)
newBall(7.0, 180.0, 180.0, 5.0, 2.0, true, 0.3, 0.5)
newBall(15.0, 50.0, 180.0, -2.0, 1.0, true, 0.0, -1)
newBall(20.0, 240.0, 40.0, 15.0, 0.0, true, 0.0, -1, 0.99)


run(int width, int height, draw, name = "ecs")
