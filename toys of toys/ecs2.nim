import ../yeacs

type
  Health = distinct int
  Armour = distinct int
  Locomotion = object
    speed: int
    posX, posY: int
  Gun = object
    damage: int

proc addMarine(world: var World): Entity {.discardable.} =
  world.addEntity (Health(100), Locomotion(speed: 5), Gun(damage: 1))

proc addTank(world: var World): Entity {.discardable.} =
  world.addEntity (Health(100), Armour(300), Locomotion(speed: 10), Gun(damage: 100))

var world = World()
let raynor = world.addMarine()
world.addMarine()
world.addTank()

for (locomotion,) in world.foreach (Locomotion,):
  locomotion.posX += 1

for (gun, ) in world.foreach (Gun,):
  echo "Fire: ", gun.damage

for (health, locomotion, gun) in world.foreach (Health, Locomotion, Gun, Not[Armour]):
  echo "Here's jimmy"

for (health, armour, locomotion, gun) in world.foreach (Health, Armour, Locomotion, Gun):
  echo "Here's tank"

world.addComponent raynor, Armour(300)

for (health, armour, locomotion, gun) in world.foreach (Health, Armour, Locomotion, Gun):
  if locomotion.speed == 5:
    echo "Here's jimmy"
  else:
    echo "Here's tank"


