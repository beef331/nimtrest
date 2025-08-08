import ../yeacs
import strutils

type
  UnitKind = enum
    Soldier, Truck, Ship, Root

  Unit = object
    name: string
    kind: UnitKind
    position: int
    children: seq[Id]

proc makeSoldier(name: string, position: int): (Id, Unit) =
  (Id(), Unit(kind: Soldier, position: position, name: name))

proc makeTruck(name: string, position: int, children: seq[Id]): (Id, Unit) =
  (Id(), Unit(kind: Truck, position: position, name: name, children: children))

proc makeShip(name: string, position: int, children: seq[Id]): (Id, Unit) =
  (Id(), Unit(kind: Ship, position: position, name: name, children: children))

proc makeRoot(children: seq[Id]): (Id, Unit) =
  (Id(), Unit(kind: Root, position: 0, name: "root", children: children))

proc move(unit: var Unit, delta: int) =
  unit.position += delta

proc show(world: var World, unit: Unit, globalPosition: int = 0, depth: int = 0) =
  let unitGlobalPosition = unit.position + globalPosition
  echo "   ".repeat(depth), "[", unit.name, " local pos: ", unit.position, " global pos: ", unitGlobalPosition, " kind: ", unit.kind, "]"
  for child in unit.children:
    let childEntity = world.getEntity(child)
    for childUnit in world.component(childEntity, Unit):
      world.show(childUnit, unitGlobalPosition, depth + 1)

proc show(world: var World, root: Entity) =
  for rootUnit in world.component(root, Unit):
    world.show(rootUnit)

var world = World()

let abe = world.addEntity makeSoldier("abe", 0)
let ben = world.addEntity makeSoldier("ben", 1)
let bob = world.addEntity makeSoldier("bob", 1)
let dom = world.addEntity makeSoldier("dom", 2)
let ray = world.addEntity makeSoldier("ray", 2)
let rob = world.addEntity makeSoldier("rob", 3)

let truckA = world.addEntity makeTruck("truck A", 0, @[abe.id, ben.id])
let truckB = world.addEntity makeTruck("truck B", 5, @[bob.id, dom.id])

let ship = world.addEntity makeShip("ship", 0, @[ray.id, truckA.id])

let root = world.addEntity makeRoot(@[ship.id, truckB.id, rob.id])

echo "\nInitial state:"
world.show(root)

echo "\nTruck A moves 2 units:"
for unit in world.component(truckA, Unit):
  unit.move(2)

world.show(root)

echo "\nShip moves 4 units:"
for unit in world.component(ship, Unit):
  unit.move(4)

world.show(root)

echo "\nRob moves 5 units:"
for unit in world.component(rob, Unit):
  unit.move(5)

world.show(root)
