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

proc makeSoldier(name: string, position: int): Unit =
  Unit(kind: Soldier, position: position, name: name)

proc makeTruck(name: string, position: int, children: seq[Id]): Unit =
  Unit(kind: Truck, position: position, name: name, children: children)

proc makeShip(name: string, position: int, children: seq[Id]): Unit =
  Unit(kind: Ship, position: position, name: name, children: children)

proc makeRoot(children: seq[Id]): Unit =
  Unit(kind: Root, position: 0, name: "root", children: children)

proc move(unit: var Unit, delta: int) =
  unit.position += delta

var world = World()

proc show(world: var World, unit: Unit, depth: int = 0) =
  echo "  ".repeat(depth), unit.name, ": ", unit.position
  for child in unit.children:
    let childEntity = world.getEntity(child)
    for childUnit in world.component(childEntity, Unit):
      world.show(childUnit, depth + 1)

let abe = world.addEntity (Id(), makeSoldier("abe", 0))
let ben = world.addEntity (Id(), makeSoldier("ben", 1))
let bob = world.addEntity (Id(), makeSoldier("bob", 1))
let dom = world.addEntity (Id(), makeSoldier("dom", 2))
let ray = world.addEntity (Id(), makeSoldier("ray", 2))
let rob = world.addEntity (Id(), makeSoldier("rob", 3))

let truckA = world.addEntity (Id(), makeTruck("truck A", 0, @[abe.id, ben.id]))
let truckB = world.addEntity (Id(), makeTruck("truck B", 5, @[bob.id, dom.id]))

let ship = world.addEntity (Id(), makeShip("ship", 0, @[ray.id, truckA.id]))

let root = world.addEntity (Id(), makeRoot(@[ship.id, truckB.id, rob.id]))

echo $world

for rootUnit in world.component(root, Unit):
  world.show(rootUnit)

echo "Id type info:   ", Id.getTheTypeInfo().int
echo "Unit type info: ", Unit.getTheTypeInfo().int
