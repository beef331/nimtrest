import std/[typetraits, macros, genasts, strformat, enumerate]

type Component = object of RootObj

proc onlyUniqueValues(t: typedesc[tuple]): bool =
  result = true
  for nameA, fieldA in default(t).fieldPairs:
    for nameB, fieldB in default(t).fieldPairs:
      when nameA != nameB and fieldA is typeof(fieldB):
        return false

proc fromComponent(t: typedesc[tuple]): bool =
  result = true
  for x in default(t).fields:
    if x isnot Component:
      return false

type
  ArchetypeBase = ref object of RootObj
    typeCount: int
    types: seq[pointer]
    componentOffset: seq[int]
    stride: int
    data: seq[byte]
  ComponentTuple = concept ct, type CT
    onlyUniqueValues(CT)
    fromComponent(CT)

  Archetype[T: ComponentTuple] = ref object of ArchetypeBase

proc makeArchetype(tup: typedesc[ComponentTuple]): Archetype[tup] =
  const tupLen = tupleLen(tup)
  result = Archetype[tup](
    typeCount: tupLen,
    types: newSeqOfCap[pointer](tupLen),
    componentOffset: newSeqOfCap[int](tupLen)
  )
  let def = default(tup)
  var offset = 0
  for field in def.fields:
    result.types.add field.getTypeInfo()
    result.componentOffset.add offset
    offset.inc sizeof(field) - sizeof(pointer)
  result.stride = offset

proc len*(arch: ArchetypeBase): int = arch.data.len div arch.stride

proc `$`[T: ComponentTuple](arch: Archetype[T]): string =
  const
    typStr = $T
    tupLen = tupleLen(T)
  result = fmt"Archetype[{typStr}]("
  let len = arch.len
  for i in 0..<len:
    let def = default T
    var ind = 0
    for field in def.fields:
      let
        startField = arch.data[i * arch.stride + arch.componentOffset[ind]].addr
        data = cast[ptr typeof(field)](cast[int](startField) - sizeof(pointer))
      result.add $data[]
      if ind < tupLen - 1:
        result.add ", "
      inc ind
    if i < len - 1:
      result.add ", "
  result.add ")"

proc filter(archetypes: openarray[ArchetypeBase], tup: typedesc[ComponentTuple]): seq[ArchetypeBase] =
  var def: tup
  const requiredCount = tupleLen(tup)
  for arch in archetypes:
    if arch.typeCount >= requiredCount:
      var found = 0
      for field in def.fields:
        for i in 0..<arch.typeCount:
          if arch.types[i]  == field.getTypeInfo:
            inc found

      if found == requiredCount:
        result.add arch

proc add*[T](arch: Archetype[T], tup: sink T) =
  for field in tup.fields:
    const realSize = sizeof(field) - sizeof(pointer) # Dont need type information
    let startLen = arch.data.len
    arch.data.setLen(arch.data.len + realSize)
    let fieldStart = cast[pointer](cast[int](field.addr) + sizeof(pointer))
    arch.data[startLen].addr.copyMem(fieldStart, realSize)

macro generateAccess(arch: ArchetypeBase, ind: int, indArray: array, tup: ComponentTuple): untyped =
  result = nnkTupleConstr.newTree()
  for i, val in tup.getTypeImpl:
    result.add:
      genast(val, ind, indArray, tup, i):
        let element = arch.data[ind * arch.stride + arch.componentOffset[indArray[i]]].addr
        cast[ptr val](cast[int](element) - sizeof(pointer))

iterator foreach*(arch: ArchetypeBase, tup: typedesc[ComponentTuple]): auto =
  var
    indices: array[tupleLen(tup), int]
    found = 0
    def: tup
  for field in def.fields:
    if found >= indices.len:
      break
    for i in 0..<arch.typeCount:
      if arch.types[i] == field.getTypeInfo():
        indices[found] = i
        inc found
        if found >= indices.len:
          break

  assert found == tupleLen(tup)
  for i in 0 ..< arch.len:
    yield arch.generateAccess(i, indices, def)

iterator foreach*(archs: openarray[ArchetypeBase], tup: typedesc[ComponentTuple], filtered: static bool = true): auto =
  when filtered:
    for arch in archs.filter(tup):
      for ent in arch.foreach(tup):
        yield ent
  else:
    for arch in archs:
      for ent in arch.foreach(tup):
        yield ent

when isMainModule:
  type
    Position = object of Component
      x, y, z: float32
    Health = object of Component
      current, max: int32

  var
    posHealth = makeArchetype((Position, Health))
    pos = makeArchetype((Position,))
    health = makeArchetype((Health,))

  pos.add (Position(x: 100, y: 10, z: 10), )
  posHealth.add (Position(x: 1, y: 10, z: 40), Health())

  var myArchs = [ArchetypeBase pos, posHealth, health]

  for (pos,) in myArchs.foreach (Position,):
    pos.x = 300
    echo pos[]

  for (health, pos) in myArchs.foreach (Health, Position):
    pos.x = 300
    health.current = 100
    health.max = 130

  for (health, pos) in posHealth.foreach (Health, Position):
    assert pos[] == Position(x: 300, y: 10, z: 40)
    assert health[] == Health(current: 100, max: 130)

  echo $pos
  echo posHealth
  echo health
